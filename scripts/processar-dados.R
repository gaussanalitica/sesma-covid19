library(rdrop2)
library(dplyr)
library(DT)
library(devtools)
library(forecast)
library(gglabeller)
library(ggmap)
library(ggplot2)
library(ggpubr)
library(jsonlite)
library(ggrepel)
library(grid)
library(gridExtra)
library(httr)
library(leaflet)
library(lubridate)
library(magrittr)
library(data.table)
library(openxlsx)
library(plotly)
library(R0)
library(raster)
library(readr)
library(readxl)
library(rgdal)
library(rlist)
library(scales)
library(sf)
library(stringr)
library(stringi)
library(tidyquant)
library(tidyr)
library(tidyverse)
library(tmap)
library(writexl)
library(R.utils)
library(shinymanager)
library(EpiEstim)

setwd("~/dados-covid-sesma")

token <- readRDS("droptoken.rds")
drop_auth(rdstoken = "droptoken.rds")

brazil <- read_delim("https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil-total.csv",delim = ",", col_names = T)
#brazil<-read_rds("brazil.Rds")
brazil$incidence<-(brazil$totalCases_per_100k_inhabitants*1000000)/100000
brazil$letality<-(brazil$deathsMS/brazil$totalCasesMS)*100
brazil$mortality<-(brazil$deaths_per_100k_inhabitants*1000000)/100000
brazil$tests1M<-(brazil$tests_per_100k_inhabitants*1000000)/100000
idh_ufs<-read_delim("https://raw.githubusercontent.com/lucaseosilva/covid19-ma/master/ATLAS_idh_uf_2010.csv", col_names = T, delim =",")
idh_ufs<-idh_ufs%>%dplyr::select(sigla,idh2010)
brazil<-full_join(brazil,idh_ufs, by=c("state"="sigla"))
brazil %<>% mutate(ma_logic = ifelse(state=="MA", "1", "0")) 

#states <- read_delim("https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil-states.csv", delim = ",", col_names = T)
#states<-read_rds("states.Rds")
#states$incidence<-(states$totalCases_per_100k_inhabitants*1000000)/100000
#states$letality<-(states$deathsMS/states$totalCasesMS)*100
#states$mortality<-(states$deaths_per_100k_inhabitants*1000000)/100000
#states$tests1M<-(states$tests_per_100k_inhabitants*1000000)/100000
#ma<-states%>%filter(state=="MA")

api <- "https://api.brasil.io/v1/dataset/covid19/caso_full/data"
info_uf <- "?state=MA&place_type=state&is_repeated=False"
repos <- httr::GET(paste0(api,info_uf), 
                   add_headers(Authorization = "Token 751c315e84d471ae53117e08c40a337eb6b5f462"))
my_content <- content(repos, as="text")
my_content_json <- fromJSON(my_content)

#ma<-read_delim("https://brasil.io/dataset/covid19/caso_full/?state=MA&place_type=state&is_repeated=False&format=csv", delim = ",", col_names = T)
ma <- my_content_json$results
names(ma)<-str_replace(names(ma),"last_available_","")
ma <- ma[,-11]
ma$date <- as.Date(ma$date)
ma<-ma%>%arrange(date)
ma$deaths_per_100k_inhabitants<-(ma$deaths*100000)/ma$estimated_population
ma<-ma%>%mutate(cases07d=zoo::rollmean(new_confirmed, k = 7, fill = NA, align="right"),
                deaths07d=zoo::rollmean(new_deaths, k = 7, fill = NA, align="right"))

#cities <- read_delim("https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil-cities-time.csv", delim = ",",col_names = T)
#cities<-read_rds("cities.Rds")
#ma_cidades<-cities%>%filter(state=="MA")

#info_city <- "?state=MA&place_type=city&is_repeated=False"
#repos <- httr::GET(paste0(api,info_city), 
#                   add_headers(Authorization = "Token 751c315e84d471ae53117e08c40a337eb6b5f462"))
#my_content <- content(repos, as="text")
#my_content_json <- fromJSON(my_content)
#ma_cidades<-read_delim("https://brasil.io/dataset/covid19/caso_full/?state=MA&place_type=city&is_repeated=False&format=csv",delim = ",",col_names = T)
# ma_cidades <- my_content_json$results
# names(ma_cidades)<-str_replace(names(ma_cidades),"last_available_","")
# ma_cidades <- ma_cidades[,-11]
# ma_cidades$date <- as.Date(ma_cidades$date)

# gunzip(download.file("",tempfile()), "rt")
# 
# temp <- tempfile()
# download.file("https://data.brasil.io/dataset/covid19/caso_full.csv.gz", temp)
# gzfile(temp, 'rt')
# data <- fread(temp)

temp_nc <- fs::file_temp(ext = ".gz")
url <- "https://data.brasil.io/dataset/covid19/caso_full.csv.gz"
download.file(url, destfile = temp_nc, mode = "wb")
nc_path <- gunzip(temp_nc)
dados_gerais <- fread(nc_path)
ma_cidades<-dados_gerais%>%filter(state=="MA" & place_type=="city")
#removendo last_date
ma_cidades<-ma_cidades[, -11]
names(ma_cidades)<-str_replace(names(ma_cidades),"last_available_","")
ma_cidades$date<-as.Date(ma_cidades$date,"%Y-%m-%d")

#ma_cidades<-ma_cidades%>%arrange(date)%>%dplyr::select(city_ibge_code, city, date, estimated_population,
#                                                       confirmed,confirmed_per_100k_inhabitants, deaths,
#                                                       death_rate, new_confirmed, new_deaths)
ma_cidades$cod_ibge6<-as.numeric(str_sub(as.character(ma_cidades$city_ibge_code),1,6))

info_mun_ma<-read_delim("https://raw.githubusercontent.com/lucaseosilva/covid19-ma/master/DATASUS_pop_ma.csv", col_names = T, delim =",")

ma_cidades<-full_join(ma_cidades,info_mun_ma%>%dplyr::select(codmun,codRegiaoSaude,nomeRegiaoSaude),
                      by=c("cod_ibge6"="codmun"))

# última data de análise
data_referencia_uf<-ma$date[nrow(ma)]
data_referencia_mun<-ma_cidades$date[nrow(ma_cidades)]

# copiando informações do último dia para o estado e municípios
ma_dia<-ma[ma$date==data_referencia_uf,]
ma_mun_td <- ma_cidades %>% 
  filter(date==data_referencia_mun)
ma_mun_td<-ma_mun_td%>%mutate(mortality=(deaths*100000)/estimated_population)

r_maranho<-estimate_R(ma$new_confirmed, 
                      method="parametric_si", 
                      config = make_config(list(mean_si = 4.8, std_si = 2.3)))

# calculando o r para os municípios
cod_mun<-unique(ma_cidades$city_ibge_code)
nome_mun<-unique(ma_cidades$city)
ma_mun_r<-data.frame()
for(i in 1:length(cod_mun)){
  mun<-ma_cidades%>%filter(city_ibge_code==cod_mun[i])
  tryCatch({
    results <- estimate_R(abs(mun$new_confirmed), 
                          method="parametric_si",
                          config = make_config(list(mean_si = 4.8, std_si = 2.3)))
    mun_l<-data.frame(city_ibge_code=cod_mun[i],
                      city=nome_mun[i],
                      data=mun$date[results$R$t_start],
                      r=round(results$R$`Mean(R)`, digits = 3),
                      row.names = NULL, stringsAsFactors = F)
    ma_mun_r<-rbind(ma_mun_r,mun_l)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}
# o espelho do dia para os municípios
ma_mun_td <- left_join(ma_mun_td, ma_mun_r%>%
                         filter(data==tail(ma_mun_r$data,1))%>%
                         dplyr::select(city_ibge_code, r),
                       by="city_ibge_code")

ma_reg_se<-ma_cidades%>%
  group_by(codRegiaoSaude, nomeRegiaoSaude, date)%>%
  summarise(casosAcumulados=sum(confirmed), obitosAcumulados=sum(deaths),
            casosNovos=sum(new_confirmed), obitosNovos=sum(new_deaths))

# calculando o r para as regiões
cod_reg<-unique(ma_reg_se$codRegiaoSaude)
nome_reg<-unique(ma_reg_se$nomeRegiaoSaude)
ma_reg_r<-data.frame()
for(i in 1:length(cod_reg)){
  reg<-ma_reg_se%>%filter(codRegiaoSaude==cod_reg[i])
  tryCatch({
    results <- estimate_R(abs(reg$casosNovos), 
                          method="parametric_si",
                          config = make_config(list(mean_si = 4.8, std_si = 2.3)))
    reg_l<-data.frame(region_ibge_code=cod_reg[i],
                      region=nome_reg[i],
                      data=reg$date[results$R$t_start],
                      r=round(results$R$`Mean(R)`, digits = 3),
                      row.names = NULL, stringsAsFactors = F)
    ma_reg_r<-rbind(ma_reg_r,reg_l)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}
# alguns merges populacionais necessários
popRegMA<-ma_cidades%>%
  filter(date==data_referencia_mun)%>%
  group_by(codRegiaoSaude, nomeRegiaoSaude)%>%
  summarise(populacao=sum(estimated_population))

ma_reg_se<-merge(ma_reg_se,popRegMA%>%dplyr::select(codRegiaoSaude,populacao), by="codRegiaoSaude")
ma_reg_se$incidence<-(ma_reg_se$casosAcumulados/ma_reg_se$populacao)*100000
ma_reg_se$mortality<-(ma_reg_se$obitosAcumulados/ma_reg_se$populacao)*100000    
ma_reg_se$letality<-ma_reg_se$obitosAcumulados/ma_reg_se$casosAcumulados*100

# o espelho do dia para as regiões
ma_reg_td<-ma_reg_se%>%
  filter(date==data_referencia_mun)
ma_reg_td<-merge(ma_reg_td, ma_mun_td%>%group_by(codRegiaoSaude)%>%summarise(rMedio=mean(r,na.rm=T)))

# análise dos mapas
if (!file.exists("MA-MUN.zip")) {
  link_shape<-"http://www.usp.br/nereus/wp-content/uploads/MA-MUN.zip"
  download.file(link_shape, destfile="MA-MUN.zip")
}
unzip("MA-MUN.zip")

ma_shape <- st_read(paste0(getwd(),"/21MUE250GC_SIR.shp"), stringsAsFactors = F)
ma_shape$CD_GEOCODM<-as.double(ma_shape$CD_GEOCODM)

mapa_dado_ma <- full_join(ma_shape, ma_mun_td, by=c("CD_GEOCODM"="city_ibge_code"))

# Análise regional
if (!file.exists("regioes_ma.zip")) {
  link_shape2<-"https://github.com/lucaseosilva/covid19-ma/raw/master/regioes_ma.zip"
  download.file(link_shape2, destfile="regioes_ma.zip")
}
unzip("regioes_ma.zip")
ma_rs_shape <- st_read(paste0(getwd(),"/regioes_saude-polygon.shp"), stringsAsFactors = F)
ma_rs_shape$Primary.ID<-as.double(ma_rs_shape$Primary.ID)

# ma_reg_se<-ma_cidades%>%
#     group_by(codRegiaoSaude, nomeRegiaoSaude, date)%>%
#     summarise(casosAcumulados=sum(confirmed), obitosAcumulados=sum(deaths),
#               casosNovos=sum(new_confirmed), obitosNovos=sum(new_deaths))


mapa_dado_ma_reg <- full_join(ma_rs_shape, ma_reg_td, by=c("Primary.ID"="codRegiaoSaude"))

comparativo<-brazil%>%
  filter(!state=="TOTAL")%>%
  dplyr::select(state,totalCases, deaths, tests, idh2010, incidence, letality, mortality)%>%
  mutate(totalCases=log(totalCases),deaths=log(deaths), tests=log(tests))%>%
  rename(`Estado`=state,`Casos`=totalCases, `Óbitos`=deaths, `Testes`=tests, `IDH`=idh2010,
         `Incidência`=incidence, `Letalidade`=letality, `Mortalidade`=mortality)

save.image(file = "dados-covid-sesma.RData")
drop_upload('dados-covid-sesma.RData', path = "covid19-sesma")
