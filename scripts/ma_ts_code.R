source(paste0(getwd(),"/scripts/ma_ts_func.R"))

states <- read_delim("https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil-states.csv",
                     delim = ",", col_names = T)
ma<-states%>%filter(state=="MA")
ma<-insertFC(ma)
if(ma$totalCases[ma$date==Sys.Date()]==ma$totalCases[ma$date==Sys.Date()-1] 
   || ma$totalCases[ma$date==Sys.Date()]==0){
  ma<-ma[-nrow(ma),]
}
round(ma$fc[ma$date==Sys.Date()],digits = 2)
previsaoPlot(ma$date,ma$totalCases, 5, "casos")
previsaoPlot(ma$date,ma$deaths, 5, "óbitos")

ma<-read_delim("https://raw.githubusercontent.com/lucaseosilva/covid19-ma/master/SESMA_casos_agregados.csv",
               delim = ",", col_names = T)
ma$data<-as.Date(ma$data, "%d/%m/%Y")
ma$newCases<-NA
ma$newCases[1]<-ma$casosAcumulados[1]
for(i in 2:nrow(ma)){
  ma$newCases[i]<-ma$casosAcumulados[i]-ma$casosAcumulados[i-1]
}
names(ma)<-c("date","totalCases","deaths","newCases")
ma<-insertFC(ma)
round(ma$fc[ma$date==Sys.Date()], digits = 2)
previsaoPlot(ma$date,ma$totalCases, 5, "casos")
previsaoPlot(ma$date,ma$deaths, 5, "óbitos")

cities<-read_delim("https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil-cities-time.csv",
                   delim = ",", col_names = T)
slz<-cities%>%filter(city=="São Luís/MA")
slz<-insertFC(slz)
if(slz$totalCases[slz$date==Sys.Date()]==slz$totalCases[slz$date==Sys.Date()-1] 
   || slz$totalCases[slz$date==Sys.Date()]==0){
  slz<-slz[-nrow(slz),]
}
round(slz$fc[slz$date==Sys.Date()-1],digits = 2)
previsaoPlot(slz$date,slz$totalCases, 5, "casos")
previsaoPlot(slz$date,slz$deaths, 5, "óbitos")

interior<-cities%>%filter(state=="MA"& !city %in% c("São Luís/MA","São José de Ribamar/MA",
                                                 "Paço do Lumiar/MA", "Raposa/MA"))%>%group_by(date)%>%summarise(totalCases=sum(totalCases),
                                                                                          deaths=sum(deaths),
                                                                                          newCases=sum(newCases))
interior<-insertFC(interior)
if(interior$totalCases[interior$date==Sys.Date()]==interior$totalCases[interior$date==Sys.Date()-1] 
   || interior$totalCases[interior$date==Sys.Date()]==0){
  interior<-interior[-nrow(interior),]
}
round(interior$fc[interior$date==Sys.Date()-1], digits = 2)
previsaoPlot(interior$date,interior$totalCases, 5, "casos")
previsaoPlot(interior$date,interior$deaths, 5, "óbitos")

ilha<-cities%>%
        filter(state=="MA"& (city=="São Luís/MA"| city=="São José de Ribamar/MA" 
                             | city=="Paço do Lumiar/MA"| city=="Raposa/MA"))%>%
        group_by(date)%>%
        summarise(totalCases=sum(totalCases), deaths=sum(deaths),newCases=sum(newCases))
ilha<-insertFC(ilha)
if(ilha$totalCases[ilha$date==Sys.Date()]==ilha$totalCases[ilha$date==Sys.Date()-1] 
   || ilha$totalCases[ilha$date==Sys.Date()]==0){
  ilha<-ilha[-nrow(ilha),]
}
round(ilha$fc[ilha$date==Sys.Date()-1],digits = 2)
previsaoPlot(ilha$date,ilha$totalCases, 5, "casos")
previsaoPlot(ilha$date,ilha$deaths, 5, "óbitos")
