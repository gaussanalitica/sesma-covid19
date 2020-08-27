library(tidyverse)
library(httr)
library(jsonlite)
library(stringr)

casosMunicipiosAgregados=function(){
  api <- "https://webapi.saude.ma.gov.br/painel-covid19-webapi"
  informacao <- "/api/casos/v1/obter-casos-municipios"
  repos <- GET(paste0(api,informacao))
  my_content <- content(repos, as="text")
  my_content_json <- fromJSON(my_content)
  
  return (my_content_json)
}

casosBairrosAgregados=function(){
  api <- "https://webapi.saude.ma.gov.br/painel-covid19-webapi"
  informacao <- "/api/casos/v1/obter-casos-bairros"
  repos <- GET(paste0(api,informacao))
  my_content <- content(repos, as="text")
  my_content_json <- fromJSON(my_content)
  
  return (my_content_json)
}

casosDiariosDetalhados=function(){
  api <- "https://webapi.saude.ma.gov.br/painel-covid19-webapi"
  informacao <- "/api/casos/v1/obter-casos-registro-detalhe-diario"
  repos <- GET(paste0(api,informacao))
  my_content <- content(repos, as="text")
  my_content_json <- fromJSON(my_content)
  df<-my_content_json$lista_casos
  
  df$data_registro<-as.Date(df$data_registro, "%d/%m/%Y")
  df$data_recuperacao<-as.Date(df$data_recuperacao, "%d/%m/%Y")
  df$data_obito<-as.Date(df$data_recuperacao, "%d/%m/%Y")
  df$situacao_paciente<-str_to_upper(df$situacao_paciente)
  df$tem_comorbidade<-as.logical(df$tem_comorbidade)
  df$sexo[df$sexo == "M"] <- "MASCULINO"
  df$sexo[df$sexo == "F"] <- "FEMININO"
  
  df$faixa_etaria <- with(df, cut(x = idade, breaks = c (0,10,20,30,40,50,60, max(idade, na.rm = T)), 
                                        labels=c('0-10','10-20','20-30', '30-40','40-50', '50-60', '>60'),
                                        ordered_result = TRUE, right = TRUE))
  
  return (df)
}


casosEstadoDia=function(){
  api <- "https://webapi.saude.ma.gov.br/painel-covid19-webapi"
  informacao <- "/api/casos/v1/obter-casos-registro-por-dia"
  repos <- GET(paste0(api,informacao))
  my_content <- content(repos, as="text")
  my_content_json <- fromJSON(my_content)
  
  return (my_content_json)
}







