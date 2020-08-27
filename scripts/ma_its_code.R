source(paste0(getwd(),"/scripts/ma_its_func.R"))

lockdown.date<-as.Date("2020-05-05")

states <- read_delim("https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil-states.csv",
                     delim = ",", col_names = T)
ma<-states%>%filter(state=="MA")
if(ma$totalCases[ma$date==Sys.Date()]==ma$totalCases[ma$date==Sys.Date()-1] 
   || ma$totalCases[ma$date==Sys.Date()]==0){
  ma<-ma[-nrow(ma),]
}
ma<-settingData(ma,lockdown.date)
plotGraphic(ma,ma$totalCases,"casos","Maranhão", lockdown.date)
plotGraphic(ma,ma$deaths,"óbitos","Maranhão", lockdown.date)

cities<-read_delim("https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil-cities-time.csv",
                   delim = ",", col_names = T)
slz<-cities%>%filter(city=="São Luís/MA")
if(slz$totalCases[slz$date==Sys.Date()]==slz$totalCases[slz$date==Sys.Date()-1] 
   || slz$totalCases[slz$date==Sys.Date()]==0){
  slz<-slz[-nrow(slz),]
}
slz<-settingData(slz,lockdown.date)
plotGraphic(slz,slz$totalCases,"casos","São Luís", lockdown.date)
plotGraphic(slz,slz$deaths,"óbitos","São Luís", lockdown.date)

interior<-cities%>%filter(state=="MA"& !city %in% c("São Luís/MA","São José de Ribamar/MA",
                                                    "Paço do Lumiar/MA", "Raposa/MA"))%>%group_by(date)%>%summarise(totalCases=sum(totalCases),
                                                                                                                    deaths=sum(deaths))
if(interior$totalCases[interior$date==Sys.Date()]==interior$totalCases[interior$date==Sys.Date()-1] 
   || interior$totalCases[interior$date==Sys.Date()]==0){
  interior<-interior[-nrow(interior),]
}
interior<-settingData(interior,lockdown.date)
plotGraphic(interior,interior$totalCases,"casos","Interior do estado", lockdown.date)
plotGraphic(interior,interior$deaths,"óbitos","Interior do estado", lockdown.date)

ilha<-cities%>%
  filter(state=="MA"& (city=="São Luís/MA"| city=="São José de Ribamar/MA" 
                       | city=="Paço do Lumiar/MA"| city=="Raposa/MA"))%>%
  group_by(date)%>%
  summarise(totalCases=sum(totalCases), deaths=sum(deaths))
if(ilha$totalCases[ilha$date==Sys.Date()]==ilha$totalCases[ilha$date==Sys.Date()-1] 
   || ilha$totalCases[ilha$date==Sys.Date()]==0){
  ilha<-ilha[-nrow(ilha),]
}
ilha<-settingData(ilha,lockdown.date)
plotGraphic(ilha,ilha$totalCases,"casos","Grande Ilha", lockdown.date)
plotGraphic(ilha,ilha$deaths,"óbitos","Grande Ilha", lockdown.date)
