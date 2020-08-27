library(readxl)
library(dplyr)
library(ggplot2)
source(paste0(getwd(),"/scripts/ma_ts_func.R"))

brasil<-read_excel("dados/HIST_PAINEL_COVIDBR_20200516.xlsx",sheet = 1)
brasil$coduf<-as.numeric(brasil$coduf)
brasil$codmun<-as.numeric(brasil$codmun)
brasil$codRegiaoSaude<-as.numeric(brasil$codRegiaoSaude)
brasil$data<-as.Date(brasil$data)
brasil$populacaoTCU2019<-as.numeric(brasil$populacaoTCU2019)
brasil$casosAcumulado<-as.numeric(brasil$casosAcumulado)
brasil$obitosAcumulado<- as.numeric(brasil$obitosAcumulado)

maranhao<-brasil%>%filter(estado=="MA" & !is.na(municipio))

regioes_saude_ma<-maranhao%>%group_by(codRegiaoSaude, nomeRegiaoSaude, data)%>%summarise(casos=sum(casosAcumulado),
                                                                                         obitos=sum(obitosAcumulado))
regioes<-unique(regioes_saude_ma$codRegiaoSaude)
  
for(i in 1:length(regioes)){
    regiao<-regioes_saude_ma%>%filter(codRegiaoSaude==regioes[i])
    ggsave(previsaoPlot(regiao$data, regiao$casos, 5, "casos", regiao$nomeRegiaoSaude), 
           file=paste0(getwd(),'/graficos/',regiao$codRegiaoSaude[i],"_",regiao$nomeRegiaoSaude[i],"_","casos", ".png"),
           width = 35, height = 15, units = "cm")
    ggsave(previsaoPlot(regiao$data, regiao$obitos, 5, "óbitos", regiao$nomeRegiaoSaude), 
           file=paste0(getwd(),'/graficos/',regiao$codRegiaoSaude[i],"_",regiao$nomeRegiaoSaude[i],"_","óbitos", ".png"),
           width = 35, height = 15, units = "cm")
}
