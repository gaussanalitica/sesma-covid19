states <- read_delim("https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil-states.csv",
                     delim = ",", col_names = T)
ma<-states%>%filter(state=="MA")

ilha<-cities%>%
  filter(state=="MA"& (city=="São Luís/MA"| city=="São José de Ribamar/MA" 
                       | city=="Paço do Lumiar/MA"| city=="Raposa/MA"))%>%
  group_by(date)%>%
  summarise(totalCases=sum(totalCases), deaths=sum(deaths))

interior<-cities%>%filter(state=="MA"& !city %in% c("São Luís/MA","São José de Ribamar/MA",
                                                    "Paço do Lumiar/MA", "Raposa/MA"))%>%
                  group_by(date)%>%
                  summarise(totalCases=sum(totalCases),
                            deaths=sum(deaths))
                                                                                                                    
ggplot()+
  geom_line(data=ma, aes(x=date, y=deaths, colour="Maranhão"), size=1.5)+
  geom_line(data=ilha, aes(x=date, y=deaths, colour="Grande Ilha"), size=1.5)+
  geom_line(data=interior, aes(x=date, y=deaths, colour="Interior"), size=1.5)+
  scale_y_continuous(labels = function(x) format(x, big.mark=".", scientific = F))+
  labs(title="Comparativo regional de óbitos pela COVID-19 no Maranhão",
       #subtitle = "São Luís",
       x="", 
       y = "")+
  guides(color=guide_legend(title=""))+
  theme_classic()+
  theme(plot.title = element_text(face = "bold", size = 22, hjust = 0.5),
        plot.subtitle = element_text(face = "italic", size = 16, hjust = 0.5),
        plot.caption = element_text(face = "italic", size = 15, hjust = 0.5),
        axis.title = element_text(size = 17),
        axis.text = element_text(size = 22),
        legend.text=element_text(size=16),
        legend.title = element_text(size=14),
        legend.position = "right")
