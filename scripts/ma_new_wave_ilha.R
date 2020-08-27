cities<-read_delim("https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil-cities-time.csv",
                   delim = ",", col_names = T)

## ANÁLISES PARA A ILHA
ilha<-cities%>%
  filter(state=="MA"& (city=="São Luís/MA"| city=="São José de Ribamar/MA" 
                       | city=="Paço do Lumiar/MA"| city=="Raposa/MA"))%>%
  group_by(date)%>%
  summarise(totalCases=sum(totalCases), deaths=sum(deaths),newCases=sum(newCases), newDeaths=sum(newDeaths))

ilha<-insertFC(ilha)
ilha<-ilha[1:78,]
ilha$td<-1:nrow(ilha)
  
previsaoPlot(ilha$date,ilha$totalCases, 5, "casos")
previsaoPlot(ilha$date,ilha$deaths, 5, "óbitos")

lockdown.inicio<-as.Date("2020-05-05")
lockdown.final<-as.Date("2020-05-17")

model<-lm(ilha$newCases ~ poly(ilha$td,5))%>%summary()

ggplot(ilha, aes(x=date))+
  geom_line(aes(y=newCases), size = 2)+
  geom_ma(aes(y=newCases), ma_fun = SMA, size=1.5, colour="blue", linetype=1)+
  geom_vline(aes(xintercept = lockdown.inicio), color="red", size=1.5)+
  geom_vline(aes(xintercept = lockdown.final), color="red", size=1.5)+
  annotate("text", x=as.Date("2020-05-11"), y=400, label= "Lockdown", color="red", size=10, fontface = 3)+
  #annotate("text", x=as.Date("2020-03-27"), y=400, label= paste0("R2: ",sprintf("%.3f",model$r.squared)), color="black", size=10, fontface = 3)+
  scale_y_continuous(labels = function(x) format(x, big.mark="."))+
  scale_x_date(breaks = c(as.Date("2020-03-20"), as.Date("2020-03-27"), as.Date("2020-04-03"),
                          as.Date("2020-04-10"), as.Date("2020-04-17"), as.Date("2020-04-24"),
                          as.Date("2020-04-30"), as.Date("2020-05-05"), as.Date("2020-05-17"),
                          as.Date("2020-05-22"), as.Date("2020-05-29"), as.Date("2020-06-06")),
               date_labels = "%d/%b")+
  labs(title="Evolução do número de novos casos na Grande Ilha",
       subtitle = "Média móvel\n",
       x="", 
       y = "")+
  guides(color=guide_legend(title=""))+
  theme_classic()+
  theme(plot.title = element_text(face = "bold", size = 24, hjust = 0.5),
        plot.subtitle = element_text(face = "italic", size = 24, hjust = 0.5),
        plot.caption = element_text(face = "italic", size = 12, hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 24),
        legend.text=element_text(size=14),
        legend.title = element_text(size=14),
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))
  
ggplot(ilha, aes(x=date))+
  geom_line(aes(y=newCases), size = 2)+
  geom_smooth(aes(y=newCases), method = "lm", formula = "y~poly(x, 5)", se = F, size=1.5)+
  geom_vline(aes(xintercept = lockdown.inicio), color="red", size=1.5)+
  geom_vline(aes(xintercept = lockdown.final), color="red", size=1.5)+
  annotate("text", x=as.Date("2020-05-11"), y=400, label= "Lockdown", color="red", size=10, fontface = 3)+
  #annotate("text", x=as.Date("2020-03-27"), y=400, label= paste0("R2: ",sprintf("%.3f",model$r.squared)), color="black", size=10, fontface = 3)+
  scale_y_continuous(labels = function(x) format(x, big.mark="."))+
  scale_x_date(breaks = c(as.Date("2020-03-20"), as.Date("2020-03-27"), as.Date("2020-04-03"),
                          as.Date("2020-04-10"), as.Date("2020-04-17"), as.Date("2020-04-24"),
                          as.Date("2020-04-30"), as.Date("2020-05-05"), as.Date("2020-05-17"),
                          as.Date("2020-05-22"), as.Date("2020-05-29"), as.Date("2020-06-06")),
               date_labels = "%d/%b")+
  labs(title="Evolução do número de novos casos na Grande Ilha",
       subtitle = "Análise de tendência - Polinômio de 5a ordem\n",
       x="", 
       y = "")+
  guides(color=guide_legend(title=""))+
  theme_classic()+
  theme(plot.title = element_text(face = "bold", size = 24, hjust = 0.5),
        plot.subtitle = element_text(face = "italic", size = 24, hjust = 0.5),
        plot.caption = element_text(face = "italic", size = 12, hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 24),
        legend.text=element_text(size=14),
        legend.title = element_text(size=14),
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(ilha, aes(x=date))+
  geom_line(aes(y=newDeaths), size = 2)+
  geom_smooth(aes(y=newDeaths), method = "lm", formula = "y~poly(x, 5)", se = F, size=1.5)+
  geom_vline(aes(xintercept = lockdown.inicio), color="red", size=1.5)+
  geom_vline(aes(xintercept = lockdown.final), color="red", size=1.5)+
  annotate("text", x=as.Date("2020-05-11"), y=25, label= "Lockdown", color="red", size=10, fontface = 3)+
  scale_y_continuous(labels = function(x) format(x, big.mark="."))+
  scale_x_date(breaks = c(as.Date("2020-03-20"), as.Date("2020-03-27"), as.Date("2020-04-03"),
                          as.Date("2020-04-10"), as.Date("2020-04-17"), as.Date("2020-04-24"),
                          as.Date("2020-04-30"), as.Date("2020-05-05"), as.Date("2020-05-17"),
                          as.Date("2020-05-22"), as.Date("2020-05-29"), as.Date("2020-06-06")),
               date_labels = "%d/%b")+
  labs(title="Evolução do número de novos óbitos na Grande Ilha",
       subtitle = "Análise de tendência - Polinômio de 5a ordem\n",
       x="", 
       y = "")+
  guides(color=guide_legend(title=""))+
  theme_classic()+
  theme(plot.title = element_text(face = "bold", size = 24, hjust = 0.5),
        plot.subtitle = element_text(face = "italic", size = 24, hjust = 0.5),
        plot.caption = element_text(face = "italic", size = 12, hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 24),
        legend.text=element_text(size=14),
        legend.title = element_text(size=14),
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(ilha, aes(x=date))+
  geom_line(aes(y=newDeaths), size = 2)+
  geom_ma(aes(y=newDeaths), ma_fun = SMA, size=1.5, colour="blue", linetype=1)+
  geom_vline(aes(xintercept = lockdown.inicio), color="red", size=1.5)+
  geom_vline(aes(xintercept = lockdown.final), color="red", size=1.5)+
  annotate("text", x=as.Date("2020-05-11"), y=25, label= "Lockdown", color="red", size=10, fontface = 3)+
  scale_y_continuous(labels = function(x) format(x, big.mark="."))+
  scale_x_date(breaks = c(as.Date("2020-03-20"), as.Date("2020-03-27"), as.Date("2020-04-03"),
                          as.Date("2020-04-10"), as.Date("2020-04-17"), as.Date("2020-04-24"),
                          as.Date("2020-04-30"), as.Date("2020-05-05"), as.Date("2020-05-17"),
                          as.Date("2020-05-22"), as.Date("2020-05-29"), as.Date("2020-06-06")),
               date_labels = "%d/%b")+
  labs(title="Evolução do número de novos óbitos na Grande Ilha",
       subtitle = "Média móvel\n",
       x="", 
       y = "")+
  guides(color=guide_legend(title=""))+
  theme_classic()+
  theme(plot.title = element_text(face = "bold", size = 24, hjust = 0.5),
        plot.subtitle = element_text(face = "italic", size = 24, hjust = 0.5),
        plot.caption = element_text(face = "italic", size = 12, hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 24),
        legend.text=element_text(size=14),
        legend.title = element_text(size=14),
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))

newCasesIlha<-ilha$newCases
names(newCasesIlha)<-ilha$date
distribution <- generation.time("gamma", c(7.0,4.5))
res.R <- estimate.R(newCasesIlha, GT=distribution, methods=c("TD"), begin = names(newCasesIlha)[1], end=names(newCasesIlha)[length(newCasesIlha)-1])

table<-data.frame(r=res.R[["estimates"]][["TD"]][["R"]], conf.int=res.R[["estimates"]][["TD"]][["conf.int"]])
table<-tibble::rownames_to_column(table, "date")
table$date<-as.Date(table$date)
table<-table%>%filter(date>=as.Date("2020-04-01") & date<=as.Date("2020-06-01"))

table%>%filter(date<=lockdown.inicio-1)%>%summarise(mean(r))
table%>%filter(date>lockdown.final)%>%summarise(mean(r))

ggplot(table, aes(x=date, y=r))+
  geom_line(size=1)+
  geom_ribbon(aes(ymin = conf.int.lower, ymax = conf.int.upper), size = 1, colour = 'grey70', alpha = .3)+
  scale_y_continuous(breaks = c(0,1,2,3,4,5,6))+
  geom_hline(yintercept = 1)+
  geom_vline(aes(xintercept = lockdown.inicio), color="red", size=1.5)+
  geom_vline(aes(xintercept = lockdown.final), color="red", size=1.5)+
  annotate("text", x=as.Date("2020-05-11"), y=3, label= "Lockdown", color="red", size=10, fontface = 3)+
  scale_x_date(breaks = c(as.Date("2020-04-01"), as.Date("2020-04-15"), as.Date("2020-04-30"),
                          as.Date("2020-05-05"), as.Date("2020-05-17"),
                          as.Date("2020-06-01")),
               date_labels = "%d/%b")+
  labs(title="Evolução do número efetivo de reprodução (R) na Grande Ilha\n",
       x="", 
       y = "")+
  guides(color=guide_legend(title=""))+
  theme_classic()+
  theme(plot.title = element_text(face = "bold", size = 24, hjust = 0.5),
        plot.subtitle = element_text(face = "italic", size = 24, hjust = 0.5),
        plot.caption = element_text(face = "italic", size = 12, hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 24),
        legend.text=element_text(size=14),
        legend.title = element_text(size=14),
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))

casos_en<-ggplot(ilha, aes(x=date))+
  geom_line(aes(y=newCases), size = 2)+
  scale_y_continuous(labels = function(x) format(x, big.mark="."))+
  geom_vline(aes(xintercept = lockdown.inicio), color="red", size=1.5)+
  geom_vline(aes(xintercept = lockdown.final), color="red", size=1.5)+
  annotate("text", x=as.Date("2020-05-11"), y=500, label= "Lockdown", color="red", size=5, fontface = 3)+
  scale_x_date(breaks = c(as.Date("2020-03-20"), as.Date("2020-03-27"), as.Date("2020-04-03"),
                          as.Date("2020-04-10"), as.Date("2020-04-17"), as.Date("2020-04-24"),
                           as.Date("2020-05-05"), as.Date("2020-05-17"),
                           as.Date("2020-05-29"), as.Date("2020-06-06")),
               date_labels = "%d/%b")+
  labs(title="",
       subtitle = "Escala natural\n",
       x="", 
       y = "")+
  guides(color=guide_legend(title=""))+
  theme_classic()+
  theme(plot.title = element_text(face = "bold", size = 24, hjust = 0.5),
        plot.subtitle = element_text(face = "italic", size = 24, hjust = 0.5),
        plot.caption = element_text(face = "italic", size = 12, hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 24),
        legend.text=element_text(size=14),
        legend.title = element_text(size=14),
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))

casos_log<-ggplot(ilha, aes(x=date))+
  geom_line(aes(y=log(newCases)), size = 2)+
  scale_y_continuous(labels = function(x) format(x, big.mark="."))+
  geom_vline(aes(xintercept = lockdown.inicio), color="red", size=1.5)+
  geom_vline(aes(xintercept = lockdown.final), color="red", size=1.5)+
  annotate("text", x=as.Date("2020-05-11"), y=7, label= "Lockdown", color="red", size=5, fontface = 3)+
  scale_x_date(breaks = c(as.Date("2020-03-20"), as.Date("2020-03-27"), as.Date("2020-04-03"),
                          as.Date("2020-04-10"), as.Date("2020-04-17"), as.Date("2020-04-24"),
                           as.Date("2020-05-05"), as.Date("2020-05-17"),
                           as.Date("2020-05-29"), as.Date("2020-06-06")),
               date_labels = "%d/%b")+
  labs(title="",
       subtitle = "Log\n",
       x="", 
       y = "")+
  guides(color=guide_legend(title=""))+
  theme_classic()+
  theme(plot.title = element_text(face = "bold", size = 24, hjust = 0.5),
        plot.subtitle = element_text(face = "italic", size = 24, hjust = 0.5),
        plot.caption = element_text(face = "italic", size = 12, hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 24),
        legend.text=element_text(size=14),
        legend.title = element_text(size=14),
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))
annotate_figure(ggarrange(casos_en,casos_log),
                top=text_grob("Evolução do número de novos casos na Grande Ilha",
                              color = "black", face = "bold", size = 24)
)

obitos_en<-ggplot(ilha, aes(x=date))+
  geom_line(aes(y=newDeaths), size = 2)+
  scale_y_continuous(labels = function(x) format(x, big.mark="."))+
  geom_vline(aes(xintercept = lockdown.inicio), color="red", size=1.5)+
  geom_vline(aes(xintercept = lockdown.final), color="red", size=1.5)+
  annotate("text", x=as.Date("2020-05-11"), y=25, label= "Lockdown", color="red", size=5, fontface = 3)+
  scale_x_date(breaks = c(as.Date("2020-03-20"), as.Date("2020-03-27"), as.Date("2020-04-03"),
                          as.Date("2020-04-10"), as.Date("2020-04-17"), as.Date("2020-04-24"),
                          as.Date("2020-05-05"), as.Date("2020-05-17"),
                          as.Date("2020-05-29"), as.Date("2020-06-06")),
               date_labels = "%d/%b")+
  labs(title="",
       subtitle = "Escala natural\n",
       x="", 
       y = "")+
  guides(color=guide_legend(title=""))+
  theme_classic()+
  theme(plot.title = element_text(face = "bold", size = 24, hjust = 0.5),
        plot.subtitle = element_text(face = "italic", size = 24, hjust = 0.5),
        plot.caption = element_text(face = "italic", size = 12, hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 24),
        legend.text=element_text(size=14),
        legend.title = element_text(size=14),
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))

obitos_log<-ggplot(ilha, aes(x=date))+
  geom_line(aes(y=log(newDeaths)), size = 2)+
  scale_y_continuous(labels = function(x) format(x, big.mark="."))+
  geom_vline(aes(xintercept = lockdown.inicio), color="red", size=1.5)+
  geom_vline(aes(xintercept = lockdown.final), color="red", size=1.5)+
  annotate("text", x=as.Date("2020-05-11"), y=4, label= "Lockdown", color="red", size=5, fontface = 3)+
  scale_x_date(breaks = c(as.Date("2020-03-20"), as.Date("2020-03-27"), as.Date("2020-04-03"),
                          as.Date("2020-04-10"), as.Date("2020-04-17"), as.Date("2020-04-24"),
                          as.Date("2020-05-05"), as.Date("2020-05-17"),
                          as.Date("2020-05-29"), as.Date("2020-06-06")),
               date_labels = "%d/%b")+
  labs(title="",
       subtitle = "Log\n",
       x="", 
       y = "")+
  guides(color=guide_legend(title=""))+
  theme_classic()+
  theme(plot.title = element_text(face = "bold", size = 24, hjust = 0.5),
        plot.subtitle = element_text(face = "italic", size = 24, hjust = 0.5),
        plot.caption = element_text(face = "italic", size = 12, hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 24),
        legend.text=element_text(size=14),
        legend.title = element_text(size=14),
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))
annotate_figure(ggarrange(obitos_en,obitos_log),
                top=text_grob("Evolução do número de óbitos casos na Grande Ilha",
                              color = "black", face = "bold", size = 24)
)

ilha<-settingData(ilha, lockdown.inicio)

plotGraphicNewDeaths(ilha%>%filter(date>as.Date("2020-04-05")), "Novos óbitos","Grande Ilha", lockdown.inicio)
