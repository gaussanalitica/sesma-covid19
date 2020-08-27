#-------
#abrindo a base
ma <- read.table("ma_22abr20.csv",
                     sep = ";", header = T, stringsAsFactors = F)
#-------
#padronizando as informações
ma$data <- as.Date(ma$data, "%d/%m/%Y")

ma$casosNovos <- 0

ma <- ma[,c(1,5,2:4)]

ma$casosNovos[1] <- ma$casosAcumulados[1]

for(i in 2:nrow(ma)){
  ma$casosNovos[i] <- ma$casosAcumulados[i] - ma$casosAcumulados[i-1]
}

#calculando o coef de incidência
ma$coefincidencia <- (ma$casosAcumulados/7075181)*1000000

#calculando letalidade
ma$letalidade <- ma$obitosAcumulados/ma$casosAcumulados

#calculando taxa de mortalidade
ma$mortalidade<- (ma$obitosAcumulados/7075181)*1000000

#-------
#realizando os gráficos
#casos acumulados x óbitos acumulados
g1a<-ggplot(ma, aes(x=data))+
  geom_line(aes(y=casosAcumulados, colour="Casos acumulados"),size = 1.5)+
  geom_line(aes(y=obitosAcumulados,colour="Óbitos acumulados"), size = 1.5)+
  geom_vline(aes(xintercept = as.Date("2020-03-20")), linetype="dashed",color="black", size=.9)+
  geom_vline(aes(xintercept = as.Date("2020-03-29")), linetype="dashed",color="red", size=.9)+
  annotate(geom="text", x = as.Date("2020-03-25") ,y=1500, label="9 dias", size = 6)+
  labs(subtitle = "Escala natural",
       #caption = paste("Dados de 17:50,",format(Sys.Date()-1, "%d de %B de %Y")),
       x="", 
       y = "")+
  guides(color=guide_legend(title=""))+
  theme_classic()+
  theme(plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
        plot.subtitle = element_text(face = "italic", size = 12, hjust = 0.5),
        plot.caption = element_text(face = "italic", size = 12, hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 18),
        legend.text=element_text(size=14),
        legend.position="bottom")+
  scale_colour_manual(values = c("black", "red"))

g1b<-ggplot(ma, aes(x=data))+
  geom_line(aes(y=log(casosAcumulados), colour="Casos acumulados"),size = 1.5)+
  geom_line(aes(y=log(obitosAcumulados),colour="Óbitos acumulados"), size = 1.5)+
  labs(subtitle = "Escala logarítmica",
       #caption = paste("Dados de 17:50,",format(Sys.Date()-1, "%d de %B de %Y")),
       x="", 
       y = "")+
  guides(color=guide_legend(title=""))+
  theme_classic()+
  theme(plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
        plot.subtitle = element_text(face = "italic", size = 12, hjust = 0.5),
        plot.caption = element_text(face = "italic", size = 12, hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 18),
        legend.text=element_text(size=14),
        legend.position="bottom")+
  scale_colour_manual(values = c("black", "red"))
annotate_figure(ggarrange(g1a,g1b),
                top = text_grob("Evolução da Covid-19 no Maranhão", 
                                color = "black", face = "bold", size = 18),
                bottom = text_grob(paste("Dados fornecidos pelo Comitê de Crise do Gabinete da SES-MA, informações de",format(Sys.Date()-1, "%d de %B de %Y")))
)

g2a<-ggplot(ma, aes(x=data, y = casosAcumulados))+
  geom_line(size = 1.5) + 
  geom_hline(aes(yintercept = 1000), linetype = "dashed", color="blue", size=.9)+
  #geom_vline(aes(xintercept = as.Date("2020-03-21")), color="blue", size=.9)+
  geom_point(size = 2) + 
  labs(subtitle = "Escala natural",
       #caption= paste("Dados de 17:50,",format(Sys.Date()-1, "%d de %B de %Y")),
       x="", 
       y = "")+
  guides(color=guide_legend(title=""))+
  theme_classic()+
  theme(plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
        plot.subtitle = element_text(face = "italic", size = 14, hjust = 0.5),
        plot.caption = element_text(face = "italic", size = 14, hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 18),
        legend.text=element_text(size=14),
        legend.title = element_text(size=14),
        legend.position = "bottom")+
  scale_y_continuous(labels = number, limits=c(0, 1500))
g2b<-ggplot(ma, aes(x=data, y = log(casosAcumulados)))+
  geom_line(size = 1.5) + 
  #geom_hline(aes(yintercept = 1000), linetype = "dashed", color="red", size=.9)+
  #geom_vline(aes(xintercept = as.Date("2020-03-21")), color="blue", size=.9)+
  geom_point(size = 2) + 
  labs(subtitle = "Escala logarítmica",
       #caption= paste("Dados de 17:50,",format(Sys.Date()-1, "%d de %B de %Y")),
       x="", 
       y = "")+
  guides(color=guide_legend(title=""))+
  theme_classic()+
  theme(plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
        plot.subtitle = element_text(face = "italic", size = 14, hjust = 0.5),
        plot.caption = element_text(face = "italic", size = 14, hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 18),
        legend.text=element_text(size=14),
        legend.title = element_text(size=14),
        legend.position = "bottom")+
  scale_y_continuous(labels = number)
annotate_figure(ggarrange(g2a,g2b),
                top = text_grob("Evolução dos casos de Covid-19 no Maranhão", 
                                color = "black", face = "bold", size = 18),
                bottom = text_grob(paste("Dados fornecidos pelo Comitê de Crise do Gabinete da SES-MA, informações de",format(Sys.Date()-1, "%d de %B de %Y")))
)

g3a<-ggplot(ma, aes(x=data, y = obitosAcumulados))+
  geom_line(size = 1.5, colour="red") + 
  geom_hline(aes(yintercept = 50), linetype = "dashed", color="blue", size=.9)+
  #geom_vline(aes(xintercept = as.Date("2020-03-21")), color="blue", size=.9)+
  geom_point(size = 2, colour="red") + 
  labs(subtitle = "Escala natural",
       #caption= paste("Dados de 17:50,",format(Sys.Date()-1, "%d de %B de %Y")),
       x="", 
       y = "")+
  guides(color=guide_legend(title=""))+
  theme_classic()+
  theme(plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
        plot.subtitle = element_text(face = "italic", size = 14, hjust = 0.5),
        plot.caption = element_text(face = "italic", size = 14, hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 18),
        legend.text=element_text(size=14),
        legend.title = element_text(size=14),
        legend.position = "bottom")+
  scale_y_continuous(labels = number, limits=c(0, 80), breaks = c(0,20,40,50,60,80))
g3b<-ggplot(ma, aes(x=data, y = log(obitosAcumulados)))+
  geom_line(size = 1.5, colour="red") + 
  #geom_hline(aes(yintercept = 1000), linetype = "dashed", color="red", size=.9)+
  #geom_vline(aes(xintercept = as.Date("2020-03-21")), color="blue", size=.9)+
  geom_point(size = 2, colour="red") + 
  labs(subtitle = "Escala logarítmica",
       #caption= paste("Dados de 17:50,",format(Sys.Date()-1, "%d de %B de %Y")),
       x="", 
       y = "")+
  guides(color=guide_legend(title=""))+
  theme_classic()+
  theme(plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
        plot.subtitle = element_text(face = "italic", size = 14, hjust = 0.5),
        plot.caption = element_text(face = "italic", size = 14, hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 18),
        legend.text=element_text(size=14),
        legend.title = element_text(size=14),
        legend.position = "bottom")+
  scale_y_continuous(labels = number)
annotate_figure(ggarrange(g3a,g3b),
                top = text_grob("Evolução dos óbitos de Covid-19 no Maranhão", 
                                color = "black", face = "bold", size = 18),
                bottom = text_grob(paste("Dados fornecidos pelo Comitê de Crise do Gabinete da SES-MA, informações de",format(Sys.Date()-1, "%d de %B de %Y"))))

g4a<-ggplot(ma, aes(x=data, y = casosNovos))+
  geom_line(size = 1.5) + 
  #geom_hline(aes(yintercept = 1000), linetype = "dashed", color="blue", size=.9)+
  #geom_vline(aes(xintercept = as.Date("2020-03-21")), color="blue", size=.9)+
  geom_point(size = 2) + 
  labs(subtitle = "Escala natural",
       #caption= paste("Dados de 17:50,",format(Sys.Date()-1, "%d de %B de %Y")),
       x="", 
       y = "")+
  guides(color=guide_legend(title=""))+
  theme_classic()+
  theme(plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
        plot.subtitle = element_text(face = "italic", size = 14, hjust = 0.5),
        plot.caption = element_text(face = "italic", size = 14, hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 18),
        legend.text=element_text(size=14),
        legend.title = element_text(size=14),
        legend.position = "bottom")+
  scale_y_continuous(labels = number)
g4b<-ggplot(ma, aes(x=data, y = log(casosNovos)))+
  geom_line(size = 1.5) + 
  #geom_hline(aes(yintercept = 1000), linetype = "dashed", color="red", size=.9)+
  #geom_vline(aes(xintercept = as.Date("2020-03-21")), color="blue", size=.9)+
  geom_point(size = 2) + 
  labs(subtitle = "Escala logarítmica",
       #caption= paste("Dados de 17:50,",format(Sys.Date()-1, "%d de %B de %Y")),
       x="", 
       y = "")+
  guides(color=guide_legend(title=""))+
  theme_classic()+
  theme(plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
        plot.subtitle = element_text(face = "italic", size = 14, hjust = 0.5),
        plot.caption = element_text(face = "italic", size = 14, hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 18),
        legend.text=element_text(size=14),
        legend.title = element_text(size=14),
        legend.position = "bottom")+
  scale_y_continuous(labels = number)
annotate_figure(ggarrange(g4a,g4b),
                top = text_grob("Evolução dos casos novos de Covid-19 no Maranhão", 
                                color = "black", face = "bold", size = 18),
                bottom = text_grob(paste("Dados fornecidos pelo Comitê de Crise do Gabinete da SES-MA, informações de",format(Sys.Date()-1, "%d de %B de %Y")))
)

g5a<-ggplot(ma, aes(x=data, y = obitosNovos))+
  geom_line(size = 1.5, colour="red") + 
  #geom_hline(aes(yintercept = 1000), linetype = "dashed", color="blue", size=.9)+
  #geom_vline(aes(xintercept = as.Date("2020-03-21")), color="blue", size=.9)+
  geom_point(size = 2, colour="red") + 
  labs(subtitle = "Escala natural",
       #caption= paste("Dados de 17:50,",format(Sys.Date()-1, "%d de %B de %Y")),
       x="", 
       y = "")+
  guides(color=guide_legend(title=""))+
  theme_classic()+
  theme(plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
        plot.subtitle = element_text(face = "italic", size = 14, hjust = 0.5),
        plot.caption = element_text(face = "italic", size = 14, hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 18),
        legend.text=element_text(size=14),
        legend.title = element_text(size=14),
        legend.position = "bottom")+
  scale_y_continuous(labels = number)
g5b<-ggplot(ma, aes(x=data, y = log(obitosNovos)))+
  geom_line(size = 1.5, colour="red") + 
  #geom_hline(aes(yintercept = 1000), linetype = "dashed", color="red", size=.9)+
  #geom_vline(aes(xintercept = as.Date("2020-03-21")), color="blue", size=.9)+
  geom_point(size = 2, colour="red") + 
  labs(subtitle = "Escala logarítmica",
       #caption= paste("Dados de 17:50,",format(Sys.Date()-1, "%d de %B de %Y")),
       x="", 
       y = "")+
  guides(color=guide_legend(title=""))+
  theme_classic()+
  theme(plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
        plot.subtitle = element_text(face = "italic", size = 14, hjust = 0.5),
        plot.caption = element_text(face = "italic", size = 14, hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 18),
        legend.text=element_text(size=14),
        legend.title = element_text(size=14),
        legend.position = "bottom")+
  scale_y_continuous(labels = number)
annotate_figure(ggarrange(g5a,g5b),
                top = text_grob("Evolução dos óbitos novos de Covid-19 no Maranhão", 
                                color = "black", face = "bold", size = 18),
                bottom = text_grob(paste("Dados fornecidos pelo Comitê de Crise do Gabinete da SES-MA, informações de",format(Sys.Date()-1, "%d de %B de %Y")))
)

ggplot(ma, aes(x=data, y = coefincidencia))+
  geom_line(size = 1.5, colour="blue") + 
  geom_point(size = 2, colour="blue") + 
  labs(subtitle = "Coeficiente de indicêndia (casos/1 milhão de habitantes)",
       caption= paste("Dados de 17:50,",format(Sys.Date()-1, "%d de %B de %Y")),
       x="", 
       y = "")+
  guides(color=guide_legend(title=""))+
  theme_classic()+
  theme(plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
        plot.subtitle = element_text(face = "italic", size = 12, hjust = 0.5),
        plot.caption = element_text(face = "italic", size = 12, hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 15),
        legend.text=element_text(size=14),
        legend.title = element_text(size=14),
        legend.position = "bottom")+
  scale_y_continuous(labels = number)+
  scale_colour_manual(values = c("blue"))

ggplot(ma, aes(x=data, y = coefincidencia))+
  geom_bar(stat = "identity", fill="black")+
  geom_text(data=ma%>%filter(data==Sys.Date()-1),
            aes(x=data, label=sprintf("%0.2f", round(coefincidencia, digits = 2))), 
            vjust=-0.5, color="black", size=5)+
  geom_line(size = 1.5, colour="blue") + 
  geom_point(size = 2, colour="blue") + 
  labs(title = "Coeficiente de incidência (casos/1 milhão de habitantes)",
       caption= paste("Dados fornecidos pelo Comitê de Crise do Gabinete da SES-MA, informações de",format(Sys.Date()-1, "%d de %B de %Y")),
       x="", 
       y = "")+
  guides(color=guide_legend(title=""))+
  theme_classic()+
  theme(plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
        plot.subtitle = element_text(face = "italic", size = 12, hjust = 0.5),
        plot.caption = element_text(face = "italic", size = 12, hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 18),
        legend.text=element_text(size=14),
        legend.title = element_text(size=14),
        legend.position = "bottom")+
  scale_y_continuous(labels = number)+
  scale_colour_manual(values = c("blue"))

ggplot(ma, aes(x=data, y = mortalidade))+
  geom_bar(stat = "identity", fill="#FF6666")+
  geom_text(data=ma%>%filter(data==Sys.Date()-1),
            aes(x=data, label=sprintf("%0.2f", round(mortalidade, digits = 2))), 
            vjust=-0.5, color="black", size=5)+
  geom_line(size = 1.5, colour="blue") + 
  geom_point(size = 2, colour="blue") + 
  labs(title = "Coeficiente de mortalidade (óbitos/1 milhão de habitantes)",
       caption= paste("Dados fornecidos pelo Comitê de Crise do Gabinete da SES-MA, informações de",format(Sys.Date()-1, "%d de %B de %Y")),
       x="", 
       y = "")+
  guides(color=guide_legend(title=""))+
  theme_classic()+
  theme(plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
        plot.subtitle = element_text(face = "italic", size = 12, hjust = 0.5),
        plot.caption = element_text(face = "italic", size = 12, hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 18),
        legend.text=element_text(size=14),
        legend.title = element_text(size=14),
        legend.position = "bottom")+
  scale_y_continuous(labels = number)+
  scale_colour_manual(values = c("blue"))

ggplot(ma, aes(x=data, y = letalidade))+
  geom_text(data=ma%>%filter(data==Sys.Date()-1),
            aes(x=data, label=sprintf("%0.2f", round(letalidade*100, digits = 2))), 
            vjust=-0.5, color="black", size=5)+
  geom_line(size = 1.5, colour="blue") + 
  geom_point(size = 2, colour="blue") + 
  labs(title = "Taxa de letalidade (casos/óbitos)",
       caption= paste("Dados fornecidos pelo Comitê de Crise do Gabinete da SES-MA, informações de",format(Sys.Date()-1, "%d de %B de %Y")),
       x="", 
       y = "")+
  guides(color=guide_legend(title=""))+
  theme_classic()+
  theme(plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
        plot.subtitle = element_text(face = "italic", size = 12, hjust = 0.5),
        plot.caption = element_text(face = "italic", size = 12, hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 18),
        legend.text=element_text(size=14),
        legend.title = element_text(size=14),
        legend.position = "bottom")+
  scale_y_continuous(labels = percent)+
  scale_colour_manual(values = c("blue"))

#analisando em perspectiva comparada no brasil

brasil <- read.table("arquivo_geral.csv",
                     sep = ";", header = T, stringsAsFactors = F)
# convertendo a data (char)
brasil$data <- as.Date(brasil$data)

#pegando a base da população em outro repositório
brasil2<-read.table("https://brasil.io/dataset/covid19/caso?format=csv",
                    sep = ",", header = T, stringsAsFactors = F)
#filtrando somente os estados
states <-brasil2 %>% 
  filter(place_type=="state") %>%
  select(-starts_with(c("city","city_ibge_code")))
#filtrando a populacao dos estados
populacao_estados <- states %>%
  rename(populacao2019=estimated_population_2019,
         estado = state)%>%
  distinct(estado, .keep_all = T)%>%
  select(estado,populacao2019)

#mergindo a variavel populacao
brasil<-merge(brasil,populacao_estados, by="estado")

#calculando o coef de incidência
brasil$coefincidencia <- (brasil$casosAcumulados/brasil$populacao2019)*1000000

#calculando letalidade
brasil$letalidade <- (brasil$obitosAcumulados/brasil$casosAcumulados)*100

#calculando taxa de mortalidade
brasil$mortalidade<- (brasil$obitosAcumulados/brasil$populacao2019)*1000000

brasil %<>% mutate(maranhao_logic = ifelse(estado=="MA", "1", "0"))

correlacao<- cor(log(brasil[brasil$data==Sys.Date()-1,"casosAcumulados"]), 
                 log(brasil[brasil$data==Sys.Date()-1,"obitosAcumulados"]),
                 method = "pearson")

ggplot(brasil%>%filter(data==Sys.Date()-1),
       aes(x=log(casosAcumulados), y=log(obitosAcumulados)))+
  geom_point(aes(color=maranhao_logic), size=3.5)+
  annotate("text", x=5, y=6, label= paste("r:",sprintf("%0.3f", round(correlacao, digits = 3))),
           color="black", size=6, fontface = 3)+
  geom_smooth(mapping = aes(linetype = "r2"),
              method = "lm",
              formula = y ~ x, se = FALSE,
              color = "blue")+
  #geom_text(aes(label = estado),
  #          color = "gray20",
  #          data = filter(brasil, data==Sys.Date()-1 & estado=="MA"),
  #          position = position_dodge(width = 1),
  #          vjust = 0.5, hjust=-0.4, size = 5)+
  geom_label_repel(aes(label = estado),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50')+
  labs(title="Correlação casos acumulados x óbitos acumulados entre UFs",
       caption = paste("Dados coletados do Ministério da Saúde, informações de",format(Sys.Date()-1, "%d de %B de %Y"),"- 15:30"),
       x="Casos acumulados (log)", 
       y = "Óbitos acumulados (log)")+
  theme_classic()+
  theme(plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
        plot.subtitle = element_text(face = "italic", size = 12, hjust = 0.5),
        plot.caption = element_text(face = "italic", size = 12, hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 18),
        legend.text=element_text(size=14),
        legend.position="none")+
  scale_colour_manual(values = c("black", "red"))


ggplot(brasil%>%filter(data==Sys.Date()-1),
       aes(x=coefincidencia, y=letalidade))+
  geom_point(aes(color=maranhao_logic), size=3.5)+
  geom_hline(aes(yintercept = 6.4), linetype = "dashed", color="red", size=.9)+
  geom_vline(aes(xintercept = 210.7), linetype = "dashed", color="blue", size=.9)+
  annotate("text", x=400, y=10, label= "I", color="black", size=8, fontface = 3)+
  annotate("text", x=400, y=5, label= "II", color="black", size=8, fontface = 3)+
  annotate("text", x=150, y=5.5, label= "III", color="black", size=8, fontface = 3)+
  annotate("text", x=150, y=10, label= "IV", color="black", size=8, fontface = 3)+
  #geom_text(aes(label = estado),
  #          color = "gray20",
  #          data = filter(brasil, data==Sys.Date()-1 & estado=="MA"),
  #          position = position_dodge(width = 1),
  #          vjust = -0.5, hjust=-0.1, size = 5)+
  geom_label_repel(aes(label = estado),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50')+
  labs(title="Correlação coeficiente de incidência x taxa de letalidade entre UFs",
       caption = paste("Dados coletados do Ministério da Saúde, informações de",format(Sys.Date()-1, "%d de %B de %Y"),"- 15:30"),
       x="Coeficiente de incidência (casos/1 milhão de habitantes)", 
       y = "Taxa de letalidade (casos/óbitos)%")+
  theme_classic()+
  theme(plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
        plot.subtitle = element_text(face = "italic", size = 12, hjust = 0.5),
        plot.caption = element_text(face = "italic", size = 12, hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 18),
        legend.text=element_text(size=14),
        legend.position="none")+
  scale_colour_manual(values = c("black", "red"))
