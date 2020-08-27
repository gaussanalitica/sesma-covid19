library(gridExtra)
library(grid)
library(forecast)
library(ggplot2)
library(ggpubr)
library(ggrepel)
library(readr)
library(dplyr)
library(cowplot)


insertFC = function(ma){
  ma$fc<-NA
  for(i in 2:nrow(ma)){
    ma$fc[i]<-ma$newCases[i]/ma$newCases[i-1]
  }
  ma$fc[!is.finite(ma$fc)] <- NA
  
  return(ma)
}


previsaoPlot = function(data, coluna, dias, tipo){
  
  ts<-ts(coluna, frequency = 365)
  a1 <- auto.arima(ts)
  
  fc <- forecast(a1, h = dias)
  
  bd_ar <- data.frame(data = data,
                      quantidade = a1$x, 
                      categoria = 'Observado') %>%
    bind_rows(data.frame(data = seq.Date(from = data[length(data)] + 1, length.out = dias, by = 'day'),
                         quantidade = trunc(fc$mean), 
                         categoria = 'Predito',
                         lower = trunc(fc$lower),
                         upper = trunc(fc$upper)))
  m1<-ggplot(data = bd_ar, aes(x = data, y = quantidade, color = categoria)) +
    geom_ribbon(aes(ymin = lower.95., ymax = upper.95.), size = 1, colour = 'grey80', alpha = .3) +
    geom_point(size = 2) +
    scale_y_continuous(labels = function(x) format(x, big.mark=".", scientific = F))+
    theme_classic() +
    labs(x = '', y = '') +
    guides(color=guide_legend(title=""))+
    theme_classic()+
    theme(plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
          plot.subtitle = element_text(face = "italic", size = 12, hjust = 0.5),
          plot.caption = element_text(face = "italic", size = 12, hjust = 0.5),
          axis.title = element_text(size = 17),
          axis.text = element_text(size = 22),
          legend.text=element_text(size=16),
          legend.title = element_text(size=14),
          legend.position = "right")
  tab<-bd_ar %>% 
    filter(categoria=="Predito") %>%
    group_by(data)%>%
    dplyr::select(data, lower.95., quantidade, upper.95.)%>%
    summarise(`Limite inf.` = lower.95.,
              Previsão = quantidade,
              `Limite sup.` = upper.95.)
  names(tab)[1]<-"Data"
  
  tab$Data<-format(tab$Data,"%d/%m/%Y")
  tab$`Limite inf.`<- format(tab$`Limite inf.`, big.mark=".", decimal.mark=",")
  tab$Previsão<- format(tab$Previsão, big.mark=".", decimal.mark=",")
  tab$`Limite sup.`<- format(tab$`Limite sup.`, big.mark=".", decimal.mark=",")
  
  tt <- ttheme_minimal()
  tbl <- tableGrob(tab, rows=NULL, theme=ttheme_minimal(base_size = 20))
  
  plot_row1 <- plot_grid(m1, tbl, rel_widths = c(1.5,1))
  
  title1 <- ggdraw() + 
    draw_label(
      paste("\nPrevisão de",tipo,"até o dia",format(data[length(data)]+dias, "%d de %B"),"\n"),
      fontface = 'bold',
      size = 24,
      x = 0,
      hjust = -0.7
    ) +
    theme(
      plot.margin = margin(0, 0, 0, 0)
    )
  return(plot_grid(title1, plot_row1, ncol = 1, rel_heights = c(0.1, 1)))
}

previsao=function(data, coluna, dias){
  
  ts<-ts(coluna, frequency = 365)
  a1 <- auto.arima(ts)
  
  fc <- forecast(a1, h = dias)
  
  return(trunc(fc$mean[1]))
}
