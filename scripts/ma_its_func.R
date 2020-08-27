library(nlme)
library(car)

settingData = function (dataset, lockdown.date){
  
  dataset$time<-1:nrow(dataset)
  
  dataset$level<-NA
  dataset$level[dataset$date<lockdown.date]<-0
  dataset$level[dataset$date>=lockdown.date]<-1
  
  dataset$trend<-NA
  dataset$trend[dataset$date<lockdown.date]<-0
  dataset$trend[dataset$date>=lockdown.date]<-seq(1:sum(dataset$date>=lockdown.date))
  
  return(dataset)
  
}

modelSummary = function(dataset, variable){
  
  m1<-lm(variable~time + level + trend,
         data=dataset)
  
  return(summary(m1))
}

plotGraphicNewCases = function(dataset, description, local, lockdown.date){
  
  m1<-lm(newCases ~ time + level + trend,
         data=dataset)
  
  print(summary(m1))
  
  ggplot(dataset)+
    geom_point(data = dataset%>%filter(level==0), aes(x=date, newCases, colour="Antes lockdown"), size=2)+
    geom_point(data = dataset%>%filter(level==1), aes(x=date, newCases, colour="Após lockdown"), size=2)+
    scale_y_continuous(labels = function(x) format(x, big.mark="."))+
    scale_colour_manual(values = c("blue", "red"))+
    geom_vline(xintercept = lockdown.date, size=1.5)+
    geom_smooth(data=dataset%>%filter(level==0), 
                aes(x=date, y=newCases), 
                method = "lm", formula = "y ~ x", se = T, colour="blue", size=1.5)+
    geom_segment(aes(x=date[1], y=m1$coefficients[1] + m1$coefficients[2], 
                     xend=date[nrow(dataset)], yend=m1$coefficients[1] +  m1$coefficients[2]*nrow(dataset)),
                 colour="blue", linetype="dashed", size=1.5)+
    geom_smooth(data=dataset%>%filter(level==1), 
                aes(x=date, y=newCases), 
                method = "lm", formula = "y ~ x", se = T, colour="red", size=1.5)+
    labs(title=local,
         x="", 
         y ="")+
    guides(color=guide_legend(title=""))+
    theme_classic()+
    theme(plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
          plot.subtitle = element_text(face = "italic", size = 12, hjust = 0.5),
          plot.caption = element_text(face = "italic", size = 12, hjust = 0.5),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 20),
          legend.text=element_text(size=14),
          legend.title = element_text(size=14),
          legend.position = "none")
}

plotGraphicNewDeaths = function(dataset, description, local, lockdown.date){
  
  m1<-lm(newDeaths ~ time + level + trend,
         data=dataset)
  
  print(summary(m1))
  
  ggplot(dataset)+
    geom_point(data = dataset%>%filter(level==0), aes(x=date, newDeaths, colour="Antes lockdown"), size=2)+
    geom_point(data = dataset%>%filter(level==1), aes(x=date, newDeaths, colour="Após lockdown"), size=2)+
    scale_y_continuous(labels = function(x) format(x, big.mark="."))+
    scale_colour_manual(values = c("blue", "red"))+
    geom_vline(xintercept = lockdown.date, size=1.5)+
    geom_smooth(data=dataset%>%filter(level==0), 
                aes(x=date, y=newDeaths), 
                method = "lm", formula = "y ~ x", se = T, colour="blue", size=1.5)+
    geom_segment(aes(x=date[1], y=m1$coefficients[1] + m1$coefficients[2], 
                     xend=date[nrow(dataset)], yend=m1$coefficients[1] +  m1$coefficients[2]*nrow(dataset)),
                 colour="blue", linetype="dashed", size=1.5)+
    geom_smooth(data=dataset%>%filter(level==1), 
                aes(x=date, y=newDeaths), 
                method = "lm", formula = "y ~ x", se = T, colour="red", size=1.5)+
    labs(title=local,
         x="", 
         y = "")+
    guides(color=guide_legend(title=""))+
    theme_classic()+
    theme(plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
          plot.subtitle = element_text(face = "italic", size = 12, hjust = 0.5),
          plot.caption = element_text(face = "italic", size = 12, hjust = 0.5),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 20),
          legend.text=element_text(size=14),
          legend.title = element_text(size=14),
          legend.position = "none")
}

# we have to use a gls function. but it needs some complement because it doesn't work in the function
# we must implement corARMA function on the gls function
# implementations will be processed latter
calcDifference = function(dataset, variable, lockdown.date, time=0){
  m10<-lm(variable~time + level + trend,
         data=dataset)
  pred<-fitted(m10)
  
  if(time==0){
    cfac<-m10$coef[1]+m10$coef[2]*nrow(dataset)+
      m10$coef[3]+m10$coef[4]*nrow(dataset)
    cat("\nDifference between predicted and observed values in each day after lockdown\n")
    pred <- pred[which(dataset$date==lockdown.date):length(pred)]
    cat("\n* Absolute: \n")
    print(pred-cfac)
    cat("\n* Proportional: \n")
    print((pred-cfac)/cfac)
  }
  else{
    pred<-fitted(m10)[which(dataset$date==lockdown.date)+time]
    cfac<-m10$coef[1]+m10$coef[2]*(which(dataset$date==lockdown.date)+time)+
            m10$coef[3]+m10$coef[4]*4
    cat(sprintf("\nDifference between predicted and observed values at the day %d after lockdown\n", time))
    cat("\n* Absolute: \n")
    print(pred-cfac)
    cat("\n* Proportional: \n")
    print((pred-cfac)/cfac)
  }
}

#plotGraphic = function(dataset, variable, description, local, lockdown.date){

#  m1<-lm(variable~time + level + trend,
#         data=dataset)

#  print(summary(m1))
#  pred_m1<-fitted(m1)

#  plot(dataset$time,variable,
#       ylab=paste0("", description),
#       main =local,
#ylim=c(0,500),
#       xlab="",
#       pch=20,
#       col="black",
#       xaxt="n", cex.main=2, cex.lab=1.5, cex.axis=1.5)

#  axis(1, at=1:nrow(dataset), labels=format(dataset$date, "%d/%m"))

#  abline(v=which(dataset$date==lockdown.date),lty=2)

#  lines(dataset$time[dataset$level==0], pred_m1[1:which(dataset$date==lockdown.date)-1], col="red", lwd = 2)
#  lines(dataset$time[dataset$level==1], pred_m1[which(dataset$date ==lockdown.date):nrow(dataset)], col="blue", lwd = 2)

#  segments(1, m1$coefficients[1] + m1$coefficients[2], nrow(dataset), 
#           m1$coefficients[1] +  m1$coefficients[2]*nrow(dataset), 
#           lty = 2, lwd = 2, col = "red")
#}
