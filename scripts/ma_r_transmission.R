# https://www.nature.com/articles/s41591-020-0822-7

library(R0)
library(readr)
library(ggplot2)
library(dplyr)

states <- read_delim("https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil-states.csv",
                     delim = ",", col_names = T)
ma<-states%>%filter(state=="MA")
ma<-ma[-nrow(ma),]
newCasesMA<-ma$newCases
names(newCasesMA)<-ma$date
distribution <- generation.time("gamma", c(7.0,4.5))
res.R <- estimate.R(newCasesMA, GT=distribution, methods=c("TD"), begin = names(newCasesMA)[1], end=names(newCasesMA)[length(newCasesMA)-1])
table<-data.frame(r=res.R[["estimates"]][["TD"]][["R"]], conf.int=res.R[["estimates"]][["TD"]][["conf.int"]])
table<-tibble::rownames_to_column(table, "date")
table$date<-as.Date(table$date)
ggplot(table, aes(x=date, y=r))+
  geom_line(size=.5)+
  geom_ribbon(aes(ymin = conf.int.lower, ymax = conf.int.upper), size = 1, colour = 'grey70', alpha = .3)+
  scale_y_continuous(breaks = c(0,1,2,3,4,5,6))+
  geom_hline(yintercept = 1)
