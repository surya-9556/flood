library(readxl)
library(tidyverse)
library(DataExplorer)
library(dplyr)
library(ggthemes)
library(randomForest)

flood_data <- read_excel("C:/Users/Surya/Downloads/flood data.xlsx")

x = flood_data$Year
y = flood_data$`Area Affected (Mha)`
z = flood_data$`Damage to Public Utilities (Rs. Crore)`

head(flood_data)
tail(flood_data)
mean(flood_data$`Area Affected (Mha)`)
median(flood_data$`Area Affected (Mha)`)
sd(flood_data$`Area Affected (Mha)`)


p = sum(y)/100
n=median(y)

b = 140
ci<-replicate(b,{
  x<-sample(p,size=n,replace = TRUE)
  x_hat<-sqrt(y*(y-1)/n)
})

mean(ci)

filter(flood_data,Year %in% c(2016,2017,2018,2019)) %>%
  ggplot(aes(`Area Affected (Mha)`,`Human Lives Lost Nos.`,col=Year))+
  geom_point()+
  facet_grid(.~Year)

flood_data %>% ggplot(aes(Year,`Human Lives Lost Nos.`,col=Year))+
  geom_boxplot()+
  xlab("Year")+
  ylab("Human lives lost")+
  ggtitle("Flood analysis report")+
  theme_economist()

plot(flood_data$`Area Affected (Mha)`)
plot(flood_data$`Human Lives Lost Nos.`)

plot_density(y)

plot_correlation(flood_data$`Name of State/UT`,type = "all","y")