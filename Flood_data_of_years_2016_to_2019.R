library(readxl)
library(tidyverse)
library(DataExplorer)
library(dplyr)
library(ggthemes)
flood_data <- read_excel("C:/Users/Surya/Downloads/flood data.xlsx")

x = flood_data$Year
y = flood_data$`Area Affected (Mha)`
z = flood_data$`Damage to Public Utilities (Rs. Crore)`

head(flood_data)
mean(flood_data$`Area Affected (Mha)`)
median(flood_data$`Area Affected (Mha)`)
sd(flood_data$`Area Affected (Mha)`)

p = sum(y)/100
n=median(y)

b = 140
ci<-replicate(b,{
  x<-sample(p,size=n,replace = TRUE)
  x_hat<-sqrt(y*(1-y)/n)
})

mean(ci)

flood_data %>% ggplot(aes(Year,`Human Lives Lost Nos.`,col=Year))+
  geom_point()+
  xlab("Year")+
  ylab("Human lives lost")+
  ggtitle("Flood analysis report")+
  theme_economist()

plot(flood_data$`Area Affected (Mha)`)
plot(flood_data$`Human Lives Lost Nos.`)

plot_density(y)

plot_correlation(x,type = "continuous","z")