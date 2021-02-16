library(readxl)
library(tidyverse)
library(DataExplorer)
library(dplyr)
library(ggthemes)
library(caret)
library(randomForest)
library(broom)
library(tibble)

set.seed(1)

flood_data <- read_excel("C:/Users/Surya/Downloads/flood data.xlsx")

x = flood_data$Year
y = flood_data$`Area Affected (Mha)`
z = flood_data$`Damage to Public Utilities (Rs. Crore)`

r = c("Andhra Pradesh","Bihar","Assam")

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
  ggplot(aes(`Area Affected (Mha)`,`Human Lives Lost Nos.`,col=Year,label=`Code of state`))+
  geom_text()+
  facet_grid(.~Year)

flood_data %>% ggplot(aes(`Human Lives Lost Nos.`,fill=Year))+
  geom_histogram(binwidth = 1)+
  xlab("Year")+
  ylab("Human lives lost")+
  ggtitle("Flood analysis report")+
  theme_excel()

plot(flood_data$`Area Affected (Mha)`)
plot(flood_data$`Human Lives Lost Nos.`)

plot_density(y)

plot_correlation(r,type = "all","y")

f=factor(flood_data$`Area Affected (Mha)`)
g=factor(flood_data$`Area Affected (Mha)`)

d_binomial <- tibble("target" = f,
                     "prediction" = g)

d_binomial

t=table(d_binomial)

cfm=tidy(t)

cfm

confusionMatrix(table(f,g))