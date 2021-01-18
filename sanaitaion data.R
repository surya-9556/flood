library(readxl)
library(tidyverse)
library(dplyr)
library(DataExplorer)
library(ggrepel)

flood_sanitaton_controls <- read_excel("C:/Users/Surya/Downloads/flood sanitaton controls.xls")

x=flood_sanitaton_controls$`Number of flood forecasting Stations - Level`
y=flood_sanitaton_controls$`Number of flood forecasting Stations - Inflow`
z=flood_sanitaton_controls$`Number of flood forecasting Stations - Total`

mean(x)
mean(y)
mean(z)

n=median(z)
p=sum(x)/100
b=23
ci<-replicate(b,{
  f=sample(z,size = n,replace = TRUE)
})

mean(ci)
sd(x)
a=sum(x)
se=sqrt((a*(a-1))/n)
se


flood_sanitaton_controls %>% ggplot(aes(`Number of flood forecasting Stations - Inflow`,`Number of flood forecasting Stations - Level`,label=`Code of state`))+
  geom_point(aes(col=`Name of State/UT`),size=2)+
  geom_text(nudge_x = 0.05)+
  scale_x_continuous(trans = "log10")+
  scale_y_continuous(trans = "log10")

plot_correlation(x,type = "continuous","y")