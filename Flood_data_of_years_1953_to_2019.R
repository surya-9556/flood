library(readxl)
library(tidyverse)
library(dplyr)
library(DataExplorer)
library(ggthemes)

flood_datas <- read_excel("C:/Users/Surya/Downloads/flood data from 1953 to 2017.xls")

x=type.convert(flood_datas$`Area affected in (m.ha.)`)
y=type.convert(flood_datas$`Population affected in (million)`)
z=type.convert(flood_datas$`Damage to Crops - Area (m.ha.)`)
p=type.convert(flood_datas$`Damage to Houses - Nos.`)
q=type.convert(flood_datas$`Human lives lost nos.`)
t=type.convert(flood_datas$`Cattle lost nos.`)
b=flood_datas$Year


flood_datas %>% ggplot(aes(Year,x,col=Year))+
  geom_point(size=3)+
  xlab("Year")+
  ylab("Area Affected in (m.ha.)")+
  ggtitle("Flood data from 1953 to 2019")+
  theme_foundation()

plot(x)
plot(y)
plot(z)
plot(p)
plot(q)
plot(t)

plot_correlation(x,type = "discrete","y")