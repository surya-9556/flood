library(readxl)
library(tidyverse)
library(caret)
library(dplyr)
library(e1071)
library(DataExplorer)
library(ggthemes)

flood_data <- read_excel("C:/Users/Surya/Downloads/flood data.xlsx")
head(flood_data)

a=flood_data$`Area Affected (Mha)`
b=flood_data$`Damages to Crops - Area (Mha)`

x=sum(x)
y=sum(y)

p=(x*y)/y
q=(x*y)/x

p
q

plot(a,b)

n=median(x)
o=median(y)
p=sum(x)/100
b=140
ci<-replicate(b,{
  l=sample(x,size = n,replace = TRUE)
})

si<-replicate(b,{
  m=sample(y,size = n,replace = TRUE)
})

mean(ci)
mean(si)

se<-sqrt((x*(x-1))/n)
se

se1<-sqrt((y*(y-1))/n)
se1

cm<-table(si,ci)
cm

confusionMatrix(cm)

summary(a)
summary(b)

str(flood_data)

plot_density(a,b)
plot_correlation(a,type = "continuous","b")

flood_data %>% ggplot(aes(`Area Affected (Mha)`,`Damages to Crops - Area (Mha)`,col=Year,label=`Code of state`))+
  geom_point(size=3)+
  geom_text(nudge_x = 1)+
  xlab("Area Affected")+
  ylab("Damage to crops")+
  ggtitle("Flood damaged area and crops")+
  theme_dark()