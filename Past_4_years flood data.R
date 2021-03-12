library(readxl)
library(tidyverse)
library(DataExplorer)
library(dplyr)
library(ggthemes)
library(caret)
library(randomForest)
library(rpart)
library(rattle)
library(yardstick)
library(pROC)
library(mlbench)

flood_2 <- read_excel("C:/Users/Surya/Downloads/flood data.xlsx")

x = flood_2$Year
y = flood_2$`Area Affected (Mha)`
z = flood_2$`Damage to Public Utilities (Rs. Crore)`

r = c("Andhra Pradesh","Bihar","Assam")

head(flood_2)
tail(flood_2)
mean(flood_2$`Area Affected (Mha)`)
median(flood_2$`Area Affected (Mha)`)
sd(flood_2$`Area Affected (Mha)`)


p = sum(y)/100
n=median(y)

b = 140
ci<-replicate(b,{
  x<-sample(p,size=n,replace = TRUE)
  x_hat<-sqrt(y*(y-1)/n)
})

mean(ci)

filter(flood_2,Year %in% c(2016,2017,2018,2019)) %>%
  ggplot(aes(`Area Affected (Mha)`,`Human Lives Lost Nos.`,col=Year,label=`Code of state`))+
  geom_text()+
  facet_grid(.~Year)

summary(flood_2)

x=flood_2$`Human Lives Lost Nos.`

flood_2 = flood_2 %>% select(-`Human Lives Lost Nos.`)
str(flood_2)


flood_2 = mutate(flood_2, flood_2_discrete1 = ifelse(x > 50, 'H','L'))

flood_2 = rename(flood_2,`Human Lives Lost Nos.`=`flood_2_discrete1`)

s = createDataPartition(flood_2$`Human Lives Lost Nos.`,p=0.7,list=F)

train = flood_2[s, ]
test = flood_2[-s, ]

str(train)
str(test)

prop.table(table(flood_2$`Human Lives Lost Nos.`))
prop.table(table(train$`Human Lives Lost Nos.`))
prop.table(table(test$`Human Lives Lost Nos.`))

plot_correlation(r,type = "all","y")

dec = rpart(`Human Lives Lost Nos.` ~ .,data = flood_2)
fancyRpartPlot(dec)

predict(dec,newdata = test,type = 'prob')

p = predict(dec,newdata = test,type = 'class')

m=mean(test$`Human Lives Lost Nos.`== p)
f=m*100

i=mean(test$`Human Lives Lost Nos.`!= p)
o=i*100

test$`Human Lives Lost Nos.`
p

confusionMatrix(table(p,test$`Human Lives Lost Nos.`))

test = test %>% mutate(`Human Lives Lost Nos.` = factor(test$`Human Lives Lost Nos.`))

str(test)

trControl <- trainControl(method = "repeatedcv",
                          number = 10,
                          repeats = 2)

fit <- train(`Human Lives Lost Nos.` ~.,
             data = test,
             method = "knn",
             tuneLength=20,
             trControl=trControl,)

fit
plot(fit)
predict(fit, newdata = test, type = "raw")

per = metric_set(accuracy, sens, f_meas, precision)
per(data=test, estimate=p, truth=`Human Lives Lost Nos.`)