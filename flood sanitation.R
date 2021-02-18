library(readxl)
library(tidyverse)
library(caret)
library(rpart)
library(rattle)
library(yardstick)

flood_sanitaton <- read_excel("flood sanitaton controls.xls")

summary(flood_sanitaton)

flood_sanitaton = flood_sanitaton %>% select(-`Name of State/UT`) %>%
  mutate(`Number of flood forecasting Stations - Inflow` = factor(`Number of flood forecasting Stations - Inflow`))

str(flood_sanitaton)

#flood sanitation control based in total number they are avalible

flood_sanitaton = mutate(flood_sanitaton, `Number of flood forecasting Stations - Total`, ifelse(`Number of flood forecasting Stations - Total` > 10, 'H' , 'L'))

flood_sanitaton = rename(flood_sanitaton, `flood forecasting Stations`=`Number of flood forecasting Stations - Total`)

s=createDataPartition(flood_sanitaton$`ifelse(...)`,p=0.6,list = F)

train = flood_sanitaton[s, ]
test = flood_sanitaton[-s, ]

str(train)
str(test)

prop.table(table(flood_sanitaton$`ifelse(...)`))
prop.table(table(train$`ifelse(...)`))
prop.table(table(test$`ifelse(...)`))

dec=rpart(`ifelse(...)` ~ ., data = flood_sanitaton)
fancyRpartPlot(dec)

predict(dec, newdata = test, type = "prob")

p=predict(dec, newdata = test, type = "class")

mean(flood_sanitaton$`ifelse(...)` == p) * 100

mean(flood_sanitaton$`ifelse(...)` != p) * 100

test = test %>% mutate(`ifelse(...)`=factor(`ifelse(...)`))

str(test)

confusionMatrix(table(p,test$`ifelse(...)`))

dat = metric_set(accuracy, sens, precision, f_meas)

dat(data = test, estimate = p, truth = `ifelse(...)`)

#flood sanitation control levels based on in flow

flood_sanitaton = mutate(flood_sanitaton, `Number of flood forecasting Stations - Inflow` = ifelse(`Number of flood forecasting Stations - Level` >5, 'H' , 'L'))

flood_sanitaton = rename(flood_sanitaton, `flood forecasting inflow`=`Number of flood forecasting Stations - Inflow`)

s=createDataPartition(flood_sanitaton$`flood forecasting inflow`,p=0.6,list = F)

train = flood_sanitaton[s, ]
test = flood_sanitaton[-s, ]

str(train)
str(test)

prop.table(table(flood_sanitaton$`flood forecasting inflow`))
prop.table(table(train$`flood forecasting inflow`))
prop.table(table(test$`flood forecasting inflow`))

dec=rpart(`flood forecasting inflow` ~ ., data = flood_sanitaton)
fancyRpartPlot(dec)

predict(dec, newdata = test, type = "prob")

p=predict(dec, newdata = test, type = "class")

mean(flood_sanitaton$`flood forecasting inflow` == p) * 100

mean(flood_sanitaton$`flood forecasting inflow` != p) * 100

test = test %>% mutate(`flood forecasting inflow`=factor(`flood forecasting inflow`))

str(test)

confusionMatrix(table(p,test$`flood forecasting inflow`))

dat = metric_set(accuracy, sens, precision, f_meas)

dat(data = test, estimate = p, truth = `flood forecasting inflow`)