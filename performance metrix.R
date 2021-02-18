library(readxl) # read xl data
library(tidyverse) # tidy package
library(caret) # classification package
library(rpart) # making decission trees
library(rattle) # plotting decission trees
library(yardstick) #

# importing data set
flood_2 <- read_excel("C:/Users/Surya/Downloads/flood data.xlsx")
summary(flood_2)

# assigining coloum to a variable
x=flood_2$`Human Lives Lost Nos.`

#removing coloum and converting some colouns to factors
flood_2 = flood_2 %>% select(-`Human Lives Lost Nos.`) %>% 
  mutate(`Area Affected (Mha)`=factor(`Area Affected (Mha)`), `Damages to Crops - Area (Mha)`= factor(`Damages to Crops - Area (Mha)`))

str(flood_2)

#creating binary variables
flood_2 = mutate(flood_2, flood_2_discrete1 = ifelse(x > 50, 'H','L'))

#renaming coloum
flood_2 = rename(flood_2,`Human Lives Lost Nos.`=`flood_2_discrete1`)

#creating data partition
s = createDataPartition(flood_2$`Human Lives Lost Nos.`,p=0.7,list=F)

#creating new data sets from the partition data
train = flood_2[s, ]
test = flood_2[-s, ]

str(train)
str(test)

#creating proportion
prop.table(table(flood_2$`Human Lives Lost Nos.`))
prop.table(table(train$`Human Lives Lost Nos.`))
prop.table(table(test$`Human Lives Lost Nos.`))

#creating data partition and plotting it
dec = rpart(`Human Lives Lost Nos.` ~ .,data = train)
fancyRpartPlot(dec)

dec = rpart(`Human Lives Lost Nos.` ~ .,data = flood_2)
fancyRpartPlot(dec)

dec = rpart(`Human Lives Lost Nos.` ~ .,data = test)
fancyRpartPlot(dec)

#predicting the probability
predict(dec,newdata = test,type = 'prob')

p = predict(dec,newdata = test,type = 'class')

#calculating accuracy
m=mean(test$`Human Lives Lost Nos.`== p)
f=m*100

#creating misclassification
i=mean(test$`Human Lives Lost Nos.`!= p)
o=i*100

test$`Human Lives Lost Nos.`
p

#confusion matrix
confusionMatrix(table(p,test$`Human Lives Lost Nos.`))

test = test %>% mutate(`Human Lives Lost Nos.` = factor(test$`Human Lives Lost Nos.`))

str(test)

sens(data = test, estimate=p, truth = `Human Lives Lost Nos.`)

precision(data = test, estimate=p, truth = `Human Lives Lost Nos.`)

f_meas(data = test, estimate=p, truth = `Human Lives Lost Nos.`)

#creating decission tree
dec2 = rpart(`Human Lives Lost Nos.` ~ `Area Affected (Mha)`+`Damage to Houses - Nos.`+`Cattle Lost Nos.`,data=test)
fancyRpartPlot(dec2)

predict(dec2,newdata = test,type = 'prob')
p1 = predict(dec2,newdata = test,type = 'class')

#performance metrix for the dataset
per = metric_set(accuracy, sens, f_meas, precision)
per(data=test, estimate=p, truth=`Human Lives Lost Nos.`)