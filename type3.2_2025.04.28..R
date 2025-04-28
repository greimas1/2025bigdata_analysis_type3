library(dplyr)

rdata<-read.csv("P230606.csv")

names(rdata)
str(rdata)
#### 1번문제 ####
md <- lm(Temperature ~ O3 + Solar + Wind, data = rdata)
summary(md)
summary(md)$coefficients
result1 <- summary(md)$coefficients["O3","Estimate"]
round(result1,3)
print(round(result1,3))

#### 2번문제 ####
t.test(rdata$Wind, rdata$Temperature)$p.value
result2 <- round(t.test(rdata$Wind, rdata$Temperature)$p.value,3)
print(result2)

#### 3번문제 ####
O3 = 10
Solar = 90
Wind = 20

data1 = data.frame(O3, Solar, Wind)

pred <- predict(md, newdata = data1)

result3 <- round(pred,3)

print(result3)
