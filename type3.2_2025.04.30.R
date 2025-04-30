library(dplyr)
rdata <- read.csv("P230606.csv")
#lm(Temperature ~ O3 + Solar + Wind, data = rdata)
md <- lm(Temperature ~ O3 + Solar + Wind, data = rdata)
#summary(lm)
summary(md)

#### 1번 #####
summary(md)$coefficients["O3","Estimate"]
result1 <- summary(md)$coefficients["O3","Estimate"]
round(result1,3)
print(round(result1,3))

#### 2번 #####
#t.test(rdata$wind, rdata$Temperature)
t.test(rdata$Wind, rdata$Temperature)
result2 <- t.test(rdata$Wind, rdata$Temperature)$p.value

round(result2,3)
print(round(result2,3))

#### 3번 #####
O3 = 10
Solar = 90
Wind = 20
ini <- data.frame(O3, Solar, Wind)

pred <- predict(md, newdata = ini)
result3 <- round(pred,3)
print(result3)



