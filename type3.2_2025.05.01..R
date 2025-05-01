library(dplyr)
rdata <- read.csv("P230606.csv")

md <- lm(Temperature~O3+Solar+Wind, data = rdata)

#### 문제1 ####
result1 <- summary(md)$coefficients["O3","Estimate"]
print(round(result1,3))
#0.172

#### 문제2 ####
result2 <- t.test(rdata$Wind, rdata$Temperature)$p.value
round(result2,3)
print(round(result2,3))
#0

#### 문제3 ####
O3 = 10
Solar = 90
Wind =20

ini <- data.frame(O3, Solar, Wind)

result3 <- pred <- predict(md, ini)

print(round(result3,3))
#68.334 
