library(dplyr)
rdata <- read.csv("data/Titanic.csv")

#str(rdata)

rdata$Survived <- as.factor(rdata$Survived)
rdata$Gender <- as.factor(rdata$Gender)
#str(rdata)

tb <- table(rdata$Gender, rdata$Survived)
result1 <- chisq.test(tb)$Statistic
print(round(result1,3))

--------------------수정------------------
library(dplyr)
rdata <- read.csv("data/Titanic.csv")

#str(rdata)

rdata$Survived <- as.factor(rdata$Survived)
rdata$Gender <- as.factor(rdata$Gender)
#str(rdata)

tb <- table(rdata$Gender, rdata$Survived)
#result1 <- chisq.test(tb)$Statistic
result1 <- chisq.test(tb)$statistic
print(round(result1,3))

----------------------------------------
library(dplyr)
rdata <- read.csv("data/Titanic.csv")

#str(rdata)

rdata$Survived <- as.factor(rdata$Survived)
rdata$Gender <- as.factor(rdata$Gender)
#str(rdata)

tb <- table(rdata$Gender, rdata$Survived)
#result1 <- chisq.test(tb)$Statistic
result1 <- chisq.test(tb)$statistic
print(round(result1,3))
#260.717

md <- glm(Gender~SibSp + Parch + Fare, data = rdata, family = "binomial")
summary(md)
result2 <- summary(md)$coefficients["Parch","Estimate"]
print(round(result2,3))
#-0.579 (05.06.일자 코딩 결과와 답이 다름, 시험환경에서 어려움.. 이상하게 초기화 해서 실행해도 잘 되지 않음)

result3 <- summary(md)$coefficients["SibSp","Estimate"]
print(round(exp(result3),3))
#1.004 (05.06.일자 코딩 결과와 답이 다름, 시험환경에서 어려움.. 이상하게 초기화 해서 실행해도 잘 되지 않음)






