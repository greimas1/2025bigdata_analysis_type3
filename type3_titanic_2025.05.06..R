library(dplyr)
rdata <-read.csv("data/Titanic.csv")
str(rdata)
rdata$Gender <- as.factor(rdata$Gender)
rdata$Survived <- as.factor(rdata$Survived)
str(rdata)

tb <- table(rdata$Gender, rdata$Survived)
chisq.test(tb)
result1 <- chisq.test(tb)$statistic
print(round(result1,3))
#260.717



md <- glm(Survived ~ Gender + SibSp + Parch + Fare, data = rdata, family = "binomial")


--------------------------------------------------------------
library(dplyr)
rdata <-read.csv("data/Titanic.csv")
str(rdata)
rdata$Gender <- as.factor(rdata$Gender)
rdata$Survived <- as.factor(rdata$Survived)
str(rdata)

tb <- table(rdata$Gender, rdata$Survived)
chisq.test(tb)
result1 <- chisq.test(tb)$statistic
print(round(result1,3))
#260.717



md <- glm(Survived ~ Gender + SibSp + Parch + Fare, data = rdata, family = "binomial")
#SibSp,Parch 변수 처리 안해도 되나? 바로 로지스틱회귀 해도 되나??? <-- 살펴볼 것...
summary(md)$coefficients
#result2 <- summary(md)$coefficientsp["Parch","Estimate"]
result2 <- summary(md)$coefficients["Parch","Estimate"]
print(round(result2,3))
#-0.201

result3<-summary(md)$coefficients["SibSp","Estimate"]
round(exp(result3),3)
#0.702
