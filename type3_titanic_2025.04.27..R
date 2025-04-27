library(dplyr)

rdata <- read.csv("titanic.csv")
#str(rdata)
#summary(rdata)

#idx <- sample(1:nrow(rdata), nrow(rdata)*0.75)

####1. sex와 survived 변수간의 독립성 검정####
#chisq.test(rdata$Sex, rdata$Survived)
#result1 <- chisq.test(rdata$Sex, rdata$Survived)
#result1$statistic

tb1 <- table(rdata$Sex, rdata$Survived)
result1 <- chisq.test(tb1)
result1$statistic
print(round(result1$statistic,3))

####2. Sex, SibSp, Parch, Fare를 독립변수로 사용하여 로지스틱 회구모형 ####
# Parch 변수의 계수 값
names(rdata)
md_glm <- glm(Survived ~ Sex + SibSp + Parch + Fare, data = rdata, family = binomial )
#sum_glm <- summary(md_glm)
sum_glm <- summary(md_glm)$coefficients
#type(sum_glm)
sum_glm["Parch","Estimate"]

print(round(sum_glm["Parch","Estimate"],3))

#### 3. 추정 로지스틱 회귀모형 SibSbp 관련 오즈비 ####
print(round(exp(-0.201),3))

