library(dplyr)
library(caret) #  error를 구할 때   confusionMatrix활용
rdata <- read.csv("customer.csv")
#str(rdata)
rdata$status <- as.factor(rdata$status)

idx <- sample(1:nrow(rdata), nrow(rdata)*0.7)
train <- rdata[idx,]
valid <- rdata[-idx,]
#md <- glm(status~. calls + rating + purchases, family = binomial, data = train)
md <- glm(status~ calls + rating + purchases, family = binomial, data = train)
summary(md)

summary(md)$coefficients

#result1 <- summary(md)$coefficients[,p.values]
#result1 <- summary(md)$coefficients[,pvalues]
pvalues <- summary(md)$coefficients[,4]
max(pvalues)
result1 <- round(max(pvalues),3)
print(result1)
#0.314


#coef(md)
summary(md)$coefficients[,1]

result2 <- exp(summary(md)$coefficients[,1])[3]
print(round(result2,3))


#### 3번 문제 ####
#predict <- pre
pred <- predict(md, newdata = valid, type = "response")
pred_status <- ifelse(pred>0.5, 1 ,0)
actual <- valid$status
accuracy <- mean(pred_status == actual)
error <- 1 - accuracy
print(accuracy)
result3 <- print(error)

# confusionMatrix 쓰는 방법
#cm <- confusionMatrix(pred, valid$status)
#cm <- confusionMatrix(pred_status, valid$status)
cm <- confusionMatrix(as.factor(pred_status), as.factor(valid$status))

#cm$overall
#cm$overall[Accuracy]
#아주 정확히 보셨어요! 네, 그 오류는 Accuracy를 객체처럼 
#쓰려고 해서 발생한 거예요. R에서는 리스트나 벡터의 이름으로 인덱싱할 때 이름을 따옴표로 묶어야 합니다.

cm$overall["Accuracy"]
accuracy <- cm$overall["Accuracy"]
error <- 1 - accuracy

reulst3 <- error
print(round(result3,3))
#0.417
