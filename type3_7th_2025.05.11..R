library(dplyr)
library(caret)
library(pROC)
rdata <- read.csv("eduhealth.csv", fileEncoding = "euc-kr")
#str(rdata)
rdata$성별 <- as.integer(factor(rdata$성별, levels=c("남","여")))

#str(rdata)
#summary(rdata)

data <- na.omit(rdata)
#summary(data)

data2 <- data[,c("학년","키","몸무게","성별")]
str(data2)

cor_mat <- cor(data2)
#max_cor_mat <- max(cor_mat(lower.tri(cor_mat)))
max_cor_mat <- max(cor_mat[lower.tri(cor_mat)])
result1 <- round(max_cor_mat,3)
print(result1)
#0.849


md1 <- glm(몸무게~키, data = data)
#md1.deviance
pseudo_r2 <- 1-(md1$deviance/md1$null.deviance)
result2 <- round(pseudo_r2,3)
print(result2)
#0.721


#md1$coefficients["키","Estimate"]
result3 <- round(md1$coefficients["키"],3)
print(result3)
#0.854

result4 <- round(exp(md1$coefficients["키"]),3)
print(result4)
#2.35


md2 <- glm(성별~키+몸무게, data=data)
summary(md2)$coefficients[-1,4]
pval<-summary(md2)$coefficients[-1,4]
result5 <- max(pval)
print(result5)
# 1.550106e-06



idx <- sample(1:nrow(data), nrow(data)*0.7)
train <- data[idx,]
test <- data[-idx,]

md3 <- glm(성별~키+몸무게, data=train)
pred3 <- predict(md3, newdata = test)

tb <- table(test$성별,round(pred3,0))
confusionMatrix(tb)

confusionMatrix(tb)$overall

acc <- confusionMatrix(tb)$overall["Accuracy"]
err <- 1-acc
result6 <- round(err,3)
print(result6)
#0.47


#plot.roc(test$성별,round(pred3,0))
plot.roc(test$성별,round(pred3,0), legacy.axes=TRUE)

