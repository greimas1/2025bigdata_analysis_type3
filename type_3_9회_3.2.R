library(dplyr)
rdata <- read.csv("airquality.csv")
#str(rdata)
data <- na.omit(rdata)
#str(data)
#summary(data)
md <- lm(Temp ~ Ozone + Solar.R + Wind, data = data)
summary(md)
#summary(md)[-1,4]
summary(md)$coefficients[-1,4]
result1 <- sum(summary(md)$coefficients[-1,4] < 0.05)
print(result1)
#1


md2 <- lm(Temp ~ Ozone + Wind, data = data)
cor_md2 <- cor(data$Temp, data$Ozone, method = "pearson")
result2 <- round(cor_md2,3)
print(result2)
#0.699


idx <- sample(1:nrow(data), nrow(data)*0.7)
train <- data[idx,]
test <- data[-idx,]
md3 <- lm(Temp~Ozone + Wind, data = train)
pred3 <- predict(md3, newdata = test)

mse <- mean((pred3-test$Temp)^2)
#rmse <- root(mse)
rmse <- sqrt(mse)
result3 <- round(rmse,3)
print(result3)
#6.63
