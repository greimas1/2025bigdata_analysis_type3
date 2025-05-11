library(dplyr)
rdata <- read.csv("customer.csv")
#str(rdata)
#summary(rdata)
rdata$status <- as.factor(rdata$status)

idx <- sample(1:nrow(rdata), nrow(rdata)*0.7)
train <- rdata[idx,]
test <- rdata[-idx,]

#md <- glm(status _ calls + rating + purchases, family = "binomial", data = train)
md <- glm(status ~ calls + rating + purchases, family = "binomial", data = train)
summary(md)
summary(md)$coefficients[-1,4]
result1 <- max(summary(md)$coefficients[-1,4])
print(round(result1,3))
#0.332


result2 <- summary(md)$coefficients["rating","Estimate"]
print(round(exp(result2),3))
#1.016

pred <- predict(md, newdata = test, type = "response")
str(pred)
pred_status <- ifelse(pred>0.5,1,0)
acc <- mean(pred_status == test$status)
error <- 1-acc
print(round(error,3))
#0.408


--------------------------------------------------------------------
  library(dplyr)
rdata <- read.csv("customer.csv")
#str(rdata)
#summary(rdata)
rdata$status <- as.factor(rdata$status)

idx <- sample(1:nrow(rdata), nrow(rdata)*0.7)
train <- rdata[idx,]
test <- rdata[-idx,]

#md <- glm(status _ calls + rating + purchases, family = "binomial", data = train)
md <- glm(status ~ calls + rating + purchases, family = "binomial", data = train)
summary(md)
summary(md)$coefficients[-1,4]
result1 <- max(summary(md)$coefficients[-1,4])
print(round(result1,3))
#0.332


result2 <- summary(md)$coefficients["rating","Estimate"]
print(round(exp(result2),3))
#1.016

pred <- predict(md, newdata = test, type = "response")
str(pred)
pred_status <- ifelse(pred>0.5,1,0)
pred_status <- as.factor(pred_status)
acc <- mean(pred_status == test$status)
error <- 1-acc
print(round(error,3))
#0.408

----------------------------------------------------------
  library(dplyr)
rdata <- read.csv("customer.csv")
#str(rdata)
#summary(rdata)
rdata$status <- as.factor(rdata$status)

idx <- sample(1:nrow(rdata), nrow(rdata)*0.7)
train <- rdata[idx,]
test <- rdata[-idx,]

#md <- glm(status _ calls + rating + purchases, family = "binomial", data = train)
md <- glm(status ~ calls + rating + purchases, family = "binomial", data = train)
summary(md)
summary(md)$coefficients[-1,4]
result1 <- max(summary(md)$coefficients[-1,4])
print(round(result1,3))
#0.332


result2 <- summary(md)$coefficients["rating","Estimate"]
print(round(exp(result2),3))
#1.016

pred <- predict(md, newdata = test, type = "response")
str(pred)
pred_status <- factor(ifelse(pred>0.5,1,0), levels=c(0,1))
#pred_status <- as.factor(pred_status)
acc <- mean(pred_status == test$status)
error <- 1-acc
print(round(error,3))
#0.408