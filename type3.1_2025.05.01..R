library(dplyr)
rdata <- read.csv("P230605.csv", fileEncoding = "euc-kr")
#str(rdata)
tot_cnt <- nrow(rdata)
#data <- rdata %>% group_by(코드) %>% summarise(건수 = n(), 비율 = 건수()/tot_cnt)
data <- rdata %>% group_by(코드) %>% summarise(건수 = n(), 비율 = 건수/tot_cnt)
str(data)
#### 1번문제 ####
result1 <- round(data$비율[4],3)
print(result1)

#### 2번문제 ####
chisq.test(data$건수, p=c(0.05, 0.1, 0.05, 0.8))
result2 <- chisq.test(data$건수, p=c(0.05, 0.1, 0.05, 0.8))$statistic
print(round(result2,3))


#### 3번 문제 ####
result3 <- chisq.test(data$건수, p=c(0.05, 0.1, 0.05, 0.8))$p.value
round(result3,3)
print(round(result3,3))

