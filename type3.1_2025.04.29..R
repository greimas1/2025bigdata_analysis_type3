library(dplyr)

rdata <- read.csv("P230605.csv", fileEncoding = "euc-kr")

total <- nrow(rdata)
  
rdata %>% group_by(코드) %>% summarise(건수=n(), 비율=건수/total)

data <- rdata %>% group_by(코드) %>% summarise(건수=n(), 비율=건수/total)

#### 1번 문제 ####
print((round(data$비율[4],3)))

#### 2번 문제 ####

#tb <- table(data$건수, p=data$비율)
#chisq.test(tb)$statistic
#round(chisq.test(tb)$statistic,3)

#코드 = c(1,2,3,4)
#비율 <- c(0.05, 0.1, 0.05, 0.8)
#tb <- table(data$건수, p=c(0.05, 0.1, 0.05, 0.8))
chisq.test(data$건수, p=c(0.05, 0.1, 0.05, 0.8))

chisq.test(data$건수, p=c(0.05, 0.1, 0.05, 0.8))$statistic
round(chisq.test(data$건수, p=c(0.05, 0.1, 0.05, 0.8))$statistic,3)
print(round(chisq.test(data$건수, p=c(0.05, 0.1, 0.05, 0.8))$statistic,3))

#### 3번 문제 ####
#tb <- table(data$건수, p=data$비율)
#chisq.test(tb)$p.value
chisq.test(data$건수, p=c(0.05, 0.1, 0.05, 0.8))$p.value
round(chisq.test(data$건수, p=c(0.05, 0.1, 0.05, 0.8))$p.value,3)
print(round(chisq.test(data$건수, p=c(0.05, 0.1, 0.05, 0.8))$p.value,3))
