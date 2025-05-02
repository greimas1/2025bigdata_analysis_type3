#P230605.csv
library(dplyr)
rdata <- read.csv("P230605.csv", fileEncoding = "euc-kr")

#str(rdata)
tot_cnt <- nrow(rdata)
data <- rdata %>% group_by(코드) %>% summarise(건수=n(), 비율 = 건수/tot_cnt)
#str(data)

result1 <- data$비율[4]
print(round(result1, 3))
# 0.787


#chisq.test(data$건수, p=(0.05, 0.1, 0.05, 0.8))
#여기서 p에 확률 벡터를 넣으려고 하셨는데,
#R에서는 소괄호 ( )만으로 벡터를 만들 수 없고, 반드시 c() 함수로 묶어야 합니다. 예를 들어:

chisq.test(data$건수, p=c(0.05, 0.1, 0.05, 0.8))
result2 <- chisq.test(data$건수, p=c(0.05, 0.1, 0.05, 0.8))$statistic
round(result2,3)
print(round(result2,3))
#0.997 


result3 <- chisq.test(data$건수, p=c(0.05, 0.1, 0.05, 0.8))$p.value
round(result3,3) 
print(round(result3,3))
#0.802
