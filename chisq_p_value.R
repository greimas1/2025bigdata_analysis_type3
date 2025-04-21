library(dplyr)

#rdata <- read.csv("P230605.csv", header = TRUE)


# 열 이름을 R에서 쓸 수 있는 형태로 변환
#names(rdata) <- make.names(names(rdata), unique = TRUE)

rdata <- read.csv("P230605.csv", header = TRUE, fileEncoding = "euc-kr")

#str(rdata)

tot_cnt <- nrow(rdata)
rdata_placebo <- rdata %>% group_by(코드) %>% 
  summarise(건수 = n(), 비율 = n()/tot_cnt)

result1 <- round(rdata_placebo$비율[4],3)
print(result1)


rdata_rate <- data.frame(코드 =c(1,2,3,4), 비율 = c(0.05, 0.1, 0.05, 0.8))

result_chisq <- chisq.test(rdata_placebo$건수, p=rdata_rate$비율)
print(result_chisq)


result2 <- round(result_chisq$statistic,3)
print(result2)

result3 <- round(result_chisq$p.value,3)
print(result3)
