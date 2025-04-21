rdata <- read.csv("P230606.csv")
#str(rdata)

md_lm <- lm(Temperature~O3 + Solar + Wind, rdata)

sum_lm <- summary(md_lm)$coefficients

result1 <- round(sum_lm["O3", "Estimate"],3)
print(result1)

result_ttest<-t.test(rdata["Wind"], rdata["Temperature"])

round(result_ttest$p.value,3)


rdata_oneday <- data.frame(10,90,20)
colnames(rdata_oneday) <- c("O3", "Solar", "Wind")

result3 <- predict(md_lm, newdata = rdata_oneday)
print(round(result3,3))
