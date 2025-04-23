rdata <- read.csv("P230606.csv")

#str(rdata)

md_lm <- lm(Temperature ~ O3 + Solar + Wind, rdata)

sum_lm <- summary(md_lm)$coefficients

sum_lm["O3", "Estimate"]

result1 <- sum_lm["O3", "Estimate"]

t.test(rdata["Wind"], rdata["Temperature"])

result_ttest <-t.test(rdata["Wind"], rdata["Temperature"])
result_ttest$p.value
round(result_ttest$p.value,3)

