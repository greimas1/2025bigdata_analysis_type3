library(dplyr)
library(Hmisc)
rdata <- read.csv("eduhealth.csv", fileEncoding="euc-kr")
#str(rdata)
data <- na.omit(rdata)
#summary(data)
data$성별 <- as.integer(factor(data$성별, levels=c("남","여")))
#str(data)
cor_matrix <- cor(data[,c("학년","키","몸무게","성별")])
#cor_matrix[lower.tri(cor_matrix)]
max_cor <- max(abs(cor_matrix[lower.tri(cor_matrix)]))
result1 <- round(max_cor,3)
print(result1)
# 0.849



data_df <- data.frame(data$학년, data$키, data$몸무게, data$성별)
r_cor_matrix <- rcorr(as.matrix(data_df), type="pearson")

#r_cor_matrix$r[lower.tri(r_cor_matrix$r, diag=FALSE)]

max_r_cor<- max(abs(r_cor_matrix$r[lower.tri(r_cor_matrix$r, diag=FALSE)]))

result2 <- round(max_r_cor,3)
print(result2)
#0.849