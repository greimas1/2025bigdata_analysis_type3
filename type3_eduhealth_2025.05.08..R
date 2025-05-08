library(psych)
library(dplyr)
library(Hmisc)
rdata <- read.csv("eduhealth.csv", fileEncoding="euc-kr")
#str(rdata)
#describe(rdata)
data <- rdata %>% na.omit(rdata)
#summary(data)
data <- data[,c("학년","키","몸무게","성별")]
#str(data)
#data$성별<- as.factor(data$성별)

#data$성별 <- as.numeric(data$성별)
#예를 들어, levels(data$성별)이 남자, 여자라면,
#남자 → 1, 여자 → 2 로 변환됩니다.
#(이 순서는 알파벳 또는 한글 순서에 따라 결정됨)


#data$성별 <- ifelse(data$성별 == "남자", 1, 0)  # 남자=1, 여자=0
#library(dplyr)
#data <- data %>%
  #mutate(성별 = case_when(
    #성별 == "남자" ~ 1,
    #성별 == "여자" ~ 0,
    #TRUE ~ NA_real_))

#levels(data$성별)

data$성별<- as.numeric(factor(data$성별, levels=c("남","여")))
#str(data)
cormatrix <- cor(data)
cormatrix

maxcor <- max(abs(cormatrix[lower.tri(cormatrix)]))
#maxcor1 <- max(abs(cormatrix))

result1 <- round(maxcor,3)
print(result1)
#0.849


#install.packages("Hmisc")
#library(Hmisc)
#typeof(data)
df <- as.data.frame(data)
#typeof(df)
cormatrix2 <- rcorr(as.matrix(df), type="pearson")

#maxcor2 <- max(abs(cormatrix2$r[lower.tri(cormatrix2)]))
maxcor2 <- max(abs(cormatrix2$r[lower.tri(cormatrix2$r)]))

result2 <- round(maxcor2,3)
print(result2)
#0.849



md <- glm(몸무게~키, data=data)
summary(md)
#md$deviance/md$null.deviance
rsq <- 1-(md$deviance/md$null.deviance)

result3 <- round(rsq,3)
print(result3)
#0.721

#summary(md)$coefficeints

#summary(md)$coefficients
#summary(md)$coefficients["키"]
summary(md)$coefficients["키","Estimate"]

result4 <- round(exp(summary(md)$coefficients["키","Estimate"]),3)
print(result4)
#2.35 오즈비

md2 <- glm(성별~키 + 몸무게, data=data)
#summary(md2)$coefficients
summary(md2)$coefficients[,4]
pvalues <- summary(md2)$coefficients[,4]
result5 <- round(max(pvalues),3)
print(result5)
#0

print(max(pvalues))
#1.550106e-06


idx <- sample(1:nrow(data), nrow(data)*0.7)
train <- data[idx,]
test <- data[-idx,]
md3 <- glm(성별~키 + 몸무게, data=train)

