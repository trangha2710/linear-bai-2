install.packages("psych")
install.packages("minpack.lm")
library(ggplot2)
library(dplyr)
library(tidyr)
library(haven)
library(psych)

#import data
df <- read_dta("LFS_5_Simple.dta")
View(df)
summary(df)
#phan tich moi lien he tuong quan
sum(is.na(df$c38))
sum(is.na(df$wage))
cov(df$c38, df$wage, method = "pearson", use ="na.or.complete")
cor(df$c38, df$wage, method ="pearson", use = "na.or.complete")

#mo hinh hoi quy
model <- lm(wage~c38, data = df)
summary(model)
plot(df$c38,df$wage,xlab="c38",ylab = "wage",col = "blue")
abline(model, col = "Red")
#bien doi logarit
log_X <- log(df$c38)
log_Y <- log(df$wage)
model1 <- lm(log_Y~log_X)
summary(model1)
plot(log_X,log_Y,xlab = "log_X",ylab = "log_Y",col = "blue")
abline(model1, col = "Red")
