rm(list = ls())

#Load dataset

library(dplyr)
library(tidyverse)

df <- read.csv("datasetfinalenergy.csv")

glimpse(df)

vars_to_convert <- c('BA_climate','SWIMPOOL_',
                           'INCOME','TELLWORK','TYPEHUQ_','DISHWASH','CWASHER',
                           'ELWARM','ELCOOL','ELWATER','REGIONC')
df <- df %>% mutate_at(vars(vars_to_convert), as.factor)

glimpse(df)

# Exploratory data analysis
par(mar=c(2,2,3,2), cex.lab=1.2, cex.axis=1.2, cex.main=1.2)
layout.matrix <- matrix(c(1,1,2,3,4,5,6,7), nrow = 2, ncol = 4)
layout(mat = layout.matrix,
       heights = c(1, 1), # Heights of the two rows
       widths = c(1, 1)) 
plot(density(log(df$DOLLAREL)), main="log(Total electricity cost)")
lines(density(log(df$DOLLAREL))$x, dnorm(density(log(df$DOLLAREL))$x,mean(log(df$DOLLAREL)),sd(log(df$DOLLAREL))),col=2)

plot(density(df$TOTSQFT_EN), main="TOTSQFT_EN")
lines(density(df$TOTSQFT_EN)$x, dnorm(density(df$TOTSQFT_EN)$x,mean(df$TOTSQFT_EN),sd(df$TOTSQFT_EN)),col=2)

barplot(table(df$BEDROOMS), main="BEDROOMS")
barplot(table(df$NHSLDMEM), main="NHSLDMEM")
barplot(table(df$TVCOLOR), main="TVCOLOR")
barplot(table(df$CWASHER), main="CWASHER")
barplot(table(df$DISHWASH), main="DISHWASH")


par(mfrow=c(2,4), mar=c(2,2,2,3), cex.lab=1.3, cex.main=1.3, cex.axis=1.3)
barplot(table(df$ELCOOL), main="ELCOOL")
barplot(table(df$ELWARM), main="ELWARM")
barplot(table(df$ELWATER), main="ELWATER")
barplot(table(df$INCOME), main="Annual Income")
barplot(table(df$REGIONC), main="Region")
barplot(table(df$SWIMPOOL), main="SWIMPOOL")
barplot(table(df$BA_climate), main="Climate")
barplot(table(df$TYPEHUQ), main="TYPEHUQ")

#figure 2
par(mfrow=c(4,3),mar=c(4,4,3,2), cex.lab=1.1, cex.axis=1.1, cex.main=0.4)
plot(df$BEDROOMS, df$DOLLAREL, xlab="BEDROOMS", ylab = "DOLLAREL")
plot(lm(log(DOLLAREL)~BEDROOMS, data = df), 1, caption="log(DOLLAREL)~BEDROOMS")
plot(lm(log(DOLLAREL)~poly(BEDROOMS,2), data=df), 1, caption="log(DOLLAREL)~poly(BEDROOMS,2)")
plot(df$TVCOLOR, df$DOLLAREL,  xlab="TVCOLOR", ylab = "DOLLAREL")
plot(lm(log(DOLLAREL)~TVCOLOR, data = df), 1, caption="log(DOLLAREL)~TVCOLOR")
plot(lm(log(DOLLAREL)~poly(TVCOLOR,2), data = df), 1, caption="log(DOLLAREL)~poly(TVCOLOR,2)")
plot(df$NHSLDMEM, df$DOLLAREL,  xlab="NHSLDMEM", ylab = "DOLLAREL")
plot(lm(log(DOLLAREL)~NHSLDMEM, data = df), 1, caption="log(DOLLAREL)~NHSLDMEM")
plot(lm(log(DOLLAREL)~poly(NHSLDMEM,2), data = df), 1, caption="log(DOLLAREL)~poly(NHSLDMEM,2)")
plot(df$TOTSQFT_EN, df$DOLLAREL,  xlab="TOTSQFT", ylab = "DOLLAREL")
plot(lm(log(DOLLAREL)~TOTSQFT_EN, data = df), 1, caption="log(DOLLAREL)~TOTSQFT")
plot(lm(log(DOLLAREL)~poly(TOTSQFT_EN,2), data = df), 1, caption="log(DOLLAREL)~poly(TOTSQFT,2)")



par(mfcol=c(3,2),mar=c(6,6,1,1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8)
boxplot(log(DOLLAREL)~BA_climate, data = df)
boxplot(log(DOLLAREL)~SWIMPOOL_, data = df)
boxplot(log(DOLLAREL)~INCOME, data = df)
boxplot(log(DOLLAREL)~REGIONC, data = df)
boxplot(log(DOLLAREL)~TYPEHUQ_, data = df)
boxplot(log(DOLLAREL)~DISHWASH, data = df)

par(mfcol=c(2,2),mar=c(6,6,1,1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8)
boxplot(log(DOLLAREL)~CWASHER, data = df)
boxplot(log(DOLLAREL)~ELWARM, data = df)
boxplot(log(DOLLAREL)~ELCOOL, data = df)
boxplot(log(DOLLAREL)~ELWATER, data = df)

#Split data train - test set
set.seed(1)
n <- nrow(df)
train <- sample(1:n, ceiling(n*0.5))
test <- (1:n)[-train]
data_train <- df[train,]
data.test <- df[test,]

#MODEL FORMULA
formula2 <- 'log(DOLLAREL) ~ poly(BEDROOMS,2) + poly(TVCOLOR,2) + poly(NHSLDMEM,2) + poly(TOTSQFT_EN,2) + SWIMPOOL_  + INCOME  + TYPEHUQ_ + DISHWASH + CWASHER  +
ELWARM + ELCOOL + ELWATER + REGIONC + BA_climate'

#OLS FIT
fit.lr   <- lm(formula2, data = df, subset=train)
summary(fit.lr)

#figure 4
par(mfcol=c(1,2),mar=c(4,4,3,2), pty="s", cex.lab=1.2, cex.axis=1.2, cex.main=1.2)
plot(fit.lr, 1)
plot(fit.lr, 2)


coef.lr<- as.data.frame(summary(fit.lr)$coefficients[, c('Estimate','Std. Error')])
names.lr<- rownames(coef.lr)
estimates.lr<- data.frame(Variable=names.lr,coef.lr)


#QUANTILE REGRESSION FIT
la       <- 0.025
ua       <- 0.975

library(quantreg)
fit.qrla <- rq(formula2, tau = la, data = df, subset=train)
summary(fit.qrla)

coef.qrla<- data.frame(summary(fit.qrla)$coefficients[, c('Value','Std. Error')])
names.qrla<- rownames(coef.qrla)
estimates.qrla<- data.frame(Variable=names.qrla,coef.qrla)


fit.qrua <-  rq(formula2 , tau = ua, data = df, subset=train)
summary(fit.qrua)

coef.qrua<- data.frame(summary(fit.qrua)$coefficients[, c('Value','Std. Error')])
names.qrua<- rownames(coef.qrua)
estimates.qrua<- data.frame(Variable=names.qrua,coef.qrua)

#Export table with estimates and standard errors
export <- estimates.lr %>% left_join(estimates.qrla, by="Variable") %>%
  left_join(estimates.qrua, by="Variable")

#write_csv2(export, "estimates.csv")

# Out-of-sample predictions
qla.lr <- predict(fit.lr, data.test) + sigma(fit.lr)*qnorm(la)
qua.lr <- predict(fit.lr, data.test) + sigma(fit.lr)*qnorm(ua)
qla.qr <- predict(fit.qrla, data.test)
qua.qr <- predict(fit.qrua, data.test)


# Coverage evaluation of models
i.qla.lr <- 1*(data.test$DOLLAREL < exp(qla.lr))
i.qua.lr <- 1*(data.test$DOLLAREL > exp(qua.lr))
i.qla.qr <- 1*(data.test$DOLLAREL < exp(qla.qr))
i.qua.qr <- 1*(data.test$DOLLAREL > exp(qua.qr))

#number of violations
v.qla.lr <- sum(i.qla.lr)
v.qua.lr <- sum(i.qua.lr)
v.qla.qr <- sum(i.qla.qr)
v.qua.qr <- sum(i.qua.qr)

uc.qla.lr <- mean(i.qla.lr)
uc.qua.lr <- mean(i.qua.lr)
uc.qla.qr <- mean(i.qla.qr)
uc.qua.qr <- mean(i.qua.qr)

#p-values likelihood test
pv.qla.lr <- 1-pchisq(-2*(sum((i.qla.lr)*log(la)+(1-i.qla.lr)*log(ua))-sum((i.qla.lr)*log(uc.qla.lr)+(1-i.qla.lr)*log(1-uc.qla.lr))), 1)
pv.qua.lr <- 1-pchisq(-2*(sum((i.qua.lr)*log(la)+(1-i.qua.lr)*log(ua))-sum((i.qua.lr)*log(uc.qua.lr)+(1-i.qua.lr)*log(1-uc.qua.lr))), 1)
pv.qla.qr <- 1-pchisq(-2*(sum((i.qla.qr)*log(la)+(1-i.qla.qr)*log(ua))-sum((i.qla.qr)*log(uc.qla.qr)+(1-i.qla.qr)*log(1-uc.qla.qr))), 1)
pv.qua.qr <- 1-pchisq(-2*(sum((i.qua.qr)*log(la)+(1-i.qua.qr)*log(ua))-sum((i.qua.qr)*log(uc.qua.qr)+(1-i.qua.qr)*log(1-uc.qua.qr))), 1)

#Export table with likelihood test results
test <- matrix (rep(NA,8), nrow = 2,  dimnames = list(c("V","p-value"),c("LR_q_0.025", "LR_q_0.975","QR_q_0.025","QR_q_0.975")))
test[1,1] <- v.qla.lr
test[1,2] <- v.qua.lr
test[1,3] <- v.qla.qr
test[1,4] <- v.qua.qr

test[2,1] <- pv.qla.lr
test[2,2] <- pv.qua.lr
test[2,3] <- pv.qla.qr
test[2,4] <- pv.qua.qr

test <- test %>% as.tibble()

write_csv2(test, "likelihoodtest.csv")
nrow(data.test) * 0.025 #number of violations expected

