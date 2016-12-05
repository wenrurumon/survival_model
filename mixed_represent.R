
rm(list=ls())

#############################
# Mixed model for Survival approximation
#############################

setwd('C://Users//WenluluSens//Documents//uthealth//qinchuan')
raw <- read.csv('raw.csv',row.names=1)
y <- raw[rowSums(is.na(raw))==0,]

library(nlme)
x.lme <- lme(time~TS+TP+POLH+KI67+DUT+RRM1+RRM2+RRM2B+
               age+sex+tumor_grade+tumor_stage,random= ~1|chemo/dead,data=y)
summary(x.lme)
plot(y$time);lines(fit <- predict(x.lme),col=2)
cor(fit,y$time)

#############################
# Classification Setup
#############################

res <- as.numeric(y$time - fit)
y2 <- ifelse(res>0,y$chemo,1-y$chemo)
x2 <- as.matrix(dplyr::select(y,-time,-chemo))

#LDA
library(MASS)
y2.lda <- lda(y2 ~ x2)
table(predict(y2.lda)$class,y2)

#TREE
library(rpart)
y2.rpart <- rpart(y2 ~ x2)
table(predict(y2.rpart)>.5,y2)

#Logistic
y2.logistic <- glm(y2 ~ x2, family = binomial())
table(predict(y2.logistic)>0,y2)

#ROC
library(pROC)
par(mfrow=c(1,3))
plot(roc(predict(y2.lda)$class,y2))
plot(roc(predict(y2.rpart),y2))
plot(roc(predict(y2.logistic),y2))
