
rm(list=ls())

#############################
# Mixed model for Survival approximation
#############################

setwd('C://Users//WenluluSens//Documents//uthealth//qinchuan')
raw <- read.csv('raw.csv',row.names=1)
y <- raw[rowSums(is.na(raw))==0,]

table(predict(MASS::lda(dead~.,data=y))$class,y$dead)
pROC::roc(predict(MASS::lda(dead~.,data=y))$class,y$dead)
plot(pROC::roc(predict(MASS::lda(dead~.,data=y))$class,y$dead))

bst <- xgboost(data=as(as.matrix(y[,-9]),'CsparseMatrix'),label=y[,9],
               max.depth=2,eta=1,nrounds=2,objective='binary:logistic')
pred <- predict(bst,as(as.matrix(y[,-9]),'CsparseMatrix'))
table(pred>.5,y[,9])
pROC::roc(pred>0.5,y$dead)
plot(pROC::roc(pred>0.5,y$dead))
