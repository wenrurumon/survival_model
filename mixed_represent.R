
rm(list=ls())

#############################
#Macro

pca <- function(X,ifscale=T){
  if(ifscale) {X <- scale(as.matrix(X))}
  m = nrow(X)
  n = ncol(X)
  X = scale(X)
  Xeigen <- svd(as.matrix(X))
  value <- (Xeigen$d)^2/m
  value <- cumsum(value/sum(value))
  score <- X %*% Xeigen$v
  mat <- Xeigen$v
  list(score=score,prop=value,mat=mat)
}
 ###############

setwd('C://Users//WenluluSens//Documents//uthealth//qinchuan')
raw <- read.csv('raw.csv',row.names=1)
y <- raw[rowSums(is.na(raw))==0,]

#############################

library(nlme)
x.lme <- lme(time~.,random=~1|dead,data=y)

