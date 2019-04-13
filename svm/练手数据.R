rm(list=ls())
library(e1071)
library(lattice)
library(SVMMatch)
library(NLP)
library(tm)
library(rJava)
library(SnowballC)
library(Rwordseg)
library(MASS)
library(RColorBrewer)
library(wordcloud)
library(pcaPP)
library(rainbow)
library(Rcpp)
library(cluster)
library(mvtnorm)
library(hdrcde)
library(locfit)
library(ash)
library(KernSmooth)
library(misc3d)
library(rgl)
library(ks)
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(sp)
library(maptools)
library(grid)
library(vcd)
library(topicmodels)
library(randomForest)
library(rFerns)
library(ranger)
library(Boruta) 
library(lattice)
library(caret)
library(slam)
library(Matrix)
library(foreach)
library(glmnet)
library(xlsx)
library(igraph)
library(wordcloud2)
library(reshape)
library(kernlab)


#---------#
#ipop练手
#---------#

## solve the Support Vector Machine optimization problem
data(spam)

## sample a scaled part (500 points) of the spam data set
m <- 500
set <- sample(1:dim(spam)[1],m)
x <- scale(as.matrix(spam[,-58]))[set,]
y <- as.integer(spam[set,58])
y[y==2] <- -1

##set C parameter and kernel
C <- 5
rbf <- rbfdot(sigma = 0.1)

## create H matrix etc.
H <- kernelPol(rbf,x,,y)
c <- matrix(rep(-1,m))
A <- t(y)
b <- 0
l <- matrix(rep(0,m))
u <- matrix(rep(C,m))
r <- 0

sv <- ipop(c,H,A,b,l,u,r)
sv
dual(sv)

#-----------#
#mvrnorm练手
#-----------#

Sigma <- matrix(c(10,3,3,2),2,2)
Sigma
zr=var(mvrnorm(n = 1000000, rep(0, 2), Sigma))
zx=var(mvrnorm(n = 1000, rep(0, 2), Sigma, empirical = TRUE))
zz=mvrnorm(n = 1000, rep(0, 2), Sigma, empirical = TRUE)
print(zr)














