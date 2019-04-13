rm(list=ls())
#基本包
library(rJava)
library(xlsx)     #读表
library(reshape)  #修改变量名称
#未知
library(lattice)
library(NLP)
library(SnowballC)
library(mvtnorm)
library(hdrcde)
library(locfit)
library(ash)
library(KernSmooth)
library(misc3d)
library(rgl)
library(ks)
library(sp)
library(grid)
library(vcd)
library(topicmodels)
library(rFerns)
library(ranger)
library(Boruta) 
library(lattice)
library(caret)
library(slam)
library(Matrix)
library(foreach)
library(glmnet)
library(dplyr)   #截取
#画图表
library(plotrix)
library(igraph)
library(ggplot2)
#基本统计信息
library(car)      #拟合和评价回归模型
library(gvlma)
library(leaps)    #全子集回归
library(nnet)
library(caret)
library(pastecs)
#颜色
library(RColorBrewer)
library(rainbow)
#画地图
library(ggmap)
library(maps)
library(mapdata)
library(maptools)
#支持向量机
library(e1071)
library(SVMMatch)
library(kernlab)
#主成分
library(pcaPP)
#文本挖掘
library(tm)
library(Rwordseg)
library(wordcloud)
library(wordcloud2)
#随机森林
library(randomForest)
#s神经网络
library(Rcpp)#与RSNNS关联
library(RSNNS)#涉及到神经网络中的其它拓扑结构和网络模型
#Stuttgart Neural Network Simulator（SNNS）是德国斯图加特大学开发的优秀神经网络仿真软件
library(nnet)#提供了最常见的前馈反向传播神经网络算法
library(AMORE)#提供了更为丰富的控制参数，并可以增加多个隐藏层
library(neuralnet)#提供了弹性反向传播算法和更多的激活函数形式
library(autoencoder)
library(deepnet)#实现了一些Deep Learning结构和Neural Network相关算法，
#包括BP，RBM训练，Deep Belief Net，Deep Auto-Encoder
#聚类分析
library(cluster)
library(MASS)
library(stats)  #hclust,kmeans等函数
library(fpc)
library(amap)

rm(list=ls())
cat("\014")  


#---------------自定义svm函数-----------------------#

mysvm<-function(X,y,cost){
  n <- dim(X)[1]
  Q <- sapply(1:n, function(i) y[i]*t(X)[,i])
  D <- t(Q)%*%Q
  d <- matrix(1, nrow=n)
  uu <- cost   
  eps <- 1e-2    
  b <- 0
  r <- 0
  A2 <- t(y)
  l <- matrix(0, nrow=n, ncol=1)
  u <- matrix(uu, nrow=n, ncol=1)
  capture.output(sol <- ipop(-d, t(Q)%*%Q+eps*diag(n), A2, b, l, u, 
                             r,verb=TRUE, sigf=5, margin=1e-8))
  ipopsol <- primal(sol)
  alpha<- matrix(ipopsol , nrow=n)
# Calculation of the normal vector W and bias term b
  w=t(alpha*y)%*%(X) #W
  ff=matrix(rep(alpha*y,n),n,n)*X%*%t(X)
  fout=matrix(t(apply(ff,2,sum)))
  pos=which(alpha>1e-6)
  b = mean(y[pos]-fout[pos]) #b
  fx=t(w %*% t(as.matrix(X))) + b 
#    SVM line & support vectors
  plot(X,pch=ifelse(y==1, 1, 3),col=ifelse(y==1, 1, 2))
  abline(a=-b/w[1,2], b=-w[1,1]/w[1,2], col="black", lty=1)
  abline(a=-(b+1)/w[1,2], b=-w[1,1]/w[1,2], col="orange", lty=3)
  abline(a=-(b-1)/w[1,2], b=-w[1,1]/w[1,2], col="orange", lty=3)
  points(X[pos,],col="blue",cex=2) # show the support vectors
#    ACCURACY
  pred<-function(x,lable){
    C=NULL
    for (i in 1:length(x)){
      if(x[i]>0){C[i]=1}
      else {C[i]=-1}
    }
    accuracy=mean(C==lable)
    result=list(C,accuracy)
    return(accuracy)
  }
  pred(fx,y)
}

#---------------自定义隶属度函数-----------------------#

Si<-function(x,y,delta){
  Pxbar<-apply(groupP,2,mean)
  Nxbar<-apply(groupN,2,mean)
  S=NULL
  for (i in 1:nrow(x)){
    for (j in 1:ncol(x)){
      if (y[i]==1)                    #求一方的隶属度
      {S[i]=1-sqrt(sum((Pxbar[j]-x[i,j])^2))/(max(sqrt(sum((Pxbar[j]-x[i ,j])^2)))+delta)}
      else                            #求另一组数据的隶属度
      {S[i]=1-sqrt(sum((Nxbar[j]-x[i,j])^2))/(max(sqrt(sum((Nxbar[j]
                                                            - x[i,j])^2)))+delta)}
    }}
  return(S)
}

#---------------输入x,y数据-----------------------#


mysvm(x,y,10*Si(x,y,0.5))








































#---------------#
#导入交叉关系数据
#---------------#

#我选的（硬科技里面的新能源alternative energy）和（高科技里面的新能源与节能energy saving）
#这两个部分存在明显的包含关系
#--------目的----------#
#1.训练fsvm
#2.测验程序中的隶属度函数能够准确反映这种关系
#3.如果能得到一个完全包含的关系图，则选择的两个特征重要

ae<-read.xlsx("C:/Users/lenovo/Desktop/data/fuzzy/ae.xlsx",1,encoding="UTF-8")
sp=c(rep(1,10))                 #这里变量名称是sp
ae=cbind(ae,sp)[,-9]
aees<-read.xlsx("c:/users/lenovo/desktop/data/fuzzy/aees.xlsx",1,encoding="UTF-8")
sp=c(rep(-1,18))                #这里变量名称如果是sp1的话，就会导致下面的组合不起来
aees=cbind(aees,sp)[,-9]
all=rbind(ae,aees)



#-------------# 
#重命名变量名称
#-------------#

names(ae)=c("price","huan","zhenfu","totalMV","circulationMV","DPE","TTM","PBV","species")
names(aees)=c("price","huan","zhenfu","totalMV","circulationMV","DPE","TTM","PBV","species")
names(all)=c("price","huan","zhenfu","totalMV","circulationMV","DPE","TTM","PBV","species")

#--------------#
#进行标准化处理
#--------------#
ae1=scale(as.matrix(ae[,-9]))
ae=cbind(ae1,ae$species)

aees1=scale(as.matrix(aees[,-9]))
aees=cbind(aees1,aees$species)

all1<- scale(as.matrix(all[,-9]))
all=cbind(all1,all$species)


#用all数据做svm,用ae和aees画两个圆#
#-----------------------#
#使用自定义支持向量机运算
#-----------------------#

mysvm<-function(X,y,cost){

#----------------------------------------------------#
# Optimization uing ipop() function of kernlab package
#----------------------------------------------------#
n <- dim(X)[1]

# build the system matrices
#sapply函数的用法
#sapply(数据,运算函数,函数的参数,simplify = TRUE, USE.NAMES = TRUE)
#sapply(s,function(x){
#           if(is.numeric(x)) {mean(x)} else {length(x)}
#                     }
#       ) 


#对任意1到n条数据，都要执行第一步操作
#---------t是个啥东西？
Q <- sapply(1:n, function(i) y[i]*t(X)[,i])

D <- t(Q)%*%Q
d <- matrix(1, nrow=n)
  
  uu <- cost   
  eps <- 1e-2    
  b <- 0
  r <- 0
  A2 <- t(y)
  l <- matrix(0, nrow=n, ncol=1)
  u <- matrix(uu, nrow=n, ncol=1)
  
  capture.output(sol <- ipop(-d, t(Q)%*%Q+eps*diag(n), A2, b, l, u, 
                             r,verb=TRUE, sigf=5, margin=1e-8))
  ipopsol <- primal(sol)
  alpha<- matrix(ipopsol , nrow=n)
  
  #--------------------------------------------------#
  # Calculation of the normal vector W and bias term b
  #--------------------------------------------------#
  w=t(alpha*y)%*%(X) #W
  ff=matrix(rep(alpha*y,n),n,n)*X%*%t(X)
  fout=matrix(t(apply(ff,2,sum)))
  pos=which(alpha>1e-6)
  b = mean(y[pos]-fout[pos]) #b
  fx=t(w %*% t(as.matrix(X))) + b 
  
  #-----------------------------#
  #    SVM line & support vectors
  #-----------------------------#
  
  plot(X,pch=ifelse(y==1, 1, 3),col=ifelse(y==1, 1, 2))
  abline(a=-b/w[1,2], b=-w[1,1]/w[1,2], col="black", lty=1)
  abline(a=-(b+1)/w[1,2], b=-w[1,1]/w[1,2], col="orange", lty=3)
  abline(a=-(b-1)/w[1,2], b=-w[1,1]/w[1,2], col="orange", lty=3)
  points(X[pos,],col="blue",cex=2) # show the support vectors
  #------------------#
  #    ACCURACY
  #------------------#
  
  pred<-function(x,lable){
    C=NULL
    for (i in 1:length(x)){
      if(x[i]>0){C[i]=1}
      else {C[i]=-1}
    }
    accuracy=mean(C==lable)
    result=list(C,accuracy)
    return(accuracy)
  }
  pred(fx,y)
}





#-----------------------------#
#    代入测试样本值
#-----------------------------#

#-------第一组---------#
data12=data.frame(all[,1],all[,2])
names(data12)=c("price","huan")
X=as.matrix(data12)                        #关键在X必须是个矩阵
class(X)
y=as.vector(all[,9])
mysvm(X,y,0.1)                         #cost=0.1
#分步骤求圆的半径#                     #半径还是存在问题！！！！！！
#————————r1------#

a=as.matrix(data.frame(ae[,1],ae[,2]))
#均值版本一
am_<-apply(a,2,mean) 
#均值版本二
#am1=mean(a[,1])              
#print(am)
#均值版本三
aaa=c(1.6478444,0.2393084,-0.4675075,-0.6575886,-0.2244281,-0.6764438,-0.8986296,-0.4583347,-0.4629211,1.9587007)
am1=mean(aaa)
am2=c(-2.22045E-17)
am=c(am1,am2)

for (i in 1:nrow(a)){                          
  for (j in 1:ncol(a)){
    r1=max(sqrt(sum((am-a[i ,j])^2)))
  }
}
#————————r2------#
b=as.matrix(data.frame(aees[,1],aees[,2]))
bm_<-apply(b,2,mean) 
bm=c(-1.66667E-09,1.11111E-09)
for (i in 1:nrow(b)){                          
  for (j in 1:ncol(b)){
    r2=max(sqrt(sum((bm-b[i ,j])^2)))
  }
}

#我自己用Excel把半径求出来，r1=2.428311877，r2=3.384144305




#------第二步用draw.circle 函数画圆
d1=draw.circle(am[1],am[2],r1,nv=1000,border = "red")
#再添加另外一个圆
lines(d2){d2=draw.circle(bm[1],bm[2],r2,border = "yellow")}






















#------------------#
#       DATA
#------------------#

set.seed(44)#44
#20是需要多少条数据
#c(rep(-2,2))的结果是两个均值，rep是把-2重复两次
#diag是把c（1,1）取对角阵，整个是协方差矩阵
groupN=mvrnorm(20,c(rep(-2,2)),diag(c(rep(1,2))))
#以下是自己不懂上述含义时，测试的方法
#print(diag(c(rep(1,2))))
#zzz=var(groupN)
#print(zzz)
groupP=mvrnorm(20,c(rep(2,2)),diag(c(rep(1,2))))
X=rbind(groupN,groupP)
#注意此处rep的区别
y<-rep(c(-1,1),each=20)
a1=c(0,0);a2=c(0,-1);a3=c(-1,-1);a4=c(1,0)      #?????????这波没搞明白
X=rbind(X,a1,a2,a3,a4)
y=c(y,1,1,1,-1)
mysvm(X,y,10)


#--------------#
#我要画出两个圆
#--------------#

#——————第一步圆的半径--------#

#分步骤求圆的半径#
#————————r1------#
a=groupP
amP<-apply(a,2,mean)          
for (i in 1:nrow(a)){                          
  for (j in 1:ncol(a)){
    r1=max(sqrt(sum((amP[j]-a[i ,j])^2)))
  }
}

#————————r2------#
a=groupN
amN<-apply(a,2,mean)          
for (i in 1:nrow(a)){                          
  for (j in 1:ncol(a)){
    r2=max(sqrt(sum((amN[j]-a[i ,j])^2)))
  }
}


#---自己编的函数，运行结果是NULL————#

#只需要求一个圆的半径
rr<-function(a){                      #a应该是个数组
  am<-apply(a,2,mean)          
 for (i in 1:nrow(a)){                          
  for (j in 1:ncol(a)){
                r1=max(sqrt(sum((am[j]-a[i ,j])^2)))
  }
 }
}    
#实战运用自己编写的函数
r1=rr(groupN)
print(r1)


#------第二步用draw.circle 函数画圆
d1=draw.circle(amN[1],amN[2],r1,nv=1000,border = "red")
#再添加另外一个圆
lines(d2){
  d2=draw.circle(amP[1],amP[2],r2,border = "yellow")
}










#------------------#
#    FUZZY SVM
#------------------#

Si<-function(x,y,delta){
  Pxbar<-apply(groupP,2,mean)
  Nxbar<-apply(groupN,2,mean)
  S=NULL
  for (i in 1:nrow(x)){
    for (j in 1:ncol(x)){
      if (y[i]==1)                    #求一方的隶属度
      {S[i]=1-sqrt(sum((Pxbar[j]-x[i,j])^2))/(max(sqrt(sum((Pxbar[j]-x[i ,j])^2)))+delta)}
      else                            #求另一组数据的隶属度
      {S[i]=1-sqrt(sum((Nxbar[j]-x[i,j])^2))/(max(sqrt(sum((Nxbar[j]
                                                            - x[i,j])^2)))+delta)}
    }}
  return(S)
}





mysvm(x,y,10*Si(x,y,0.5))

#The calculated accuracy for classical SVM is 0.9318182.
# The following pictures are respectively the classical and fuzzified SVM. enter image description hereenter image description here










