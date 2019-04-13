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
#数据加工厂
library(plyr)
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

#--------------------------去除了交叉项数据-----------------------------------------#


#-----------------------#
#导入硬科技与高科技数据
#-----------------------#

#少1表就读不出来，少encoding="UTF-8"就乱码
hard<-read.xlsx("C:/Users/lenovo/Desktop/data/t1/hard.xlsx",1,encoding="UTF-8")
high<-read.xlsx("c:/users/lenovo/desktop/data/t1/high.xlsx",1,encoding="UTF-8")
#all里面只是数据的简单加总，有重复部分
#all<-read.xlsx("C:/Users/lenovo/Desktop/data/t1/hard.xlsx",2,encoding="UTF-8")
#去除了高科技和硬科技交叉的20条数据
whole<-read.xlsx("C:/Users/lenovo/Desktop/data/t1/hard.xlsx",3,encoding="UTF-8")


names(whole)=c("price","huan","zhenfu","totalMV","circulationMV","DPE","TTM","PBV","species")


#-------------------------------#
#用e1071包中的svm函数进行分类处理
#-------------------------------#

#提取数据中除第9列之外的数据作为特征变量
x=whole[,-9]
#将species作为结果变量
y=whole[,9]
#如果特征向量是向量则gamma值取l，否则gamma值为特征向量个数的倒数
svm1=svm(x,y,kernel="radial",gamma=if(is.vector(x)) 1 else 1 / ncol(x))
summary(svm1)
print(svm1)



#plot(svm1,whole,price~huan)           #逗号还不能是全字符



#-----------------------------#
#用kernlab包中的lssvm函数处理
#-----------------------------#

#下面lssvm是kernlab包的函数，可以自动估计核函数参数
svm11=lssvm(species~.,data=whole)
summary(svm11)
print(svm11)
pred=predict(svm11,x)
table(pred,y)
print(svm11)
#--------------------#
#进行相应的预测和判别
#--------------------#

#用样本数据建立模型后，要进行相应的预测和判别
pred=predict(svm1,x)
#用table（）对预测结果和真实结果做出对比展示
table(pred,y)
print(svm1)

#函数predict()中的一个可选参数是decision.values
#pred1=predict(svm11,x,decision.values = TRUE)
#attr(pred1,"decision.values")


#---------#
#图形可视化
#---------#

plot(cmdscale(dist(whole[,-9])),
     col=c("orange","blue")[as.integer(whole[,9])],
     pch=c("o","+")[1:150 %in% svm1$index+1]
)
#画图例
legend( "topright",
        1.8,-0.8, 
        c("hard","high"),
        col = c("orange","blue"),
        lty = 1
)




#--------------------------保留交叉项数据-----------------------------------------#
rm(list = ls())
#all里面只是数据的简单加总，有重复的20条数据，属于两类
all<-read.xlsx("C:/Users/lenovo/Desktop/data/t1/hard.xlsx",2,encoding="UTF-8")
names(all)=c("price","huan","zhenfu","totalMV","circulationMV","DPE","TTM","PBV","species")

#提取数据中除第9列之外的数据作为特征变量
x=all[,-9]
#将species作为结果变量
y=all[,9]
#下面lssvm是kernlab包的函数，可以自动估计核函数参数
svm22=lssvm(species~.,data=all)
summary(svm22)
print(svm22)
pred=predict(svm22,x)

table(pred,y)
print(pred)




#num=function(x){
#  for (i in 1:nrow(x)){
#       if(x[i,1]!=x[i,2])
#         return(i)
#   }
# }
# aa=data.frame(pred,all[,9])
# aaa=num(aa)
# aaa                     多行注释ctrl+shift+c

#--------挑选出预测错误的编号---------#
num=function(x,y){
  k=rep(0,156)
  for (i in 1:156){
    if(x[i]!=y[i])
      k[i]=i
  }
  return(k)
}
aaa=num(pred,all[,9])
aaa
#------计算正确和错误个数---------#

n=which(aaa!=0)
data.frame(n)
length(n)


#------------画图demo------------------#
## a simple example
data(cats, package = "MASS")
m <- svm(Sex~., data = cats)
plot(m, cats)

## more than two variables: fix 2 dimensions
data(iris)
names(iris)
m2 <- svm(species~., data = iris)
plot(m2, iris, petal.wid ~ petal.len,
     slice = list(sepal.wid = 3, sepal.len = 4))

## plot with custom symbols and colors
plot(m, cats, svSymbol = 1, dataSymbol = 2, symbolPalette = rainbow(4),
     color.palette = terrain.colors)


data("mtcars")
attach(mtcars)
slice(mtcars, 1L)
slice(mtcars, n())      #切割数据
slice(mtcars, 5:n())




#-----------------------------最初的尝试------------------------------------#

#-------------#
#重命名变量名称
#-------------#

#数据导出的时候，指标全是中文，所以进行重命名处理
names(whole)
#下面的批量操作出现循环问题；首项也出问题？
#names(whole)[1-4]=c("price","huan","zhenfu","totalMV")
#不加角标就可以了，添加角标出现循环问题
names(whole)=c("price","huan","zhenfu","totalMV","circulationMV","DPE","TTM","PBV","species")
names(hard)=c("price","huan","zhenfu","totalMV","circulationMV","DPE","TTM","PBV","species")
names(high)=c("price","huan","zhenfu","totalMV","circulationMV","DPE","TTM","PBV","species")
#names(whole)[1]="price"



#----替换出错----#
x=whole$species
x=as.matrix(data.frame(x))
switch(x,"hard"=1)
#-----------------#
#把种类替换成1和-1
#-----------------#
for(i in 1:nrow(whole)){
  for(j in 1:ncol(whole)){
    switch(whole[i,j],
           "hard"<-1,
           "high"<--1)
  }
}


#---------------#
#进行主成分分析
#---------------#

#能够反映硬科技的主要成分指标。第9个变量是species，字符型变量，不是数值型变量，要剔除
pca1 <- prcomp(hard[-9],scale. = TRUE)
summary(pca1)#选取的时候用累计贡献率
#能够反映高科技的主成分
pca2<-prcomp(high[-9],scale. = TRUE)
summary(pca2)
pca3<-prcomp(whole[-9],scale. = TRUE)
summary(pca3)
# 把表格放在桌面上
#write.xlsx(hard,file="C:/Users/lenovo/Desktop/b.xlsx",row.names = F)

#--------------#
#进行标准化处理
#--------------#

#-----最简单方法----#
#将数据转化成矩阵，然后进行标准化处理，一步到位
whole1<- scale(as.matrix(whole[,-9]))
whole=cbind(whole1,whole$species)

#-----笨方法-------#
#出现缺漏值会导致不能进行数值化处理
whole$price=scale(whole$price)
whole$huan=scale(whole$huan)
whole$zhenfu=scale(whole$zhenfu)
whole$totalMV=scale(whole$totalMV)
whole$DPE=scale(whole$DPE)
whole$PBV=scale(whole$PBV)
whole$TTM=scale(whole$TTM)
whole$circulationMV=scale(whole$circulationMV)

#-----步骤编函数方法----#
myscale=function(p){
  m=mean(p)
  s=sd(p)
  wp=(p-m)/s 
}
#代入数据
p=whole$price
#myscale相当于已经变成了一个函数，而非function
aa=myscale(p)
  

#---------------------#
#画出变量间的两两关系图
#---------------------#

#价格和其他指标
#不进行标准化处理也可以画图
png("C:/Users/lenovo/Desktop/test3.png",width = 2048, height = 2048) 
opar=par(no.readonly = TRUE)
par(mfrow=c(2,2))
xyplot(whole$price ~ whole$huan, data = whole, groups = species,auto.key=list(corner=c(1,0)))  
xyplot(whole$price ~ whole$zhenfu, data = whole, groups = species,auto.key=list(corner=c(1,0)))
xyplot(whole$price ~ whole$totalMV, data = whole, groups = species,auto.key=list(corner=c(1,0)))
xyplot(whole$price ~ whole$circulationMV, data = whole, groups = species,auto.key=list(corner=c(1,0)))
xyplot(whole$price ~ whole$DPE, data = whole, groups = species,auto.key=list(corner=c(1,0)))
xyplot(whole$price ~ whole$TTM, data = whole, groups = species,auto.key=list(corner=c(1,0)))
xyplot(whole$price ~ whole$PBV, data = whole, groups = species,auto.key=list(corner=c(1,0)))
#换手率和其他指标
xyplot(whole$huan ~ whole$zhenfu, data = whole, groups = species,auto.key=list(corner=c(1,0)))
xyplot(whole$huan ~ whole$totalMV, data = whole, groups = species,auto.key=list(corner=c(1,0)))
xyplot(whole$huan ~ whole$circulationMV, data = whole, groups = species,auto.key=list(corner=c(1,0)))
xyplot(whole$huan ~ whole$DPE, data = whole, groups = species,auto.key=list(corner=c(1,0)))
xyplot(whole$huan ~ whole$TTM, data = whole, groups = species,auto.key=list(corner=c(1,0)))
xyplot(whole$huan ~ whole$PBV, data = whole, groups = species,auto.key=list(corner=c(1,0)))
#振幅和其他
xyplot(whole$zhenfu ~ whole$totalMV, data = whole, groups = species,auto.key=list(corner=c(1,0)))
xyplot(whole$zhenfu ~ whole$circulationMV, data = whole, groups = species,auto.key=list(corner=c(1,0)))
xyplot(whole$zhenfu ~ whole$DPE, data = whole, groups = species,auto.key=list(corner=c(1,0)))
xyplot(whole$zhenfu ~ whole$TTM, data = whole, groups = species,auto.key=list(corner=c(1,0)))
xyplot(whole$zhenfu ~ whole$PBV, data = whole, groups = species,auto.key=list(corner=c(1,0)))
#总市值
xyplot(whole$totalMV ~ whole$circulationMV, data = whole, groups = species,auto.key=list(corner=c(1,0)))
xyplot(whole$totalMV ~ whole$DPE, data = whole, groups = species,auto.key=list(corner=c(1,0)))
xyplot(whole$totalMV ~ whole$TTM, data = whole, groups = species,auto.key=list(corner=c(1,0)))
xyplot(whole$totalMV ~ whole$PBV, data = whole, groups = species,auto.key=list(corner=c(1,0)))
#流通市值
xyplot(whole$circulationMV ~ whole$DPE, data = whole, groups = species,auto.key=list(corner=c(1,0)))
xyplot(whole$circulationMV ~ whole$TTM, data = whole, groups = species,auto.key=list(corner=c(1,0)))
xyplot(whole$circulationMV ~ whole$PBV, data = whole, groups = species,auto.key=list(corner=c(1,0)))
#动态市盈率
xyplot(whole$DPE ~ whole$TTM, data = whole, groups = species,auto.key=list(corner=c(1,0)))
xyplot(whole$DPE ~ whole$PBV, data = whole, groups = species,auto.key=list(corner=c(1,0)))
#市盈率
xyplot(whole$TTM ~ whole$PBV, data = whole, groups = species,auto.key=list(corner=c(1,0)))
par(opar)















#-----------------------------R自带样本数据------------------------------------#
data(cats, package="MASS")
inputData <- data.frame(cats[, c (2,3)], response = as.factor(cats$Sex)) # response as factor
svmfit<-svm(response~.,data=inputData,kernel="linear",cost=10,scale=FALSE)
print(svmfit)
plot(svmfit,inputData)
compareTable<-table(inputData$response,predict(svmfit))  #tabulate
mean(inputData$response!=predict(svmfit))#19.44% misclassification error


data(iris)#sepal 花萼    petal花瓣
attach(iris)#versicolor ['və:sikʌlə]
xyplot(Petal.Length ~ Petal.Width, data = iris, groups = Species,  auto.key=list(corner=c(1,0))) 
subdata=iris[iris$Species!='virginica',]
model1=svm(Species~Petal.Length+Petal.Width,data=subdata)
plot(model1, subdata, Petal.Length ~ Petal.Width) 
summary(model1)

#control+L清空控制台console
#type表示是用于分类还是回归，还是检测，取决于y是否是一个因子。

