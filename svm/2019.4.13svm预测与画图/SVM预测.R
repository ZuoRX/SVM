library(xlsx)
#SVM
library(e1071)
library(SVMMatch)
library(kernlab)
#数据处理
library(magrittr)
library(plyr)
library(dplyr) 
library(data.table)
library(tidyr) #数据转换包
library(splines) #数据差值包
#画图
library(ggplot2)
library(ggthemes)
library(devtools)
#devtools::install_github("rstudio/rmarkdown")

#-------------#
#--表1预测图--#
#-------------#
#setwd("c:/users/lenovo/desktop/svm")
data<-fread("c:/users/lenovo/desktop/svm/table1.csv")

names(data)<-c("code","y","x1","x2","x3","x4","x5")

data1<-data[,-1]%>% na.omit()

sv<-svm(y~.,data=data1,scale = TRUE)
preds <- predict(sv, data1)

all<-data.table(sample=1:207,real=data1$y,pred=preds)
table(preds, all$real)

#偏差值 deviation value
deviation<-abs(all$pred-all$real)/all$real
max(deviation)

#均方误差mean square error (MSE)
mse<-sum(all$pred-all$real)^2/length(all$sample)

#精确度计算
all$label[abs(all$real-all$pred)<=0.03]<-1
all$label[is.na(all$label)]<-0
accuracy<-sum(all$label)/length(all$sample)

#绘制预测值与真实值之间的散点图
ggplot(all, aes(x = real, y =pred)) +
  geom_point() +
  xlab("真实值")+ylab("预测值")+
  scale_x_continuous(breaks=seq(0.5, 1, 0.1))+
  scale_y_continuous(breaks=seq(0.5, 1, 0.1),
                     limits = c(0.45,1))+
  geom_abline(intercept=0,slope=1,color = "red", size = 1)+
  geom_abline(intercept=0.03,slope=1)+
  geom_abline(intercept=-0.03,slope=1)

#预测值与真实值折线图
all1 <- all %>% gather("item",value,-1) %>%
  bind_cols(data.frame(item_id=rep(1:2,207)))

ggplot(all1,aes(x=sample,y=value,colour=item,fill=item))+
  theme_set(theme_few())+ #可删
  xlab("检验样本个数")+
  ylab("Mn元素收得率")+
  geom_line()+
  theme(legend.title=element_blank(), #可删
      axis.line = element_line(colour = "black"))+
  theme_set(theme_few())+ #可删
  theme(legend.title=element_blank(), #可删
        axis.line = element_line(colour = "black"))


# #-------------#
# #--表2预测图--#
# #-------------#
data<-read.csv("c:/users/lenovo/desktop/svm/table2.csv")

names(data)<-c("code","y","x1","x2","x3","x4","x5")

data1<-data[,-1]%>% na.omit()

#标准化
# minmaxscale<-function(a){
#   center <- sweep(a, 2, apply(a, 2, min),'-')
#   R <- apply(a, 2, max) - apply(a,2,min)
#   return(sweep(center, 2, R, "/"))
# }
#
# data2<-ddply(data1,2,minmaxscale)

sv<-svm(y~.,data=data1,scale = TRUE)
preds <- predict(sv, data1)
head(preds,20)

all<-data.table(sample=1:650,real=data1$y,pred=preds)

#偏差值 deviation value
deviation<-abs(all$pred-all$real)/all$real
max(deviation)

#均方误差mean square error (MSE)
mse<-sum(all$pred-all$real)^2/length(all$sample)
print(mse)

#精确度计算
all$label[abs(all$real-all$pred)<0.03]<-1
all$label[is.na(all$label)]<-0
accuracy<-sum(all$label)/length(all$sample)
print(accuracy)

#绘制预测值与真实值之间的散点图
ggplot(all, aes(x = real, y =pred)) +
  geom_point() +
  xlab("真实值")+ylab("预测值")+
  scale_x_continuous(breaks=seq(0.6, 1, 0.1))+
  scale_y_continuous(breaks=seq(0.6, 1, 0.1),
                     limits = c(0.65,1))+
  geom_abline(intercept=0,slope=1,color = "red", size = 1)+
  geom_abline(intercept=0.03,slope=1)+
  geom_abline(intercept=-0.03,slope=1)+
  theme_set(theme_few())+ #可删
  theme(legend.title=element_blank(), #可删
      axis.line = element_line(colour = "black"))

#预测值与真实值折线图
all1 <- all %>% gather("item",value,-1) %>%
  bind_cols(data.frame(item_id=rep(1:2,650)))

ggplot(all1,aes(x=sample,y=value,colour=item,fill=item))+
  theme_set(theme_few())+ #可删
  xlab("检验样本个数")+
  ylab("Mn元素收得率")+
  geom_line()+
  theme(legend.title=element_blank(), #可删
        axis.line = element_line(colour = "black"))





