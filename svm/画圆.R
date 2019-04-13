rm(list = ls())


#----------#
#画圆代码
#----------#


country<-c("老挝","菲律宾","尼泊尔","孟加拉","阿富汗","新加坡","泰国","蒙古","新西兰","印度尼西亚","印度","澳大利亚","台湾","日本","香港","柬埔寨","马来西亚","巴基斯坦","韩国","斯里兰卡","中国","越南")  
percent<-c(90,81,80,77,75,74,73,72,68,68,68,67,65,63,61,60,59,58,53,51,49,48)  
d<-data.frame(country,percent)  
png("C:/Users/lenovo\\Desktop\\test.png",width = 2048, height = 2048) 

f<-function(name,value){  
  
  xsize=200  
  plot(0, 0,xlab="",ylab="",axes=FALSE,xlim=c(-xsize,xsize),ylim=c(-xsize,xsize))  
  for(i in 1:length(name)){  
    info = name[i]  
    percent = value[i] 
    #？？？？？？？？？？？？下面的：没看懂？？？？？？？？
    k = (1:(360*percent/100)*10)/10          #k是画多大的弧
    r=xsize*(length(name)-i+1)/length(name)  #r是按画圆个数的比例缩小的(相对于xsize=200)
    #print(r)  
    x=r*sin(k/180*pi)                        #k在这里定义了x与y的定义域
    y=r*cos(k/180*pi)  
    text(-18,r,info,pos=2,cex=3)             #添加文本 国家名称
    text(-9,r,paste(percent,"%"),cex=3)      #添加文本 百分比例
    lines(x,y,col="red")                     #定义圆弧的颜色
  }               
}
  

f(country,percent)  
dev.off()  

#------------#
#课本3.3画图
#------------#

dose=c(20,30,40,45,60)
drugA=c(16,20,27,40,60)
drugB=c(15,18,25,31,40)
#type="b" 画点且线
plot(dose,drugA,type="b")

#par函数对图形参数进行修改
#含有当前图形参数设置的列表
oopar=par()
#含有---可以修改的---当前图形参数设置的列表
#readonly只读的
opar=par(no.readonly = TRUE)
par(lty=2,pch=17)
plot(dose,drugA,type="b")

par(font.lab=10)


library(plotrix)
dev.new()
plot(1:5,seq(1,10,length=5),type="n",xlab="",ylab="",main="Test draw.circle")
draw.circle(2,4,c(1,0.66,0.33),border="purple",
            col=c("#ff00ff","#ff77ff","#ffccff"),lty=1,lwd=1)
draw.circle(2.5,8,0.6,border="red",lty=3,lwd=3)
draw.circle(4,3,0.7,border="green",col="yellow",lty=1,
            density=5,angle=30,lwd=10)
draw.circle(3.5,8,0.8,border="blue",lty=2,lwd=2)




a=as.matrix(c(1,2))
class(a)
b=as.matrix(c(4,-2))
a2=a^2
c=a-b
c2=c^2
for (i in 1:nrow(a)){                          
  for (j in 1:ncol(a)){
    r1=max(sqrt(sum((amP[j]-a[i ,j])^2)))
  }
}















