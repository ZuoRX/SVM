mysvm<-function(X,y,cost){
library(kernlab)
#----------------------------------------------------#
# Optimization uing ipop() function of kernlab package
#----------------------------------------------------#
n <- dim(X)[1]
# build the system matrices
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
#------------------#
#       DATA
#------------------#

library(MASS)
set.seed(44)#44

groupN=mvrnorm(20,c(rep(-2,2)),diag(c(rep(1,2))))
groupP=mvrnorm(20,c(rep(2,2)),diag(c(rep(1,2))))
X=rbind(groupN,groupP)
y<-rep(c(-1,1),each=20)
a1=c(0,0);a2=c(0,-1);a3=c(-1,-1);a4=c(1,0)
X=rbind(X,a1,a2,a3,a4)
y=c(y,1,1,1,-1)
mysvm(X,y,10)
The calculated accuracy for classical SVM is 0.9545455. I have computed the membership function according to this codes,

#------------------#
#    FUZZY SVM
#------------------#

Si<-function(x,y,delta){
Pxbar<-apply(groupP,2,mean)
Nxbar<-apply(groupN,2,mean)
S=NULL
 for (i in 1:nrow(x)){
 for (j in 1:ncol(x)){
if (y[i]==1)
{S[i]=1-sqrt(sum((Pxbar[j]-x[i,j])^2))/(max(sqrt(sum((Pxbar[j]-x[i ,j])^2)
))+delta)}
else
{S[i]=1-sqrt(sum((Nxbar[j]-x[i,j])^2))/(max(sqrt(sum((Nxbar[j]
- x[i,j])^2)))+delta)}
}}
return(S)
}
mysvm(X,y,10*Si(X,y,0.5))

#The calculated accuracy for classical SVM is 0.9318182.
# The following pictures are respectively the classical and fuzzified SVM. enter image description hereenter image description here

r svm outliers accuracy fuzzy