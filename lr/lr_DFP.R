#!/usr/bin/env Rscript

library('methods')
library('Matrix')

startTime = Sys.time()

# 批量学习算法，稀疏矩阵，矢量化编程，梯度下降

#nr = 5 # 样本数
#nc = 2 # 特征数

# 样本矩阵
#sampledata = Matrix(c(20,35,45,55,70,1,1,1,1,1), nrow = nr, ncol = nc, byrow = FALSE,sparse=TRUE)
#print(sampledata);

# 真实值 第一列为 
#realdata = Matrix(c(6,17,26,37,44,44,33,24,13,6), nrow = nr, ncol = 2, byrow = FALSE,sparse=TRUE)
#print(realdata);

file = (read.csv("test_data.csv",sep="\t",header=FALSE))
file = as.matrix(file)
nr = nrow(file) # 样本数
sampledata = cbind(file[,1:2],rep(1,nr))

realdata = apply(as.matrix(file[,3]),1, function(x) {if(x==1) c(1,0) else c(0,1)})
realdata = t(realdata)
nc = ncol(sampledata) # 特征数

print(sampledata)
print(realdata)

fml = glm(realdata ~ sampledata[,1]+sampledata[,2],family = binomial)
summary(fml)
predict(fml,type="response")
#return(0)

# 初始化权重向量
#U = matrix(seq(1,24),nrow=r,ncol=nr)
#U = matrix(rnorm(r*nr),r,nr)
#S = rnorm(nc)
#S = c(1,-2,14)
S = runif(3,min=-10,max=10)

# 初始化正则化系数与迭代步长
ks = 0.00001

# 

iter = 1
RMSE = LAST_RMSE = 999
ds = lastds = rep(0,length(S))
H = diag(length(S))
diffS = rep(1,length(S))
repeat
{
  print(paste("---iter ",iter,"---"))
  iter = iter + 1

  lastds = ds

  # S迭代差值向量
  ds = (realdata[,1] - (realdata[,1]+realdata[,2]) * sampledata %*% S) 
  ds = apply(sampledata,2,function(x,y) x*y,y=ds)
  ds = colSums(ds)
  
#ds = as.numeric(lapply(ds,colSums)) # 稀疏矩阵

  # H迭代
  diffDS = ds - lastds
  H = H + (diffS %*% t(diffS))/as.numeric(t(diffS) %*% diffDS) - (H %*% diffDS %*% t(diffDS) %*% H)/as.numeric(t(diffDS) %*% H %*% diffDS)



  # 迭代修正
  S = S - H %*% ds
  diffS = H %*% ds

  LAST_RMSE = RMSE
#index = which(!is.na(inputdata))
#RMSE = sqrt(sum((inputdata[index] - (t(U)%*%M)[index])^2)/(nr*nc))
  RMSE = sqrt(sum((realdata[,1] - (realdata[,1]+realdata[,2])*apply(sampledata,1,function(a,lrs) 1/(1+exp(-(a%*%lrs))),lrs=S))^2)/(nr))
  print("ds:")
  print(ds)
  print("H:")
  print(H)
  print("diffS:")
  print(diffS)
  print("S:")
  print(S)
  print("predict:")
  print(apply(sampledata,1,function(a,lrs) 1/(1+exp(-(a%*%lrs))),lrs=S))
  print(paste("RMSE:",RMSE))

  # 判断迭代结束
  if(is.infinite(RMSE)) break
  if(abs(RMSE)<0.01) break
if(RMSE>LAST_RMSE) break
}
consumetime = Sys.time() - startTime
print(paste("consume time:",consumetime))
