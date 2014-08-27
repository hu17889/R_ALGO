#!/usr/bin/env Rscript

library('methods')
library('Matrix')

startTime = Sys.time()

# 批量学习算法，稀疏矩阵，矢量化编程

r = 4 # 隐式特征数
nr = 6 # 用户数
nc = 4 # 物品数

# 真实值矩阵
inputdata = Matrix(c(5,5,NA,5,5,NA,3,4,3,4,NA,3,NA,NA,5,3,5,4,4,5,5,4,5,5), nrow = nr, ncol = nc, byrow = TRUE,sparse=TRUE)
print(inputdata);

# 初始化分解矩阵
#U = matrix(seq(1,24),nrow=r,ncol=nr)
U = matrix(2,nrow=r,ncol=nr)
#U = matrix(rnorm(r*nr),r,nr)
M = matrix(2,nrow=r,ncol=nc)

# 初始化正则化系数与迭代步长
ku = 0.05
km = 0.05
u = 0.003

iter = 1
RMSE = LAST_RMSE = 999
repeat
{
  print(paste("---iter ",iter,"---"))
  iter = iter + 1
  # U矩阵的迭代差值矩阵
  du = matrix(0,nrow=r,ncol=nr)
  for(i in c(1:nr)) {
    t = M %*% diag(inputdata[i,] - as(t(M) %*% U[,i],"numeric"))
    index = which(is.na(t))
    t[index] = 0
    du[,i] = rowSums(t) - ku * U[,i] 
  }

  # M矩阵的迭代差值矩阵
  dm = matrix(0,nrow=r,ncol=nc)
  for(j in c(1:nc)) {
    t = U %*% diag(inputdata[,j] - as(t(U) %*% M[,j],"numeric"))
    index = which(is.na(t))
    t[index] = 0
    dm[,j] = rowSums(t) - km * M[,j] 
  }


  # 迭代修正
  U = U + u * du
  M = M + u * dm
  # 判断迭代结束
  LAST_RMSE = RMSE
  index = which(!is.na(inputdata))
  RMSE = sqrt(sum((inputdata[index] - (t(U)%*%M)[index])^2)/(nr*nc))
  if(is.infinite(RMSE)) break
  if(abs(RMSE)<0.01) break
  if(RMSE>LAST_RMSE) break
  print("du:")
  print(du)
  print("dm:")
  print(dm)
  print("U:")
  print(U)
  print("M:")
  print(M)
  print("U*M:")
  print(t(U)%*%M)
  print(paste("RMSE:",RMSE))
}
consumetime = Sys.time() - startTime
print(paste("consume time:",consumetime))
