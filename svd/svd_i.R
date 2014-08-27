#!/usr/bin/env Rscript

# 非完全增量学习算法，非稀疏矩阵

r = 4 # 隐式特征数
nr = 6 # 用户数
nc = 4 # 物品数

# 真实值矩阵
inputdata = matrix(c(5,5,0,5,5,0,3,4,3,4,0,3,0,0,5,3,5,4,4,5,5,4,5,5), nrow = nr, ncol = nc, byrow = TRUE)

# 初始化分解矩阵
U = matrix(seq(1,24),nrow=r,ncol=nr)
M = matrix(2,nrow=r,ncol=nc)

# 初始化正则化系数与迭代步长
ku = 0.05
km = 0.05
u = 0.003

iter = 1
repeat
{
  print(paste("---iter ",iter,"---"))
  iter = iter + 1
  for(i in c(1:nr)) {
    # Ui的迭代差值矩阵
    t = matrix(0,nrow=r,ncol=nc)
    for(j in c(1:nc)) {
      t[,j] = (inputdata[i,j] - t(U[,i]) %*% M[,j]) * M[,j] 
    }
    du = rowSums(t) - ku * U[,i] 
    U[,i] = U[,i] + u * du

    # M矩阵的迭代差值矩阵
    dm = matrix(0,nrow=r,ncol=nc)
    for(j in c(1:nc)) {
      t = (inputdata[i,j] - t(U[,i]) %*% M[,j]) * U[,i] 
      dm[,j] = t - km * M[,j] 
    }
    M = M + u * dm
  }


  # 判断迭代结束
  RMSE = sqrt(sum((inputdata - t(U)%*%M)^2)/(nr*nc))
  if(is.infinite(RMSE)) break
  if(abs(RMSE)<0.01) break
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
