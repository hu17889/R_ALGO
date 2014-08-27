#!/usr/bin/env Rscript

# 导入数据
library("methods")
library("Rcpp")
library("RSNNS")
data(iris)

# 随机
iris = iris[sample(nrow(iris)),]
# 抽取输入数据
irisValues= iris[,1:4]
# 定义输出标签
irisTargets = decodeClassLabels(iris[,5])
#从中划分出训练样本和检验样本 
iris = splitForTrainingAndTest(irisValues, irisTargets, ratio=0.15)
#数据标准化 
iris = normTrainingAndTestSet(iris)
#利用mlp命令执行前馈反向传播神经网络算法 
model = mlp(iris$inputsTrain, iris$targetsTrain, size=5, learnFunc="Quickprop", learnFuncParams=c(0.1, 2.0, 0.0001, 0.1),maxit=100, inputsTest=iris$inputsTest, targetsTest=iris$targetsTest) 
#利用上面建立的模型进行预测 
predictions = predict(model,iris$inputsTest)
print(predictions)
#生成混淆矩阵，观察预测精度 
confusionMatrix(iris$targetsTest,predictions)

