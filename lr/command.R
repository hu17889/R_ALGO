kalythos <- data.frame(x = c(20,35,45,55,70), n = rep(50,5),y = c(6,17,26,37,44))
kalythos
kalythos$Ymat <- cbind(kalythos$y, kalythos$n - kalythos$y)
kalythos
fml <- glm(Ymat ~ x, family = binomial, data = kalythos)
summary(fml)
predict(fml,type="response")
