
rm(list=ls())
install.packages("MASS")
require(MASS)
install.packages("class")
require("class")


dimR=2
mu=rep(0,dimR)

##Ex.:2: (70%-30%)

x12 =  mvrnorm(700 , mu, diag(dimR))
x22 = mvrnorm(300 ,mu, diag(dimR))

x_treino= rbind(x12,x22)


y12 = rep(0, 700)
y22 = rep(1, 300)
y_treino = c(y12, y22)
plot(x12)
plot(x22)


plot(x_treino, col = c("blue", "red")[as.factor(y_treino)])


x12_teste =  mvrnorm(700 , mu, diag(dimR))
x22_teste = mvrnorm(300 ,mu, diag(dimR))

x_teste= rbind(x12_teste,x22_teste)

y_teste = y_treino

plot(x_teste, col = c("blue", "red")[as.factor(y_teste)])



resposta1 = knn(x_treino, x_teste, y_treino, k = 1, l = 0, prob = TRUE, use.all = TRUE)
prop.table(table(resposta1==y_teste))

resposta3 = knn(x_treino, x_teste, y_treino, k = 3, l = 0, prob = FALSE, use.all = TRUE)
prop.table(table(resposta3==y_teste))

resposta5 = knn(x_treino, x_teste, y_treino, k = 5, l = 0, prob = FALSE, use.all = TRUE)
prop.table(table(resposta5==y_teste))

resposta7 = knn(x_treino, x_teste, y_treino, k = 7, l = 0, prob = FALSE, use.all = TRUE)
prop.table(table(resposta7==y_teste))

resposta9 = knn(x_treino, x_teste, y_treino, k = 9, l = 0, prob = FALSE, use.all = TRUE)
prop.table(table(resposta9==y_teste))

resposta17 = knn(x_treino, x_teste, y_treino, k = 17, l = 0, prob = FALSE, use.all = TRUE)
prop.table(table(resposta17==y_teste))

