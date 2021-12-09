rm(list=ls())


#--------------------------------------------------
#             Pacotes necessários
#--------------------------------------------------
#carregando pacote MASS
install.packages("MASS")
require("MASS")
install.packages("class")
require("class")
install.packages("openxlsx")
require("openxlsx")
install.packages("ks")
require("ks")
require("mvtnorm")
#--------------------------------------------------





#==================================================

#   INICIALIZAÇÃO DOS DADOS ARTIFICIAIS

#==================================================

#--------------------------------------------------
#    Criando distribuição normal (simulada)
#--------------------------------------------------
n<-100

x1 = mvrnorm(n , c(16,-2), matrix(c(1,0,2,18), nrow = 2))
x2 = mvrnorm(n , c(0,-2), matrix(c(1,0,2,18), nrow = 2))
x3 = mvrnorm(n , c(7,7), matrix(c(18,1,6,1), nrow = 2))
x4 = mvrnorm(n , c(7,-2), matrix(c(1,0,2,18), nrow = 2))
x5 = mvrnorm(n , c(8,-15), matrix(c(1,0,2,18), nrow = 2))

x= rbind(x1,x2,x3,x4,x5)


y1 = rep(1, n)
y2 = rep(2, n)
y3 = rep(3, n)
y4 = rep(4, n)
y5 = rep(5, n)
y = c(y1, y2, y3, y4,y5)

plot(x, col = c("blue","red","green","magenta","black")[as.factor(y)], pch=16, cex=1)

n_classes = length(table(y))
densidade_cluster = matrix(data = 0, nrow = length(x[,1]), ncol = n_classes)


#--------------------------------------------------
#comparando dens. prob. de cada elemento por classe
#--------------------------------------------------



for(i in 1:length(x[,1])){
  
  for(j in 1:n_classes){
    
    obs = x[i,]
    
    if(y[i] == j){
      Mat_aval= x[which(y==j),]
      Y_aval = which(y==j)
      indice_pra_tirar = which(Y_aval==i)
      Mat_aval=Mat_aval[-c(indice_pra_tirar),]
      densidade = kde(Mat_aval, eval.points = obs, binned = FALSE)
      densidade_cluster[i,j] = densidade$estimate
    }
    else{
      Mat_aval= x[which(y==j),]
      densidade = kde(Mat_aval, eval.points = obs, binned = FALSE)
      densidade_cluster[i,j] = densidade$estimate
    }
  }
}


#--------------------------------------------------
#   Comparando a probabilidade com a soma
#   das probabilidades por classe (baseado em bayes)
#--------------------------------------------------


prob_final = rep(0,length(x[,1]))

for (i in 1:length(prob_final)) {
  
  prob_final[i] = densidade_cluster[i, y[i]]/sum(densidade_cluster[i,])
  
}



#métodos de "classificação" (cores)
i = 1
cor = rep(0,length(prob_final))
while (i <= length(prob_final))
{
  if(prob_final[i] <= 0.20) {
    cor[i] = "red"
  } else {
    if((prob_final[i] > 0.20)&(prob_final[i] <= 0.4)) {
        cor[i] = "gold"
    } else {
      if((prob_final[i] > 0.40)&(prob_final[i] <= 0.60)) {
        cor[i] = "black"
      } else {
        if((prob_final[i] > 0.60)&(prob_final[i] <= 0.80)) {
          cor[i] = "blue"
        } else {
          cor[i] = "green"
          }
        }
      }
    }

  i=i+1
}


#plot(x, col = c("blue","red","green","magenta","black")[as.factor(y)], pch=1, cex=2)
plot(x, col = cor, pch=16, cex=1)




# P11 = length(which((prob_final < 0.2) & y == 0))#/length(which(y == 0))
# P12 = length(which((prob_final > 0.2) & (prob_final < 0.4) & y == 0))#/length(which(y == 0))
# P13 = length(which((prob_final > 0.4) & (prob_final < 0.6) & y == 0))#/length(which(y == 0))
# P14 = length(which((prob_final > 0.6) & (prob_final < 0.8) & y == 0))#/length(which(y == 0))
# P15 = length(which((prob_final > 0.8) & y == 0))#/length(which(y == 0))
# 
# P21 = length(which((prob_final < 0.2) & y == 1))#/length(which(y == 1))
# P22 = length(which((prob_final > 0.2) & (prob_final < 0.4) & y == 1))#/length(which(y == 1))
# P23 = length(which((prob_final > 0.4) & (prob_final < 0.6) & y == 1))#/length(which(y == 1))
# P24 = length(which((prob_final > 0.6) & (prob_final < 0.8) & y == 1))#/length(which(y == 1))
# P25 = length(which((prob_final > 0.8) & y == 1))#/length(which(y == 1))
# 
# nomes = list(c("classe1","classe2"), c("p<0.2","0.2<p<0.4","0.4<p<0.6","0.6<p<0.8","p>0.8"))
# 
# matrix(c(P11,P12,P13,P14,P15,P21,P22,P23,P24,P25), nrow = 2, ncol = 5,byrow = TRUE,dimnames = nomes)
