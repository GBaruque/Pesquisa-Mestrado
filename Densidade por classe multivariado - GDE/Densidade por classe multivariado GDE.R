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

# x1 = mvrnorm(n , c(0,0), matrix(c(1,0,0,10), nrow = 2))
# x2 = mvrnorm(n , c(10,10), matrix(c(10,0,0,1), nrow = 2))
# x= rbind(x1,x2)
# 
# y1 = rep(1, n)
# y2 = rep(2, n)
# y = c(y1, y2)

plot(x, col = c("blue","red","green","magenta","black")[as.factor(y)], pch=16, cex=1)


n_classes = length(table(y))

densidade_cluster_GDE = matrix(data = 0, nrow = length(x[,1]), ncol = n_classes)


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
      densidade_cluster_GDE[i,j] = GDE_manual(obs, Mat_aval)
      
    }
    else{
      Mat_aval= x[which(y==j),]
      densidade_cluster_GDE[i,j] = GDE_manual(obs, Mat_aval)
    }
  }
}


#--------------------------------------------------
#   Comparando a probabilidade com a soma
#   das probabilidades por classe (baseado em bayes)
#--------------------------------------------------


prob_final_GDE = rep(0,length(x[,1]))

for (i in 1:length(prob_final_GDE)) {
  
  prob_final_GDE[i] = densidade_cluster_GDE[i, y[i]]/sum(densidade_cluster_GDE[i,])
  
}



#métodos de "classificação" (cores)
i = 1
cor = rep(0,length(prob_final_GDE))
while (i <= length(prob_final_GDE))
{
  if(prob_final_GDE[i] <= 0.20) {
    cor[i] = "red"
  } else {
    if((prob_final_GDE[i] > 0.20)&(prob_final_GDE[i] <= 0.4)) {
      cor[i] = "gold"
    } else {
      if((prob_final_GDE[i] > 0.40)&(prob_final_GDE[i] <= 0.60)) {
        cor[i] = "black"
      } else {
        if((prob_final_GDE[i] > 0.60)&(prob_final_GDE[i] <= 0.80)) {
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




