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
#--------------------------------------------------





#==================================================

#   INICIALIZAÇÃO DOS DADOS ARTIFICIAIS

#==================================================

#--------------------------------------------------
#    Criando distribuição normal (simulada)
#--------------------------------------------------
n<-100

x1 = mvrnorm(n , c(18,10), matrix(c(50,0,0,20), nrow = 2))
x2 = mvrnorm(n , c(10,10), matrix(c(1,0,0,1), nrow = 2))

x= rbind(x1,x2)


y1 = rep(1, n)
y2 = rep(2, n)
y = c(y1, y2)

plot(x, col = c("deeppink", "aquamarine3")[as.factor(y)])


densidade_C1 = rep(0,length(x[,1]))
densidade_C2 = rep(0,length(x[,1]))



#--------------------------------------------------
#comparando dens. prob. de cada elemento por classe
#--------------------------------------------------

i=1
for(i in 1:length(x[,1])){
  
  if(y[i] == 1){
    obs = x[i,]
    Mat_aval= x[which(y==1),]
    Mat_aval=Mat_aval[-c(i),]
    densidade = kde(Mat_aval, eval.points = obs, binned = FALSE)
    densidade_C1[i] = densidade$estimate
    
    
    Mat_aval= x[which(y==2),]
    densidade = kde(Mat_aval, eval.points = obs, binned = FALSE)
    densidade_C2[i] = densidade$estimate
    
  }
  else{
    if(y[i]==2){
      obs = x[i,]
      Mat_aval=x[which(y==2),]
      Mat_aval=Mat_aval[-c(i-length(which(y==1))),] #é necessário estar em ordem: primeiro classe 0, depois classe 1
      densidade = kde(Mat_aval, eval.points = obs, binned = FALSE)
      densidade_C2[i] = densidade$estimate
      
      
      Mat_aval= x[which(y==1),]
      densidade = kde(Mat_aval, eval.points = obs, binned = FALSE)
      densidade_C1[i] = densidade$estimate
      
    }
  }
}


#--------------------------------------------------
#   Comparando a probabilidade com a soma
#   das probabilidades por classe (baseado em bayes)
#--------------------------------------------------


prob_final = rep(0,length(densidade_C1))

i=1
for (i in 1:length(prob_final)) {
  if(y[i] == 1){
    prob_final[i] = densidade_C1[i]/(densidade_C1[i]+densidade_C2[i])
  }
  if(y[i] == 2){
    prob_final[i] = densidade_C2[i]/(densidade_C1[i]+densidade_C2[i])
  }
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

#plot(x, col = c("red","gold","black","blue","green" )[as.factor(cor)])

plot(x, col = cor)




P11 = length(which((prob_final < 0.2) & y == 0))#/length(which(y == 0))
P12 = length(which((prob_final > 0.2) & (prob_final < 0.4) & y == 0))#/length(which(y == 0))
P13 = length(which((prob_final > 0.4) & (prob_final < 0.6) & y == 0))#/length(which(y == 0))
P14 = length(which((prob_final > 0.6) & (prob_final < 0.8) & y == 0))#/length(which(y == 0))
P15 = length(which((prob_final > 0.8) & y == 0))#/length(which(y == 0))

P21 = length(which((prob_final < 0.2) & y == 1))#/length(which(y == 1))
P22 = length(which((prob_final > 0.2) & (prob_final < 0.4) & y == 1))#/length(which(y == 1))
P23 = length(which((prob_final > 0.4) & (prob_final < 0.6) & y == 1))#/length(which(y == 1))
P24 = length(which((prob_final > 0.6) & (prob_final < 0.8) & y == 1))#/length(which(y == 1))
P25 = length(which((prob_final > 0.8) & y == 1))#/length(which(y == 1))

nomes = list(c("classe1","classe2"), c("p<0.2","0.2<p<0.4","0.4<p<0.6","0.6<p<0.8","p>0.8"))

matrix(c(P11,P12,P13,P14,P15,P21,P22,P23,P24,P25), nrow = 2, ncol = 5,byrow = TRUE,dimnames = nomes)
