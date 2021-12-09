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
n<-500

x1 = mvrnorm(n , c(0,0), matrix(c(10,0,0,10), nrow = 2))
x2 = mvrnorm(n , c(10,10), matrix(c(10,0,0,10), nrow = 2))
x= rbind(x1,x2)

y1 = rep(0, n)
y2 = rep(1, n)
y = c(y1, y2)


plot(x, col = c("blue", "green")[as.factor(y)])

#--------------------------------------------------

#--------------------------------------------------
#    estimando probabilidade global
#--------------------------------------------------

densidade_global = kde(x, eval.points = x, binned = FALSE)



densidade_global$estimate
x_com_densidade_global = cbind(x,densidade_global$estimate)
x_ordenado_global = order(x_com_densidade_global[,3]) #ordena os indices em ordem crescente com base na densidade

# Comentando o while original

#métodos de "classificação" (cores)
 i = 1
 cor = rep(0,length(x_ordenado_global))
 while (i <= length(x_ordenado_global))
 {
  if(i <= 0.25 * length(x_ordenado_global))
  {
    cor[x_ordenado_global[i]] = 1
  }
  else{if(i >= 0.75 * length(x_ordenado_global)){

      cor[x_ordenado_global[i]] = 3

    }
    else{
      cor[x_ordenado_global[i]] = 2
    }
  }

  i=i+1
 }

# colocando x_ordenado_global[i] no lugar de i dentro do while
 i = 1
 cor = rep(0,length(x_ordenado_global))
 while (i <= length(x_ordenado_global))
 {  
   if(x_ordenado_global[i] <= 0.25 * length(x_ordenado_global))
   {
     cor[x_ordenado_global[i]] = 1
   }
   else{if(x_ordenado_global[i] >= 0.75 * length(x_ordenado_global)){
     
     cor[x_ordenado_global[i]] = 3
     
   }
     else{
       cor[x_ordenado_global[i]] = 2
     }
   }
   
   i=i+1
 }
 
 head(cor)
 head(x_ordenado_global)
 ## acho que isso pode ficar no lugar do while:
 cor2 = rep(0,length(x_ordenado_global))
 percentil_25= quantile(x_ordenado_global, 0.25)
 percentil_75= quantile(x_ordenado_global, 0.75)
 cor2[which(x_ordenado_global<=percentil_25)] = 1
 cor2[which(x_ordenado_global>percentil_25 & x_ordenado_global<percentil_75 )] = 2
 cor2[which(x_ordenado_global>=percentil_75)] = 3
 head(cor)
 head(cor2)
 head(x_ordenado_global)
 
plot(x, col = c("red","black","green")[as.factor(cor2)])
#plot(x, col = cor)


