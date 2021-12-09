#Parzen Multidimensional- Gaussian Density Estimate - Fç do R

#2 pacotes podem ser usados: mvtnorm ou emdbook - ambos possuem a função dmvnorm


GDE_fc = function(dados){ #verificar outros parametros necessários
  
  require("mvtnorm")
  
  Rn = ncol(dados)
  n_dados =nrow(dados)
  
  GDE = rep(0,n_dados)
  
  
  GDE = dmvnorm(dados, mean=rep(0, Rn), sigma=diag(Rn), log=FALSE)
  
  
  return(GDE)
  
}


#####teste de plotagem

#  i=1
#  cor = rep(0,n_dados)
#  while (i <= n_dados)
#  {
#    if(GDE_norm[i] <= 0.20) {
#      cor[i] = "red"
#    } else {
#      if((GDE_norm[i] > 0.20)&(GDE_norm[i] <= 0.4)) {
#        cor[i] = "gold"
#      } else {
#        if((GDE_norm[i] > 0.40)&(GDE_norm[i] <= 0.60)) {
#          cor[i] = "black"
#        } else {
#          if((GDE_norm[i] > 0.60)&(GDE_norm[i] <= 0.80)) {
#            cor[i] = "blue"
#          } else {
#            cor[i] = "green"
#          }
#        }
#      }
#    }
#   
#    i=i+1
#  }
#  
#  
# # #plot(x, col = c("blue","red","green","magenta","black")[as.factor(y)], pch=1, cex=2)
#  plot(dados, col = cor, pch=16, cex=1)




