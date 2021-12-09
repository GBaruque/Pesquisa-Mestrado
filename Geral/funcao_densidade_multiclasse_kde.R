

densidade_multiclasse_kde = function(dados, classes){
  
  
  n_classes = length(table(classes))
  densidade_cluster = matrix(data = 0, nrow = nrow(dados), ncol = n_classes)
  
  
  #--------------------------------------------------
  #comparando dens. prob. de cada elemento por classe
  #--------------------------------------------------
  
  
  for(i in 1:nrow(dados)){
    
    for(j in 1:n_classes){
      
      obs = dados[i,]
      
      if(classes[i] == j){
        Mat_aval= dados[which(classes==j),]
        Y_aval = which(classes==j)
        indice_pra_tirar = which(Y_aval==i)
        Mat_aval=Mat_aval[-c(indice_pra_tirar),]
        densidade = kde(Mat_aval, eval.points = obs, binned = FALSE)
        densidade_cluster[i,j] = densidade$estimate
      }
      else{
        Mat_aval= dados[which(classes==j),]
        densidade = kde(Mat_aval, eval.points = obs, binned = FALSE)
        densidade_cluster[i,j] = densidade$estimate
      }
    }
  }
  
  
  #--------------------------------------------------
  #   Comparando a probabilidade com a soma
  #   das probabilidades por classe (baseado em bayes)
  #--------------------------------------------------
  
  
  prob_final = rep(0,nrow(dados))
  
  for (i in 1:length(prob_final)) {
    prob_final[i] = densidade_cluster[i, classes[i]]/sum(densidade_cluster[i,])
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
  plot(dados, col = cor, pch=16, cex=1)
  
}
