


densidade_multiclasse_gde_bayes = function (dados,classes){
  
  n_classes = length(table(classes))
  densidade_cluster_GDE = matrix(data = 0, nrow = length(dados[,1]), ncol = n_classes)
 
  
  #--------------------------------------------------
  #comparando dens. prob. de cada elemento por classe
  #--------------------------------------------------
  
  # Construindo vetor de probabilidades a priori
  P = matrix(data = 0, ncol = n_classes)
  for(j in 1:n_classes){
    P[j] = length(which(classes==j))/nrow(dados)
  }
  
  
  for(i in 1:nrow(dados)){
    
    for(j in 1:n_classes){
      
      obs = dados[i,]
      
      if(classes[i] == j){
        Mat_aval= dados[which(classes==j),]
        Y_aval = which(classes==j)
        indice_pra_tirar = which(Y_aval==i)
        Mat_aval=Mat_aval[-c(indice_pra_tirar),]
        densidade_cluster_GDE[i,j] = P[j]*GDE_manual(obs, Mat_aval)
      }
      else{
        Mat_aval= dados[which(classes==j),]
        densidade_cluster_GDE[i,j] = P[j]*GDE_manual(obs, Mat_aval)
      }
    }
  }
  
  
  #--------------------------------------------------
  #   Comparando a probabilidade com a soma
  #   das probabilidades por classe (baseado em bayes)
  #--------------------------------------------------
  
  
  prob_final_GDE = rep(0,nrow(dados))
  
  
  
  for (i in 1:length(prob_final_GDE)) {
    
    prob_final_GDE[i] = densidade_cluster_GDE[i, classes[i]]/sum(densidade_cluster_GDE[i,])
    
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
  plot(dados, col = cor, pch=16, cex=1)
  
}
