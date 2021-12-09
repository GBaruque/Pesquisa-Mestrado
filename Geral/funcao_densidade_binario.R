densidade_binario = function (dados, classes){
  
  densidade_C1 = rep(0,nrow(dados))
  densidade_C2 = rep(0,nrow(dados))
  
  #--------------------------------------------------
  #comparando dens. prob. de cada elemento por classe
  #--------------------------------------------------
  
  i=1
  for(i in 1:length(dados[,1])){
    
    if(classes[i] == 1){
      obs = dados[i,]
      Mat_aval= dados[which(classes==1),]
      Mat_aval=Mat_aval[-c(i),]
      densidade = kde(Mat_aval, eval.points = obs, binned = FALSE)
      densidade_C1[i] = densidade$estimate
      
      
      Mat_aval= dados[which(classes==2),]
      densidade = kde(Mat_aval, eval.points = obs, binned = FALSE)
      densidade_C2[i] = densidade$estimate
      
    }
    else{
      if(classes[i]==2){
        obs = dados[i,]
        Mat_aval=dados[which(classes==2),]
        Mat_aval=Mat_aval[-c(i-length(which(classes==1))),] #é necessário estar em ordem: primeiro classe 0, depois classe 1
        densidade = kde(Mat_aval, eval.points = obs, binned = FALSE)
        densidade_C2[i] = densidade$estimate
        
        
        Mat_aval= dados[which(classes==1),]
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
    if(classes[i] == 1){
      prob_final[i] = densidade_C1[i]/(densidade_C1[i]+densidade_C2[i])
    }
    if(classes[i] == 2){
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
  
  #plot(dados, col = c("red","gold","black","blue","green" )[as.factor(cor)])
  plot(dados, col = cor)
  

  
}
