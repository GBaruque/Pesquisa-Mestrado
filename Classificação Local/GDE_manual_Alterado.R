#dados: matriz com dados pertinentes a uma classe específica
#obs: observação a ser avaliada com relação aos dados

GDE_manual_Alterado = function(obs, dados){
  
  Rn = ncol(dados)
  n_dados =nrow(dados)
  
  variancia = matrix(0, nrow = 1, ncol = dim(dados)[2])
  for (i in 1:dim(dados)[2]){
    
    variancia[i] = var(dados[,i])
  }
  
  matriz = diag(as.vector(variancia))
  
  GDE = 0
  #GDE_norm = 0
  
  somatorio = 0;
  x0 = obs;
  
  for(j in 1:n_dados){
    
    xj = matrix(dados[j,], nrow = 1);
    somatorio = somatorio + dmvnorm(x0 - xj, mean=rep(0, Rn), sigma=matriz, log=FALSE)
    
  }
  GDE = (1/(n_dados))*somatorio;
  
  
  # for (i in 1:n_dados){
  #   GDE_norm[i] = (GDE[i]-min(GDE))/(max(GDE)-min(GDE))
  # }
  
  
  return(GDE)
  
}
