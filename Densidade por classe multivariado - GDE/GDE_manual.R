#dados: matriz com dados pertinentes a uma classe específica
#obs: observação a ser avaliada com relação aos dados

GDE_manual = function(obs, dados){
  
  Rn = ncol(dados)
  n_dados =nrow(dados)
  lambda = diag(1,Rn,Rn)
  
  GDE = 0
  #GDE_norm = 0
  
  somatorio = 0;
  x0 = obs;
  
  for(j in 1:n_dados){
    
    xj = matrix(dados[j,], nrow = 1);
    somatorio = somatorio + dmvnorm(x0 - xj, mean=rep(0, Rn), sigma=1*diag(Rn), log=FALSE)
    
  }
  GDE = (1/(n_dados))*somatorio;
  
  
  # for (i in 1:n_dados){
  #   GDE_norm[i] = (GDE[i]-min(GDE))/(max(GDE)-min(GDE))
  # }
  
  
  return(GDE)
  
}
