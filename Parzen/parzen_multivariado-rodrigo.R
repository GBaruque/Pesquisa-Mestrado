require("mvtnorm")

GDE_manual = function(dados){ #verificar outros parametros necessários
  
  Rn = length(dados[1,])
  n_dados =length(dados[,1])
  lambda = diag(1,Rn,Rn)
  
  GDE = rep(0,n_dados)
  GDE_norm = rep(0,n_dados)
  
  for(i in 1:n_dados){
    somatorio = 0;
    x0 = matrix(dados[i,], nrow = 1);
    
    for(j in 1:n_dados){
      
      xj = matrix(dados[j,], nrow = 1);
      somatorio = somatorio + dmvnorm(x0 - xj, mean=rep(0, Rn), sigma=diag(Rn), log=FALSE)
      
    }
    GDE[i] = (1/(n_dados))*somatorio;
  }
  
  # for (i in 1:n_dados){
  #   GDE_norm[i] = (GDE[i]-min(GDE))/(max(GDE)-min(GDE))
  # }
  
  
  return(GDE)
  
}