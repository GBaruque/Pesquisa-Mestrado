#Parzen Multidimensional- Gaussian Density Estimate - Fç implementada à mão


GDE_manual = function(dados){ #verificar outros parametros necessários
  
  Rn = length(dados[1,])
  n_dados =length(dados[,1])
  
  lambda = diag(1,Rn,Rn)
  
  somatorio = 0;
  GDE = rep(0,n_dados)
  
  for(i in 1:n_dados){
    
    x0 = matrix(dados[i,], nrow = 1);
    
    for(j in 1:n_dados){
      
      xj = matrix(dados[j,], nrow = 1);
      somatorio = somatorio + exp(-0.5*(norm(xi - x0)/lambda)^2);
      
    }
    GDE[i] = (((n_dados)*(2*(lambda^2)*pi)^(Rn/2))^(-1))*somatorio;
  }

  return(GDE)
  
  }
  
  
  
  