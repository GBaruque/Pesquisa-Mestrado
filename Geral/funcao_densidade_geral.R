densidade_geral = function (dados, classes){
  
  densidade_global = kde(dados, eval.points = dados, binned = FALSE)
  
  #densidade_global$estimate
  dados_com_densidade_global = cbind(dados,densidade_global$estimate)
  dados_ordenado_global = order(dados_com_densidade_global[,3]) #ordena os indices em ordem crescente com base na densidade
  
  #métodos de "classificação" (cores)
  i = 1
  cor = rep(0,length(dados_ordenado_global))
  while (i <= length(dados_ordenado_global))
  {  
    if(i <= 0.25 * length(dados_ordenado_global))
    {
      cor[dados_ordenado_global[i]] = 1
    }
    else{if(i >= 0.75 * length(dados_ordenado_global)){
      
      cor[dados_ordenado_global[i]] = 3
      
    }
      else{
        cor[dados_ordenado_global[i]] = 2
      }
    }
    
    i=i+1
  }
  
  plot(dados, col = c("red","black","green")[as.factor(cor)])
  
  
}
