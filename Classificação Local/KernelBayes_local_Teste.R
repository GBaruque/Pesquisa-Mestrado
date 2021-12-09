#CLASSIFICAÇÃO LOCAL DE OBSERVAÇÕES ATRAVÉS DO K-MEANS



kmeans_classificacao_local = function (dados_norm_treino, dados_norm_teste,classes_treino, classes_teste, n_cluster, n_classes){
  
  #-----------Função de distância entre pontos-----------
  euclideana = function(ponto1, ponto2){
    distancia = sqrt(sum((ponto2 - ponto1) ^ 2))
    return(distancia)
  }
  #------------------------------------------------------
  
  
  #----------------------------------------------------------------------------------------
  #                       K-means e separação de informações
  #----------------------------------------------------------------------------------------
  km = kmeans(dados_norm_treino, centers = n_cluster, iter.max = 1000, nstart = 50)
  
  #plotar novamente o gráfico com os centros e as observações clusterizadas
  #plot(dados_norm_treino, col = c(1:20)[as.factor(km$cluster)], pch=16, cex=1)
  #points(km$centers, col = "orange", pch = 8,cex = 2)
  
  
  
  #criando lista com elementos dos clusters
  cluster_elementos = vector("list", n_cluster)
  for(i in 1:n_cluster){
    cluster_elementos[[i]] = dados_norm_treino[which(km$cluster== i),]
  }
  
  
  #criando lista com classes dos clusters
  cluster_classes = vector("list", n_cluster)
  for(i in 1:n_cluster){
    cluster_classes[[i]] = classes_treino[which(km$cluster== i)]
  }
  #----------------------------------------------------------------------------------------
  
  
  
  #----------------------------------------------------------------------------------------
  #                       Probabilidades a Priori dos clusters
  #----------------------------------------------------------------------------------------
  P = matrix(0, nrow = n_cluster, ncol = n_classes)
  
  for(i in 1:n_cluster){
    for(j in 1:n_classes){
      
      P[i,j] = length(which(cluster_classes[[i]]==j))/length(cluster_classes[[i]])
      
    }
  }
  
  
  #----------------------------------------------------------------------------------------

  
  
  #----------------------------------------------------------------------------------------
  #                            Homogeneidade dos clusters
  #----------------------------------------------------------------------------------------
  cluster_homo = matrix(0, nrow = n_cluster, ncol = 2)
  
  for(i in 1:n_cluster){
    
    temp = table(cluster_classes[[i]])
    
    if(length(unique(cluster_classes[[i]])) == 1){
      cluster_homo[i,1] = TRUE
      cluster_homo[i,2] = as.integer(names(temp[temp==max(temp)]))
    } else if((max(prop.table(table(cluster_classes[[i]])))) >= 0.95 & (-sort(-table(cluster_classes[[i]]))[2] <= 20)){
      cluster_homo[i,1] = TRUE
      cluster_homo[i,2] = as.integer(names(temp[temp==max(temp)]))
    } else{
      cluster_homo[i,1] = FALSE
      cluster_homo[i,2] = 0
    }
  }
  #----------------------------------------------------------------------------------------
  
  
  
  #----------------------------------------------------------------------------------------
  #             Proximidade dos elementos de TESTE aos centros dos clusters
  #----------------------------------------------------------------------------------------
  centroides = matrix(km$centers, nrow = n_cluster, ncol = ncol(dados_norm_treino))
  
  todas_distancias = matrix(0, nrow = nrow(dados_norm_teste), ncol = n_cluster)
  
  for (i in 1:nrow(todas_distancias)){
    for(j in 1:ncol(todas_distancias)){
      
      todas_distancias[i,j] = euclideana(dados_norm_teste[i,],km$centers[j,])
      
    }
  }
  #----------------------------------------------------------------------------------------
  
  
  
  #----------------------------------------------------------------------------------------
  #             Atribuição dos dados TESTE aos clusters mais próximos
  #----------------------------------------------------------------------------------------
  
  dados_teste_cluster = rep(0, nrow(todas_distancias))
  
  for(i in 1:length(dados_teste_cluster)){
    
    dados_teste_cluster[i] = which.min(todas_distancias[i,])
    
  }
  #----------------------------------------------------------------------------------------
  
  
  
  #----------------------------------------------------------------------------------------
  #               Teste de bayes caso o cluster seja heterogêneo e atribuição
  #                       da classe majoritária caso seja homogêneo
  #----------------------------------------------------------------------------------------
  
  #definido o cluster mais próximo, realizar o teste com bayes, apenas com as observações
  #daquele cluster, dado que o cluster não é homogêneo
  
  print("Bayes local")
  classes_estimadas_teste = rep(0,nrow(dados_norm_teste))
  prob_bayes_final = matrix(0,nrow(dados_norm_teste), n_classes )
  
  for (i in 1:nrow(dados_norm_teste)) {
    progress(i, nrow(dados_norm_teste))
    if(cluster_homo[dados_teste_cluster[i],1]){
      classes_estimadas_teste[i] = cluster_homo[dados_teste_cluster[i],2]
      prob_bayes_final[i,] = P[dados_teste_cluster[i],]
    } else{
      #caso o cluster nao seja homogeneo, devemos utilizar bayes para calcular
      #a probabilidade da observação pertencer a determinada classe dentro do cluster
      prob_bayes_temp = matrix(0, nrow = 1, ncol = n_classes)
      obs = dados_norm_teste[i,]
      for(j in 1:n_classes){
        if(length(which(cluster_classes[[dados_teste_cluster[i]]]==j)) > 1){
          Mat_aval= matrix(cluster_elementos[[dados_teste_cluster[i]]][which(cluster_classes[[dados_teste_cluster[i]]]==j),],
                           ncol = ncol(dados_norm_treino))
          prob_bayes_temp[j] = P[dados_teste_cluster[i],j]*GDE_manual_Alterado(obs, Mat_aval)
          
        } else{
          next()
        }
      }
      prob_bayes_final[i,] = prob_bayes_temp/sum(prob_bayes_temp)
      classes_estimadas_teste[i] = max.col(prob_bayes_temp/sum(prob_bayes_temp))
    }
  }
  #----------------------------------------------------------------------------------------
  
  
  
  #----------------------------------------------------------------------------------------
  #                               Contabilização de acertos
  #----------------------------------------------------------------------------------------
  corretas = 0
  erradas = 0
  
  for (i in 1:nrow(dados_norm_teste)){
    
    if(classes_teste[i]==classes_estimadas_teste[i]){
      corretas = corretas + 1
    } else if(classes_teste[i]!=classes_estimadas_teste[i]){
      erradas = erradas + 1
    }
    
  }
  
  acerto_percentual_teste = corretas/length(classes_teste)
  erro_percentual_teste = erradas/length(classes_teste)
  
  print(data.frame(erradas = erro_percentual_teste,
                   corretas = acerto_percentual_teste,
                   row.names = "percentual"))
  
  
  #acerto por classe
  
  # resultado_por_classe = rep(0,n_classes)
  # for(i in 1:n_classes){
  #   resultado_por_classe = /length(which(classes_teste == 1))
  # }
  
  
}#final da função

