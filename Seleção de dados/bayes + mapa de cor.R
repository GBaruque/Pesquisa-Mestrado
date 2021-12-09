#BAYES + MAPA DE COR



bayes_mapa_cor = function (dados_norm_treino, dados_norm_teste,classes_treino, classes_teste, n_cluster=1, n_classes){
  
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
  #                            Bayes com dados de treino
  #----------------------------------------------------------------------------------------
  print(paste("INÍCIO do método Kernel Bayes Local - dados de TREINO:",Sys.time()))
  
  classes_estimadas_treino = rep(0,nrow(dados_norm_treino))
  prob_bayes_final_treino = matrix(0,nrow(dados_norm_treino), n_classes )
  
  print(Sys.time())
  
  for (i in 1:nrow(dados_norm_treino)) {
    progress(i, nrow(dados_norm_treino))
    if(cluster_homo[km[["cluster"]][i],1]){
      classes_estimadas_treino[i] = cluster_homo[km[["cluster"]][i],2]
      prob_bayes_final_treino[i,] = P[km[["cluster"]][i],]
    } else{
      #caso o cluster nao seja homogeneo, devemos utilizar bayes para calcular
      #a probabilidade da observação pertencer a determinada classe dentro do cluster
      prob_bayes_temp = matrix(0, nrow = 1, ncol = n_classes)
      obs = dados_norm_treino[i,]
      
      dados_treino_temp = dados_norm_treino[-c(i),]
      classes_treino_temp = classes_treino[-c(i)]
      clusters_temp = km[["cluster"]][-c(i)]
      
      for(j in 1:n_classes){
        
        if(length(which(classes_treino_temp[which(clusters_temp == km[["cluster"]][i])]==j)) > 1){
          
          Mat_aval= matrix(dados_treino_temp[which((clusters_temp==km[["cluster"]][i])&(classes_treino_temp==j)),],
                           ncol = ncol(dados_treino_temp))
          
          prob_bayes_temp[j] = P[km[["cluster"]][i],j]*GDE_manual_Alterado(obs, Mat_aval)
          
        } else{
          next()
        }
      }
      prob_bayes_final_treino[i,] = prob_bayes_temp/sum(prob_bayes_temp)
      classes_estimadas_treino[i] = max.col(prob_bayes_temp/sum(prob_bayes_temp))
    }
  }
  print("fim progresso")
  #----------------------------------------------------------------------------------------
  
  
  
  #----------------------------------------------------------------------------------------
  #                           SEPARAÇÃO DE OBSERVAÇÕES DE REGIÕES
  #----------------------------------------------------------------------------------------
  #lógica: which((classe 1 E menor que 0,2 na classe 1) OU (classe 2 e menor que 0,2 na classe 2))
  
  #são da classe 1 com menos de 0,2 na classe 1 =====> ((classes_treino == 1) & (prob_bayes_final_treino[,1]<=0,2))
  #dados_norm_treino[which(),]
  
  indices_bayes_classe1 = vector("list", 5)
  indices_bayes_classe2 = vector("list", 5)        
  
  #classe1
  indices_bayes_classe1[[1]] = which(((classes_treino == 1) & (prob_bayes_final_treino[,1]<=0.2)))
  indices_bayes_classe1[[2]] = which(((classes_treino == 1) & ((prob_bayes_final_treino[,1] > 0.20)&(prob_bayes_final_treino[,1] <= 0.4))))
  indices_bayes_classe1[[3]] = which(((classes_treino == 1) & ((prob_bayes_final_treino[,1] > 0.4)&(prob_bayes_final_treino[,1] <= 0.6))))
  indices_bayes_classe1[[4]] = which(((classes_treino == 1) & ((prob_bayes_final_treino[,1] > 0.6)&(prob_bayes_final_treino[,1] <= 0.8))))
  indices_bayes_classe1[[5]] = which(((classes_treino == 1) & ((prob_bayes_final_treino[,1] > 0.8)&(prob_bayes_final_treino[,1] <= 1))))
  
  #classe2
  indices_bayes_classe2[[1]] = which(((classes_treino == 2) & (prob_bayes_final_treino[,2]<=0.2)))
  indices_bayes_classe2[[2]] = which(((classes_treino == 2) & ((prob_bayes_final_treino[,2] > 0.20)&(prob_bayes_final_treino[,2] <= 0.4))))
  indices_bayes_classe2[[3]] = which(((classes_treino == 2) & ((prob_bayes_final_treino[,2] > 0.4)&(prob_bayes_final_treino[,2] <= 0.6))))
  indices_bayes_classe2[[4]] = which(((classes_treino == 2) & ((prob_bayes_final_treino[,2] > 0.6)&(prob_bayes_final_treino[,2] <= 0.8))))
  indices_bayes_classe2[[5]] = which(((classes_treino == 2) & ((prob_bayes_final_treino[,2] > 0.8)&(prob_bayes_final_treino[,2] <= 1))))
  
  
  # #classe1
  # bayes_classe1[[1]] = dados_norm_treino[which(((classes_treino == 1) & (prob_bayes_final_treino[,1]<=0.2))),]
  # bayes_classe1[[2]] = dados_norm_treino[which(((classes_treino == 1) & ((prob_bayes_final_treino[,1] > 0.20)&(prob_bayes_final_treino[,1] <= 0.4)))),]
  # bayes_classe1[[3]] = dados_norm_treino[which(((classes_treino == 1) & ((prob_bayes_final_treino[,1] > 0.4)&(prob_bayes_final_treino[,1] <= 0.6)))),]
  # bayes_classe1[[4]] = dados_norm_treino[which(((classes_treino == 1) & ((prob_bayes_final_treino[,1] > 0.6)&(prob_bayes_final_treino[,1] <= 0.8)))),]
  # bayes_classe1[[5]] = dados_norm_treino[which(((classes_treino == 1) & ((prob_bayes_final_treino[,1] > 0.8)&(prob_bayes_final_treino[,1] <= 1)))),]
  # 
  # #classe2
  # bayes_classe2[[1]] = dados_norm_treino[which(((classes_treino == 2) & (prob_bayes_final_treino[,2]<=0.2))),]
  # bayes_classe2[[2]] = dados_norm_treino[which(((classes_treino == 2) & ((prob_bayes_final_treino[,2] > 0.20)&(prob_bayes_final_treino[,2] <= 0.4)))),]
  # bayes_classe2[[3]] = dados_norm_treino[which(((classes_treino == 2) & ((prob_bayes_final_treino[,2] > 0.4)&(prob_bayes_final_treino[,2] <= 0.6)))),]
  # bayes_classe2[[4]] = dados_norm_treino[which(((classes_treino == 2) & ((prob_bayes_final_treino[,2] > 0.6)&(prob_bayes_final_treino[,2] <= 0.8)))),]
  # bayes_classe2[[5]] = dados_norm_treino[which(((classes_treino == 2) & ((prob_bayes_final_treino[,2] > 0.8)&(prob_bayes_final_treino[,2] <= 1)))),]
  
  #----------------------------------------------------------------------------------------
  
  
  
  #----------------------------------------------------------------------------------------
  #                           TESTE COM O KNN (treino)
  #----------------------------------------------------------------------------------------
  
  desempenho = array(0, dim = c(5,5,4))
  
  for (i in 1:length(indices_bayes_classe1)){
    
    if(!length(indices_bayes_classe1[[i]])){
      print(paste("Não há dados em i = ",i, ". A linha referente ficará em branco."))
      next()
    }
    
    dados_treino_temp = dados_norm_treino[indices_bayes_classe1[[i]],]
    classes_treino_temp = classes_treino[indices_bayes_classe1[[i]]]
    
    for(j in 1:length(indices_bayes_classe2)){
      
      if(!length(indices_bayes_classe2[[j]])){
        print(paste("Não há dados em j = ",j, ". A coluna referente ficará em branco."))
        next()
      }
      
      dados_treino_temp = rbind(dados_treino_temp,dados_norm_treino[indices_bayes_classe2[[j]],])
      classes_treino_temp = c(classes_treino_temp, classes_treino[indices_bayes_classe2[[j]]])
      
      if(nrow(dados_treino_temp) <=11){
        print(paste("Em i=",i, "e j=", j, "o número de padrões é:", nrow(dados_treino_temp), "e portanto não será levado em conta"))
        next()
      }
      
      for(z in 1:nrow(dados_treino_temp)){
        
        
        
        obs_knn = dados_treino_temp[z,]
        classe_obs = classes_treino_temp[z]
        
        dados_treino_temp2 = dados_treino_temp[-c(z),]
        if(!is.matrix(dados_treino_temp2)){
          dados_treino_temp2 = matrix(dados_treino_temp2, ncol = ncol(dados_treino_temp))
        }
        classes_treino_temp2 = classes_treino_temp[-c(z)]
        
        knn3 = knn(dados_treino_temp2, obs_knn, classes_treino_temp2, k = 3, l = 0, prob = FALSE, use.all = TRUE)
        knn5 = knn(dados_treino_temp2, obs_knn, classes_treino_temp2, k = 5, l = 0, prob = FALSE, use.all = TRUE)
        knn7 = knn(dados_treino_temp2, obs_knn, classes_treino_temp2, k = 7, l = 0, prob = FALSE, use.all = TRUE)
        knn9 = knn(dados_treino_temp2, obs_knn, classes_treino_temp2, k = 9, l = 0, prob = FALSE, use.all = TRUE)
        
        # if(nrow(dados_treino_temp2)>=3){
        #   knn3 = knn(dados_treino_temp2, obs_knn, classes_treino_temp2, k = 3, l = 0, prob = FALSE, use.all = TRUE)
        # }
        # 
        # if(nrow(dados_treino_temp2)>=5){
        #   knn5 = knn(dados_treino_temp2, obs_knn, classes_treino_temp2, k = 5, l = 0, prob = FALSE, use.all = TRUE)
        # }
        # 
        # if(nrow(dados_treino_temp2)>=7){
        #   knn7 = knn(dados_treino_temp2, obs_knn, classes_treino_temp2, k = 7, l = 0, prob = FALSE, use.all = TRUE)
        # }
        # 
        # if(nrow(dados_treino_temp2)>=9){
        #   knn9 = knn(dados_treino_temp2, obs_knn, classes_treino_temp2, k = 9, l = 0, prob = FALSE, use.all = TRUE)
        # }
        
        
        if (knn3 == classe_obs){desempenho[i,j,1] = desempenho[i,j,1] + 1}
        if (knn5 == classe_obs){desempenho[i,j,2] = desempenho[i,j,2] + 1}
        if (knn7 == classe_obs){desempenho[i,j,3] = desempenho[i,j,3] + 1}
        if (knn9 == classe_obs){desempenho[i,j,4] = desempenho[i,j,4] + 1}
        
      }
    }
  }
  
  print("desempenho:")
  print(desempenho)
  # nesse loop, "i" representa o grupo da classe 1:     e   "j" representa o grupo da classe 1:
  #             i = 1 -> bayes < 0,2                        j = 1 -> bayes < 0,2
  #             i = 2 -> bayes entre  0,2 e 0,4             j = 2 -> bayes entre  0,2 e 0,4
  #             i = 3 -> bayes entre  0,4 e 0,6             j = 3 -> bayes entre  0,4 e 0,6
  #             i = 4 -> bayes entre  0,6 e 0,8             j = 4 -> bayes entre  0,6 e 0,8
  #             i = 5 -> bayes > 0,8                        j = 5 -> bayes >  0,8
  
  
  #----------------------------------------------------------------------------------------
  
  
  
  #----------------------------------------------------------------------------------------
  #                   SEPARAÇÃO DO MELHOR DESEMPENHO (melhor número de vizinhos)
  #----------------------------------------------------------------------------------------
  melhor_n_vizinhos = matrix(0, nrow = nrow(desempenho[,,1]), ncol = ncol(desempenho[,,1]))
  
  for(i in 1:nrow(melhor_n_vizinhos)){
    for (j in 1:ncol(melhor_n_vizinhos)) {
      if(max(desempenho[i,j,]) == desempenho[i,j,1]){
        melhor_n_vizinhos[i,j] = 3
      } else if(max(desempenho[i,j,]) == desempenho[i,j,2]){
        melhor_n_vizinhos[i,j] = 5
      } else if(max(desempenho[i,j,]) == desempenho[i,j,3]){
        melhor_n_vizinhos[i,j] = 7
      } else if(max(desempenho[i,j,]) == desempenho[i,j,4]){
        melhor_n_vizinhos[i,j] = 9
      }
    }
  }
  

  melhor_n_vizinhos[which((desempenho[,,1] == 0) & (desempenho[,,2] == 0) & (desempenho[,,3] == 0) & (desempenho[,,4] == 0))] = NaN

  
  print("Números de vizinhos usados:")
  print(melhor_n_vizinhos)
  #----------------------------------------------------------------------------------------
  
  
  
  #----------------------------------------------------------------------------------------
  #                                    KNN NO TESTE
  #----------------------------------------------------------------------------------------
  
  desempenho_teste = matrix(0, nrow = nrow(melhor_n_vizinhos), ncol = ncol(melhor_n_vizinhos))
  
  for (i in 1:nrow(melhor_n_vizinhos)) {
    
    if(!length(indices_bayes_classe1[[i]])){
      print(paste("Não há dados em i = ",i, ". A linha/coluna referente ficará em branco."))
      next()
    }
    
    treino_atual = dados_norm_treino[indices_bayes_classe1[[i]],]
    classe_atual = classes_treino[indices_bayes_classe1[[i]]]
    
    
    for (j in 1:ncol(melhor_n_vizinhos)) {
      
      if(!length(indices_bayes_classe2[[j]])){
        print(paste("Não há dados em j = ",j, ". A linha/coluna referente ficará em branco."))
        next()
      }
      
      treino_atual2 = rbind(treino_atual,dados_norm_treino[indices_bayes_classe2[[j]],])
      classe_atual2 = c(classe_atual, classes_treino[indices_bayes_classe2[[j]]])
      
      vizinhos = melhor_n_vizinhos[i,j]
      
      if(is.na(vizinhos)){
        print(paste("Em i=",i, "e j=", j, "o teste não foi calculado"))
      } else{
        knn_resposta = knn(treino_atual2, dados_norm_teste, classe_atual2, k = vizinhos, l = 0, prob = TRUE, use.all = TRUE)
        
        desempenho_teste[i,j] = prop.table(table(knn_resposta==classes_teste))[2]
        
      }
    }
  }
  print("Desempenho:")
  desempenho_teste[which(desempenho_teste == 0)] = NaN
  print(desempenho_teste)
  
  
  
  #----------------------------------------------------------------------------------------
  
  
  
  #----------------------------------------------------------------------------------------
  #                                    MAPA DE COR
  #----------------------------------------------------------------------------------------
  
  color = colorRampPalette(rev(c("#D73027", "#FC8D59", "#FEE090", "#FFFFBF", "#E0F3F8", "#91BFDB", "#4575B4")))(100)
  levelplot(desempenho_teste, scale=list(x=list(rot=45)), col.regions = color,ylab="Classe 2", xlab="Classe 1")
  
  #----------------------------------------------------------------------------------------
  
}#final da função

