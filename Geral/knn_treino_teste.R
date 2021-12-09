#KNN TREINAMENTO (3, 5, 7 E 9 VIZINHOS) + TESTE
  
  
knn_treino_teste = function (dados_norm_treino, dados_norm_teste,classes_treino, classes_teste){
  
  print(paste("INÍCIO do método kNN:",Sys.time()))
  
  #--------------------------------------------------------------------------------
  #                                      KNN
  # Resultados prévios utilizando apenas observações de treinamento (one for all)
  #--------------------------------------------------------------------------------
  
  contador_knn = rep(0, times = 4)
  
  for(i in 1:nrow(dados_norm_treino)){
    
    obs_knn = dados_norm_treino[i,]
    
    dados_treino_temp = dados_norm_treino[-c(i),]
    classes_treino_temp = classes_treino[-c(i)]
    
    knn3 = knn(dados_treino_temp, obs_knn, classes_treino_temp, k = 3, l = 0, prob = FALSE, use.all = TRUE)
    knn5 = knn(dados_treino_temp, obs_knn, classes_treino_temp, k = 5, l = 0, prob = FALSE, use.all = TRUE)
    knn7 = knn(dados_treino_temp, obs_knn, classes_treino_temp, k = 7, l = 0, prob = FALSE, use.all = TRUE)
    knn9 = knn(dados_treino_temp, obs_knn, classes_treino_temp, k = 9, l = 0, prob = FALSE, use.all = TRUE)
    
    #contabilizando acertos para knn com 3, 5, 7 e 9 vizinhos
    if (knn3 == classes_treino[i]){contador_knn[1] = contador_knn[1] + 1}
    if (knn5 == classes_treino[i]){contador_knn[2] = contador_knn[2] + 1}
    if (knn7 == classes_treino[i]){contador_knn[3] = contador_knn[3] + 1}
    if (knn9 == classes_treino[i]){contador_knn[4] = contador_knn[4] + 1}
    
  }
  contador_knn = t(matrix(contador_knn))
  
  #escolhendo nº de vizinho baseado na contagem anterior (maior)
  #lidando com possíveis empates (escolhendo menor nº de vizinhos)
  
  if(max(contador_knn) == contador_knn[1]){
    n_vizinhos = 3
  } else if(max(contador_knn) == contador_knn[2]){
    n_vizinhos = 5
  } else if(max(contador_knn) == contador_knn[3]){
    n_vizinhos = 7
  } else if(max(contador_knn) == contador_knn[4]){
    n_vizinhos = 9
  }
  print(paste("Números de vizinhos usados:", n_vizinhos));
  
  
  
  #--------------------------------------------------------------------------------
  
  
  #--------------------------------------------------------------------------------
  #                                      KNN
  #      Classificação utilizando nº de vizinhos calculado anteriormente
  #--------------------------------------------------------------------------------
  knn_resposta = knn(dados_norm_treino, dados_norm_teste, classes_treino, k = n_vizinhos, l = 0, prob = TRUE, use.all = TRUE)
  print(prop.table(table(knn_resposta==classes_teste)))
  
  #--------------------------------------------------------------------------------
  
  print(paste("FIM do método kNN:",Sys.time()))
}
