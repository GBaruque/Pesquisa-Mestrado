#CLASSIFICADOR DE BAYES COMPARADO AO KNN


comparativo_knn_bayes = function (dados_norm_treino, dados_norm_teste,classes_treino, classes_teste){
  
  print(paste("INÍCIO do método bayes geral:",Sys.time()))
  
  #--------------------------------------------------------------------------------
  #                   Inicialização das variáveis e constantes
  #--------------------------------------------------------------------------------
  
  n_classes = length(table(classes_treino))
  
  prob_bayes_treino  = matrix(data = 0, nrow = nrow(dados_norm_treino), ncol = n_classes)
  
  prob_bayes_teste  = matrix(data = 0, nrow = nrow(dados_norm_teste), ncol = n_classes)
  
  #--------------------------------------------------------------------------------
  
  
  
  
  
  
  #--------------------------------------------------------------------------------
  #             Construindo vetor de probabilidades a priori
  #--------------------------------------------------------------------------------
  
  P = matrix(data = 0, ncol = n_classes)
  
  for(i in 1:n_classes){
    P[i] = length(which(classes_treino==i))/nrow(dados_norm_treino)
  }
  
  #--------------------------------------------------------------------------------
  
  
  
  #================================================================================
  #
  #              TREINO PELO CLASSIFICADOR BAYESIANO (NUMERADOR)
  #
  #================================================================================
  
  #--------------------------------------------------------------------------------
  #                                     BAYES
  #                   Calculando numerador de bayes para TREINO
  #--------------------------------------------------------------------------------
  
  print("Bayes treino")
  print(Sys.time())
  
  for(i in 1:nrow(dados_norm_treino)){
    progress(i,nrow(dados_norm_treino))
    for(j in 1:n_classes){
      
      obs = dados_norm_treino[i,]
      
      if(classes_treino[i] == j){
        Mat_aval= dados_norm_treino[which(classes_treino==j),]
        Y_aval = which(classes_treino==j)
        indice_pra_tirar = which(Y_aval==i)
        Mat_aval=Mat_aval[-c(indice_pra_tirar),]
        prob_bayes_treino[i,j] = P[j]*GDE_manual_Alterado(obs, Mat_aval)
      }
      else{
        Mat_aval= dados_norm_treino[which(classes_treino==j),]
        prob_bayes_treino[i,j] = P[j]*GDE_manual_Alterado(obs, Mat_aval)
      }
    }
  }
  
  #--------------------------------------------------------------------------------
  
  
  
  #--------------------------------------------------------------------------------
  #                                      BAYES
  # Classificar de acordo com o maior valor obtido para cada observação (TREINO)
  #--------------------------------------------------------------------------------
  
  classificacao_treino = max.col(prob_bayes_treino)
  
  corretas = 0
  erradas = 0
  
  #numero de erros e acertos
  print("Classificação treino")
  for (i in 1:nrow(dados_norm_treino)){
    if(classes_treino[i]==classificacao_treino[i]){
      corretas = corretas + 1
    } else if(classes_treino[i]!=classificacao_treino[i]){
      erradas = erradas + 1
    }
    
  }
  
  acerto_percentual_treino = corretas/nrow(dados_norm_treino)
  erro_percentual_treino = erradas/nrow(dados_norm_treino)
  
  print(data.frame(erradas = erro_percentual_treino,
             corretas = acerto_percentual_treino,
             row.names = "percentual treino"))
  
  #--------------------------------------------------------------------------------

  
  
  #--------------------------------------------------------------------------------
  #                                     BAYES
  #                   Calculando numerador de bayes para TESTE
  #--------------------------------------------------------------------------------

  
  #calculando o numerador do classificador de bayes para cada observação de TESTE
  #em relação a cada classe
  print("Bayes teste")
  print(Sys.time())
  for(i in 1:nrow(dados_norm_teste)){
    progress(i, nrow(dados_norm_teste))
    for(j in 1:n_classes){
      
      obs = dados_norm_teste[i,]
      
      Mat_aval= dados_norm_treino[which(classes_treino==j),]
      prob_bayes_teste[i,j] = P[j]*GDE_manual_Alterado(obs, Mat_aval)
      
    }
  }
  print("Fim bayes teste")
  #--------------------------------------------------------------------------------
  

  
  #--------------------------------------------------------------------------------
  #                                      BAYES
  # Classificar de acordo com o maior valor obtido para cada observação (TESTE)
  #--------------------------------------------------------------------------------
  
  classificacao_teste = max.col(prob_bayes_teste)
  
  corretas = 0
  erradas = 0
  
  #numero de erros e acertos
  
  print("Classificação teste")
  for (i in 1:nrow(dados_norm_teste)){
    if(classes_teste[i]==classificacao_teste[i]){
      corretas = corretas + 1
    } else if(classes_teste[i]!=classificacao_teste[i]){
      erradas = erradas + 1
    }
      
  }
  
  acerto_percentual_teste = corretas/nrow(dados_norm_teste)
  erro_percentual_teste = erradas/nrow(dados_norm_teste)
  
  print(data.frame(erradas = erro_percentual_teste,
             corretas = acerto_percentual_teste,
             row.names = "percentual teste"))
  
  #--------------------------------------------------------------------------------
  
  print(paste("FIM do método bayes geral:",Sys.time()))
  
  #================================================================================
  
  
  
  #================================================================================
  #
  #                         CLASSIFICAÇÃO ATRAVÉS DO KNN
  #
  #================================================================================
  
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
