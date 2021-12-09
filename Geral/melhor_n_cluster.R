#FUNÇÃO DE ANÁLISE DE CLUSTERS

melhor_n_cluster = function (dados_norm_treino){
  
  #Analisar melhor quantidade de clusters na clusterização por kmeans
  
  
  #silhouette
    fviz_nbclust(dados_norm_treino, kmeans, method = "silhouette")+
      labs(subtitle = "Silhouette method")
  
  # Elbow method
   fviz_nbclust(dados_norm_treino, kmeans, method = "wss") +
     geom_vline(xintercept = 4, linetype = 2)+
     labs(subtitle = "Elbow method")
  
   
   
  # Gap statistic
  # nboot = 50 to keep the function speedy. 
  # recommended value: nboot= 500 for your analysis.
  # Use verbose = FALSE to hide computing progression.
  # fviz_nbclust(dados_norm_treino, kmeans, nstart = 25,  method = "gap_stat", nboot = 50, verbose = TRUE)+
  #   labs(subtitle = "Gap statistic method")
  # 
  
}
