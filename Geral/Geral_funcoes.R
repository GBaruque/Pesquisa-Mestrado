rm(list=ls())

#--------------------------------------------------------------------------------
#             Pacotes necessários
#--------------------------------------------------------------------------------
#carregando pacote MASS
install.packages("MASS")
install.packages("class")
install.packages("openxlsx")
install.packages("ks")
install.packages("mvtnorm")
install.packages("factoextra")
install.packages("NbClust")
install.packages("svMisc")
install.packages("lattice")
install.packages("caret", dependencies = c("Depends", "Suggests"))
install.packages("e1071")
install.packages("pROC")
install.packages("NoiseFiltersR")


require("MASS")
require("class")
require("openxlsx")
require("ks")
require("mvtnorm")
require("factoextra")
require("NbClust")
require("readxl")
require("svMisc")
require("lattice")
require("caret")
require("e1071")
require("pROC")
require("NoiseFiltersR")


#ou

library("MASS")
library("class")
library("openxlsx")
library("ks")
library("mvtnorm")
library("factoextra")
library("NbClust")
library("readxl")
library("svMisc")
library("lattice")
library("caret")
library("e1071")
library("pROC")
library("NoiseFiltersR")
#--------------------------------------------------------------------------------

#================================================================================
#--------------------------------------------------------------------------------

#    DISTRIBUIÇÕES SIMULADAS UTILIZADAS EM EXPERIMENTOS ANTERIORES

#--------------------------------------------------------------------------------
#================================================================================


#--------------------------------------------------------------------------------
#    Distribuição de normais com duas classes bem distintas.
#--------------------------------------------------------------------------------
{
  n<-20
  
  x1 = mvrnorm(n , c(0,0), matrix(c(1,0,0,10), nrow = 2))
  x2 = mvrnorm(n , c(10,10), matrix(c(10,0,0,1), nrow = 2))
  dados= rbind(x1,x2)
  
  y1 = rep(1, n)
  y2 = rep(2, n)
  classes = c(y1, y2)
  
  
  plot(dados, col = c("blue", "green")[as.factor(classes)])
}
#--------------------------------------------------------------------------------


#--------------------------------------------------------------------------------
#    Distribuição de normais onde a segunda
#    classe está "mergulhada" na primeira.
#--------------------------------------------------------------------------------
{
  n<-500
  
  x1 = mvrnorm(n , c(18,10), matrix(c(50,0,0,20), nrow = 2))
  x2 = mvrnorm(0.1*n , c(10,10), matrix(c(1,0,0,1), nrow = 2))
  
  dados= rbind(x1,x2)
  
  
  y1 = rep(1, n)
  y2 = rep(2, nrow(x2))
  classes = c(y1, y2)
  
  plot(dados, col = c("deeppink", "aquamarine3")[as.factor(classes)])
}
#--------------------------------------------------------------------------------


#--------------------------------------------------------------------------------
#    Distribuição de normais com 5 classes. Formato
#    "garagem com caminhão". Caminhão comporta 2 classes.
#--------------------------------------------------------------------------------
{
  n<-1000
  
  x1 = mvrnorm(0.5*n , c(16,-2), matrix(c(1,0,2,18), nrow = 2))
  x2 = mvrnorm(0.25*n , c(0,-2), matrix(c(1,0,2,18), nrow = 2))
  x3 = mvrnorm(0.75*n , c(7,7), matrix(c(18,1,6,1), nrow = 2))
  x4 = mvrnorm(1.5*n , c(7,-2), matrix(c(1,0,2,18), nrow = 2))
  x5 = mvrnorm(2*n , c(8,-15), matrix(c(1,0,2,18), nrow = 2))
  
  dados = rbind(x1,x2,x3,x4,x5)
  
  
  y1 = rep(1, 0.5*n)
  y2 = rep(2, 0.25*n)
  y3 = rep(3, 0.75*n)
  y4 = rep(4, 1.5*n)
  y5 = rep(5, 2*n)
  classes = c(y1, y2, y3, y4,y5)
  
  n_classes = length(table(classes))
  
  plot(dados, col = c("blue","red","green","magenta","black")[as.factor(classes)], pch=16, cex=1)
}
#--------------------------------------------------------------------------------



#--------------------------------------------------------------------------------
#    Distribuição de normais com 5 classes. Bem distintas,
#    com 2 classes se sobrepondo parcialmente
#--------------------------------------------------------------------------------
{
  n<-100
  
  x1 = mvrnorm(n , c(15,15), matrix(c(1,0,2,18), nrow = 2))
  x2 = mvrnorm(n , c(15,-15), matrix(c(1,0,2,18), nrow = 2))
  x3 = mvrnorm(n , c(0,0), matrix(c(10,1,6,1), nrow = 2))
  x4 = mvrnorm(n , c(-15,5), matrix(c(1,0,2,18), nrow = 2))
  x5 = mvrnorm(n , c(-15,-5), matrix(c(1,0,2,18), nrow = 2))
  
  dados = rbind(x1,x2,x3,x4,x5)
  
  
  y1 = rep(1, n)
  y2 = rep(2, n)
  y3 = rep(3, n)
  y4 = rep(4, n)
  y5 = rep(5, n)
  classes = c(y1, y2, y3, y4,y5)
  
  n_classes = length(table(classes))
  
  plot(dados, col = c("blue","red","green","magenta","black")[as.factor(classes)], pch=16, cex=1)
}
#--------------------------------------------------------------------------------



#================================================================================
#--------------------------------------------------------------------------------

#    DISTRIBUIÇÕES REAIS EXTRAIDAS DO UCI (CONTÍNUAS E LIMPAS)

#--------------------------------------------------------------------------------
#================================================================================


#--------------------------------------------------------------------------------
#                       EXP01 - BANKNOTE AUTHENTICATION:
#--------------------------------------------------------------------------------
{
  exp01 = read.xlsx(paste0("0-Estudos/Mestrado/Projeto/Banco de dados/",
                           "SBRT.EXP01 - Data banknote authentication/",
                           "data_banknote_authentication.xlsx"),
                    startRow = 1, colNames = TRUE)
  
  dados = exp01[,-c(ncol(exp01))]
  
  classes = exp01[,ncol(exp01)]
}
#--------------------------------------------------------------------------------


#--------------------------------------------------------------------------------
#                       EXP02 - WIFI LOCALIZATION:
#--------------------------------------------------------------------------------
{
  exp02 = read.xlsx(paste0("0-Estudos/Mestrado/Projeto/Banco de dados/",
                           "SBRT.EXP02 - wireless indoor localization",
                           "/wifi_localization.xlsx"),
                    startRow = 1, colNames = TRUE)
  
  dados = exp02[,-c(ncol(exp02))]
  
  classes = exp02[,ncol(exp02)]
}
#--------------------------------------------------------------------------------


#--------------------------------------------------------------------------------
#                       EXP03 - BREAST CANCER WISCONSIN:
#--------------------------------------------------------------------------------
#
{
  exp03 = read.xlsx(paste0("0-Estudos/Mestrado/Projeto/Banco de dados/",
                           "SBRT.EXP03 - Breast cancer/breastcancer.xlsx"),
                    startRow = 1, colNames = TRUE)
  
  dados = exp03[,-c(ncol(exp03))]
  
  classes = exp03[,ncol(exp03)]
}
#--------------------------------------------------------------------------------


#--------------------------------------------------------------------------------
#                   EXP04 - CLIMATE MODEL SIMULATION CRASHES:
#--------------------------------------------------------------------------------
#não é necessário normalizar. Banco já normalizado "de fábrica".
{
  exp04 = read.xlsx(paste0("0-Estudos/Mestrado/Projeto/Banco de dados/",
                           "SBRT.EXP04 - Pop_failures/pop_failures.xlsx"),
                    startRow = 1, colNames = TRUE)
  
  dados = exp04[,-c(ncol(exp04))]
  
  classes = exp04[,ncol(exp04)]
}
#--------------------------------------------------------------------------------



#--------------------------------------------------------------------------------
#                               EXP05 - YEAST:
#--------------------------------------------------------------------------------
#não é necessário normalizar. Banco já normalizado "de fábrica"
#Retiradas colunas 5 e 6 por não possuírem variação, e portanto, serem
#irrelevantes ao classificador
{
  exp05 = read.xlsx(paste0("0-Estudos/Mestrado/Projeto/Banco de dados/",
                           "SBRT.EXP05 - Yeast/yeast - sem colunas 5 e 6 DICOTOMICO.xlsx"),
                    startRow = 1, colNames = TRUE)
  
  dados = exp05[,-c(ncol(exp05))]
  
  classes = exp05[,ncol(exp05)]
}

#--------------------------------------------------------------------------------


#--------------------------------------------------------------------------------
#                             EXP06 - WAVEFORM V1:
#--------------------------------------------------------------------------------
{
  exp06 = read.xlsx(paste0("0-Estudos/Mestrado/Projeto/Banco de dados/",
                           "Waveform/waveform-V1-DICOTOMICO.xlsx"),
                    startRow = 1, colNames = TRUE)
  
  dados = exp06[,-c(ncol(exp06))]
  
  classes = exp06[,ncol(exp06)]
}
#--------------------------------------------------------------------------------


#--------------------------------------------------------------------------------
#                             EXP07 - WAVEFORM V2:
#--------------------------------------------------------------------------------
{
  exp07 = read.xlsx(paste0("0-Estudos/Mestrado/Projeto/Banco de dados/",
                           "Waveform/version2/waveform+noise-V2-DICOTOMICO.xlsx"),
                    startRow = 1, colNames = TRUE)
  
  dados = exp07[,-c(ncol(exp07))]
  
  classes = exp07[,ncol(exp07)]
}
#--------------------------------------------------------------------------------


#--------------------------------------------------------------------------------
#                             EXP08 - ANURAN CALLS:
#--------------------------------------------------------------------------------
{
  exp08 = read.xlsx(paste0("0-Estudos/Mestrado/Projeto/Banco de dados/",
                           "Anuran Calls/Frogs-familia-Dicotomico-.xlsx"),
                    startRow = 1, colNames = TRUE)
  
  dados = exp08[,-c(ncol(exp08))]
  
  classes = exp08[,ncol(exp08)]
}
#--------------------------------------------------------------------------------


#--------------------------------------------------------------------------------
#                 EXP09 - ELECTRICAL GRID STABILITY SIMULATED:
#--------------------------------------------------------------------------------
{
  exp09 = read.xlsx(paste0("0-Estudos/Mestrado/Projeto/Banco de dados/",
                           "Electrical Grid Stability Simulated/EGSM - DICOTOMICO - apenas classe final.xlsx"),
                    startRow = 1, colNames = TRUE)
  
  dados = exp09[,-c(ncol(exp09))]
  
  classes = exp09[,ncol(exp09)]
}
#--------------------------------------------------------------------------------


#================================================================================





#================================================================================
#--------------------------------------------------------------------------------

#                 PRÉ-PROCESSAMENTO DOS DADOS

#--------------------------------------------------------------------------------
#================================================================================


#--------------------------------------------------------------------------------
#                             DIVISÃO EM 2 PARTES
#       Dividindo os dados e classes respectivas em treinamento e teste
#         + algumas variaveis importantes (tx_treino e n_classes)
#--------------------------------------------------------------------------------
{
  tx_treino = 2/3
  
  # para bancos com muitos dados, recomenda-se utilizar 
  # useHash = TRUE (pesquisar mais sobre)
  indices = sample.int(n = nrow(dados), size = floor(tx_treino*nrow(dados)),
                       replace = FALSE, prob= NULL, useHash = FALSE)
  
  
  dados_treino = dados[indices,]
  classes_treino = classes[indices]
  
  dados_teste= dados[-indices, ]
  classes_teste = classes[-indices]
  
  #plot(dados_norm_treino, col = c("blue","red","green","magenta","black")[as.factor(classes_treino)], pch=16, cex=1)
  #points(dados_norm_teste, col = "orange")
  
  #Nº de classes
  n_classes = length(table(classes_treino))
}
#--------------------------------------------------------------------------------


#--------------------------------------------------------------------------------
#                           DIVISÃO EM 3 PARTES
#       Dividir dados em treino, validação e teste, com taxa de 1/3 para cada
#         + algumas variaveis importantes (tx_treino e n_classes)
#--------------------------------------------------------------------------------
{
  tx_treino = 1/3
  
  # para bancos com muitos dados, recomenda-se utilizar 
  # useHash = TRUE (pesquisar mais sobre)
  indices = sample.int(n = nrow(dados), size = floor(tx_treino*nrow(dados)),
                       replace = FALSE, prob= NULL, useHash = FALSE)
  
  dados_treino = dados[indices,]
  classes_treino = classes[indices]
  
  dados_temp = dados[-c(indices),]
  classes_temp = classes[-c(indices)]
  
  tx_teste = 1/2
  
  indices = sample.int(n = nrow(dados_temp), size = floor(tx_teste*nrow(dados_temp)),
                       replace = FALSE, prob= NULL, useHash = FALSE)
  
  dados_teste = dados_temp[indices,]
  classes_teste = classes_temp[indices]
  
  dados_valid = dados_temp[-indices,]
  classes_valid = classes_temp[-indices]
  
  n_classes = length(table(classes_treino))
}
#--------------------------------------------------------------------------------


#VALIDAÇÃO E TESTE ESTÃO EM VARIÁVEIS TROCADAS,
#APENAS PARA EVITAR ALTERAR ALGO QUE JÁ ESTAVA FUNCIONANDO

#--------------------------------------------------------------------------------
#                   PARA BANCOS JÁ NORMALIZADOS - treino / teste
#--------------------------------------------------------------------------------
#CASO O BANCO JÁ SEJA NORMALIZADO, RODAR SOMENTE:
{
  dados_norm_treino = as.matrix(dados_treino)
  
  dados_norm_teste = as.matrix(dados_teste)
}
#--------------------------------------------------------------------------------


#--------------------------------------------------------------------------------
#           PARA BANCOS JÁ NORMALIZADOS - treino / validação / teste
#--------------------------------------------------------------------------------
#CASO O BANCO JÁ SEJA NORMALIZADO, RODAR SOMENTE:
{
  dados_norm_treino = as.matrix(dados_treino)
  
  dados_norm_teste = as.matrix(dados_teste)

  dados_norm_valid = as.matrix(dados_valid)
}
#--------------------------------------------------------------------------------


#--------------------------------------------------------------------------------
#                  Normalização dos dados - 2 partes
#--------------------------------------------------------------------------------
#CASO O BANCO NÃO SEJA NORMALIZADO, RODAR SOMENTE:
{
  dados_norm_treino = matrix(0, nrow(dados_treino),ncol(dados_treino))
  
  for (i in 1:nrow(dados_treino)){
    for(j in 1:ncol(dados_treino)){
      dados_norm_treino[i,j] = (dados_treino[i,j]-min(dados_treino[,j]))/(max(dados_treino[,j])-min(dados_treino[,j]))
    }
  }
  #plot(dados_norm, col = c("blue","red","green","magenta","black")[as.factor(classes)], pch=16, cex=1)



dados_norm_teste = matrix(0, nrow(dados_teste),ncol(dados_teste))

for (i in 1:nrow(dados_teste)){
  for(j in 1:ncol(dados_teste)){
    
    dados_norm_teste[i,j] = (dados_teste[i,j]-min(dados_treino[,j]))/(max(dados_treino[,j])-min(dados_treino[,j]))
    
  }
}
#plot(dados_norm, col = c("blue","red","green","magenta","black")[as.factor(classes)], pch=16, cex=1)



}
#--------------------------------------------------------------------------------


#--------------------------------------------------------------------------------
#                  Normalização dos dados - 3 partes
#--------------------------------------------------------------------------------
#CASO O BANCO NÃO SEJA NORMALIZADO, RODAR SOMENTE:
{
  dados_norm_treino = matrix(0, nrow(dados_treino),ncol(dados_treino))
  for (i in 1:nrow(dados_treino)){
    for(j in 1:ncol(dados_treino)){
      dados_norm_treino[i,j] = (dados_treino[i,j]-min(dados_treino[,j]))/(max(dados_treino[,j])-min(dados_treino[,j]))
    }
  }
  #plot(dados_norm, col = c("blue","red","green","magenta","black")[as.factor(classes)], pch=16, cex=1)
  
  
  
  dados_norm_teste = matrix(0, nrow(dados_teste),ncol(dados_teste))
  for (i in 1:nrow(dados_teste)){
    for(j in 1:ncol(dados_teste)){
      dados_norm_teste[i,j] = (dados_teste[i,j]-min(dados_treino[,j]))/(max(dados_treino[,j])-min(dados_treino[,j]))
    }
  }
  #plot(dados_norm, col = c("blue","red","green","magenta","black")[as.factor(classes)], pch=16, cex=1)
  
  
  
  dados_norm_valid = matrix(0, nrow(dados_valid),ncol(dados_valid))
  for (i in 1:nrow(dados_valid)){
    for(j in 1:ncol(dados_valid)){
      dados_norm_valid[i,j] = (dados_valid[i,j]-min(dados_treino[,j]))/(max(dados_treino[,j])-min(dados_treino[,j]))
    }
  }
  
}
#--------------------------------------------------------------------------------



#================================================================================
#--------------------------------------------------------------------------------

#    FUNÇÕES PARA O ESTIMADOR DAS OBSERVAÇÕES

#--------------------------------------------------------------------------------
#================================================================================

densidade_geral(dados,classes)

densidade_binario(dados,classes)

densidade_multiclasse_kde(dados,classes)

densidade_multiclasse_gde(dados,classes)

densidade_multiclasse_gde_bayes(dados,classes)

comparativo_knn_bayes(dados_norm_treino, dados_norm_teste, classes_treino, classes_teste)


melhor_n_cluster(dados_norm_treino)

n_cluster = 9

KernelBayes_Local(dados_norm_treino, dados_norm_teste, classes_treino, classes_teste, n_cluster, n_classes)

for (i in 1:10){
 n_cluster = i
 print(paste0("numero de clusters: ",n_cluster))
 KernelBayes_Local(dados_norm_treino, dados_norm_teste, classes_treino, classes_teste, n_cluster, n_classes)
}

bayes_mapa_cor(dados_norm_treino, dados_norm_teste,classes_treino, classes_teste, n_cluster=1, n_classes)

Mapa_cor_validacao(dados_norm_treino,dados_norm_teste,dados_norm_valid,classes_treino,classes_teste,classes_valid,n_cluster=1, n_classes)

#knn para divisão em 2 partes
knn_treino_teste(dados_norm_treino, dados_norm_teste,classes_treino, classes_teste)

#knn para divisão em 3 partes
knn_treino_teste(dados_norm_treino, rbind(dados_norm_teste,dados_norm_valid),classes_treino,c(classes_teste,classes_valid))
