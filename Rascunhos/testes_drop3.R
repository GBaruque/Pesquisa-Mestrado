install.packages("NoiseFiltersR")

require ("NoiseFiltersR")

isequal = vector("list", 20)

data(iris)
trainData <- iris[c(1:20,51:70,101:120),]
testData <- iris[-c(1:20,51:70,101:120),]
out1 <- DROP1(Species~ Petal.Length + Petal.Width, data = trainData, k = 3)
out2 <- DROP2(Species~ Petal.Length + Petal.Width, data = trainData, k = 3)
out3 <- DROP3(Species~ Petal.Length + Petal.Width, data = trainData, k = 3)
summary(out3, explicit = TRUE)

identical(out1$cleanData, trainData[setdiff(1:nrow(trainData),out1$remIdx),])


#testes

fullknn = knn(trainData[,-5], testData[,-5], trainData[,5], k = 3, l = 0, prob = TRUE, use.all = TRUE)
CMfullknn = confusionMatrix(fullknn, testData[,5])
print(CMfullknn$table)


out1knn = knn(out1$cleanData[,-5], testData[,-5], out1$cleanData[,5], k = 3, l = 0, prob = TRUE, use.all = TRUE)
CMout1knn = confusionMatrix(out1knn, testData[,5])
print(CMout1knn$table)


out2knn = knn(out2$cleanData[,-5], testData[,-5], out2$cleanData[,5], k = 3, l = 0, prob = TRUE, use.all = TRUE)
CMout2knn = confusionMatrix(out2knn, testData[,5])
print(CMout2knn$table)


out3knn = knn(out3$cleanData[,-5], testData[,-5], out3$cleanData[,5], k = 3, l = 0, prob = TRUE, use.all = TRUE)
CMout3knn = confusionMatrix(out3knn, testData[,5])
print(CMout3knn$table)
