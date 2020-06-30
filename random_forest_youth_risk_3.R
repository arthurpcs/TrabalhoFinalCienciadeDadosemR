baseyr = read.csv('YouthRisk.csv')

baseyr$X = NULL

summary(baseyr)

baseyr$ride.alc.driver = factor(baseyr$ride.alc.driver, levels = c(0,1))

library(caTools)
set.seed(1)
divisao = sample.split(baseyr$ride.alc.driver, SplitRatio = 0.85)
base_treinamento = subset(baseyr, divisao == TRUE)
base_teste = subset(baseyr, divisao == FALSE)


library(randomForest)
set.seed(1)

classificador = randomForest(x = base_treinamento[-1], y = base_treinamento$ride.alc.driver, ntree = 6)
print(classificador)

previsoes = predict(classificador, newdata = base_teste[-1])

matriz_confusao = table(base_teste[, 1], previsoes)
print(matriz_confusao)

library(caret)
confusionMatrix(matriz_confusao)
