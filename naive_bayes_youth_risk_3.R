baseyr = read.csv('YouthRisk.csv')

baseyr$X = NULL


baseyr$ride.alc.driver = factor(baseyr$ride.alc.driver, levels = c(0,1))

library(caTools)
set.seed(1)
divisao = sample.split(baseyr$ride.alc.driver, SplitRatio = 0.85)
base_treinamento = subset(baseyr, divisao == TRUE)
base_teste = subset(baseyr, divisao == FALSE)

library(e1071)

classificador = naiveBayes(x = baseyr[-1], y = baseyr$ride.alc.driver)
print(classificador)

previsoes = predict(classificador, newdata = base_teste[-1])

matriz_confusao = table(base_teste[, 1], previsoes)
print(matriz_confusao)

library(caret)
confusionMatrix(matriz_confusao)
