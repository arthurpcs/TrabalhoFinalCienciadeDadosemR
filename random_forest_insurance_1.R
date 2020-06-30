basei = read.csv('insurance.csv')
summary(basei)

basei$smoker = factor(basei$smoker, levels = unique(basei$smoker), labels = c(0, 1))


# calcular a média das idades sem NA e Negativos e substituir os campos negativos
mean(basei$age [basei$age > 0], na.rm = TRUE)

#Seta a média das idades nos Not Availables
basei$age = ifelse(is.na(basei$age), mean(basei$age [basei$age > 0], na.rm = TRUE), basei$age)

basei[, 1] = scale(basei[, 1])
basei[, 3] = scale(basei[, 3])
basei[, 4] = scale(basei[, 4])
basei[, 7] = scale(basei[, 7])

library(caTools)
set.seed(1)
divisao = sample.split(basei$smoker, SplitRatio = 0.85)
base_treinamento = subset(basei, divisao == TRUE)
base_teste = subset(basei, divisao == FALSE)
library(randomForest)
set.seed(1)

classificador = randomForest(x = base_treinamento[-5], y = base_treinamento$smoke, ntree = 6)
print(classificador)

previsoes = predict(classificador, newdata = base_teste[-5])

matriz_confusao = table(base_teste[, 5], previsoes)
print(matriz_confusao)

library(caret)
confusionMatrix(matriz_confusao)