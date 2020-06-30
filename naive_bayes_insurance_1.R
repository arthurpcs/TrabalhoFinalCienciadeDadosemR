basei = read.csv('insurance.csv')

summary(basei)

basei$smoker = factor(basei$smoker, levels = unique(basei$smoker), labels = c(0, 1))
basei$sex = factor(basei$sex, levels = unique(basei$sex), labels = c(0, 1))

unique(basei$region)

basei$region = factor(basei$region, levels = unique(basei$region), labels = c(1, 2, 3, 4))


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

library(e1071)

classificador = naiveBayes(x = basei[-5], y = basei$smoker)
print(classificador)

previsoes = predict(classificador, newdata = base_teste[-5])

matriz_confusao = table(base_teste[,5], previsoes)
print(matriz_confusao)

library(caret)
confusionMatrix(matriz_confusao)
