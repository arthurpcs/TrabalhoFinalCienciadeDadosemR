baseyr = read.csv('YouthRisk.csv')

baseyr$X = NULL

summary(baseyr)

#  calcular a m�dia das idades sem NA e Negativos e substituir os campos negativos
mean(baseyr$age4 [baseyr$age4 > 0], na.rm = TRUE)

#Seta a m�dia das idades nos Not Availables
baseyr$age4 = ifelse(is.na(baseyr$age4), 16, baseyr$age4)

mean(baseyr$smoke [baseyr$smoke > 0], na.rm = TRUE)
baseyr$smoke = ifelse(is.na(baseyr$smoke),1, baseyr$smoke)

mean(baseyr$DriverLicense [baseyr$DriverLicense > 15], na.rm = TRUE)
baseyr$DriverLicense = ifelse(is.na(baseyr$DriverLicense > 15),1, baseyr$DriverLicense)
baseyr$DriverLicense = ifelse(is.na(baseyr$DriverLicense < 15),0, baseyr$DriverLicense)

mean(baseyr$female [baseyr$female > 0], na.rm = TRUE)
baseyr$female = ifelse(is.na(baseyr$female),1, baseyr$female)

mean(baseyr$grade [baseyr$grade > 0], na.rm = TRUE)
baseyr$grade = ifelse(is.na(baseyr$grade),10, baseyr$grade)

baseyr$ride.alc.driver = factor(baseyr$ride.alc.driver, levels = c(0,1))
#install.packages("tidyverse")

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