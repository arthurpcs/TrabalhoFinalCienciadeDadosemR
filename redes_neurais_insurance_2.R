basei = read.csv('insurance.csv')

summary(basei)

basei$smoker = as.numeric(factor(basei$smoker, levels = unique(basei$smoker), labels = c(1, 2)))
basei$sex = as.numeric(factor(basei$sex, levels = unique(basei$sex), labels = c(1, 2)))

unique(basei$region)

basei$region = as.numeric(factor(basei$region, levels = unique(basei$region), labels = c(1, 2, 3, 4)))


#Seta a média das idades nos Not Availables
basei$age = ifelse(is.na(basei$age), mean(basei$age [basei$age > 0], na.rm = TRUE), basei$age)


library(caTools)
set.seed(1)
divisao = sample.split(basei$sex, SplitRatio = 0.85)
base_treinamento = subset(basei, divisao == TRUE)
base_teste = subset(basei, divisao == FALSE)

library(h2o)

h2o.init(nthreads = -1)

classificador = h2o.deeplearning(y = 'smoker',
                                 training_frame = as.h2o(base_treinamento),
                                 activation = 'Rectifier',
                                 hidden = c(80, 160),
                                 epochs = 1000)
previsoes = h2o.predict(classificador, newdata = as.h2o(base_teste[-5]))

previsoes = (previsoes > 0.5)

previsoes = as.vector(previsoes)
matriz_confusao = table(base_teste[, 5], previsoes)
library(caret)
confusionMatrix(matriz_confusao)