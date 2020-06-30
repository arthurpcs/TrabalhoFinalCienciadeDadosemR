baseyr = read.csv('YouthRisk.csv')

baseyr$X = NULL

summary(baseyr)

#  calcular a média das idades sem NA e Negativos e substituir os campos negativos
mean(baseyr$age4 [baseyr$age4 > 0], na.rm = TRUE)

#Seta a média das idades nos Not Availables
baseyr$age4 = ifelse(is.na(baseyr$age4), mean(baseyr$age4,na.rm = TRUE), baseyr$age4)

mean(baseyr$smoke [baseyr$smoke > 0], na.rm = TRUE)
baseyr$smoke = ifelse(is.na(baseyr$smoke),1, baseyr$smoke)

mean(baseyr$DriverLicense [baseyr$DriverLicense > 15], na.rm = TRUE)
baseyr$DriverLicense = ifelse(is.na(baseyr$DriverLicense > 15),1, baseyr$DriverLicense)
baseyr$DriverLicense = ifelse(is.na(baseyr$DriverLicense < 15),0, baseyr$DriverLicense)

mean(baseyr$female [baseyr$female > 0], na.rm = TRUE)
baseyr$female = ifelse(is.na(baseyr$female),1, baseyr$female)

mean(baseyr$grade [baseyr$grade > 0], na.rm = TRUE)
baseyr$grade = ifelse(is.na(baseyr$grade),mean(baseyr$grade,na.rm = TRUE), baseyr$grade)

#Escalona colunas específicas
#base[, 1] = scale(base[, 1])
#base[, 3] = scale(base[, 3])
#Escalona atributos das colunas 3 a 4
baseyr[ , 3:4] = scale(baseyr[ , 3:4])

baseyr$ride.alc.driver = factor(baseyr$ride.alc.driver, levels = c(0,1))
#install.packages("tidyverse")

library(caTools)
set.seed(1)
divisao = sample.split(baseyr$smoke, SplitRatio = 0.75)
base_treinamento = subset(baseyr, divisao == TRUE)
base_teste = subset(baseyr, divisao == FALSE)

library(h2o)

# método para realizar a conexão com o servidor dedicado
# vamos usar o argumento nthreads Número de threads no conjunto de encadeamentos. 
# Isso se relaciona ao número de CPUs usadas. -1 significa usar todas as CPUs no host(padrão). 
# Um inteiro positivo especifica o número de CPUs diretamente. 
# Este valor é usado somente quando R inicia H2O.
h2o.init(nthreads = -1)

# vamos usar o método h2o.deeplearning. O primeiro parâmetro é o y, que vai receber 
# a variável resposta (classe) da base (default). O parâmetro é a base de treinamento
# como ela está no padrão do R, precisamos transformá-la no padrão h2o (as.h2o)
# próximo argumento é a função de ativação, aqui vamos usar a reLU
# hidden (camada escondida) colocamos em formato de vetor quantas camadas queremos
# se colocarmos o valor 100, significa que termos uma camada oculta com 100 neurônios
# se colocarmos (100, 100) significa que termos duas camadas ocultas e cada uma terá 100 neurônios
# epochs (épocas) que é o training time, isto é quantas vezes vc vai fazer o ajuste de pesos
# install.packages('bit64')
# install.packages('data.table')
# data.table :: update.dev.pkg ()

classificador = h2o.deeplearning(y = 'ride.alc.driver',
                                 training_frame = as.h2o(base_treinamento),
                                 activation = 'Rectifier',
                                 hidden = c(80, 160),
                                 epochs = 500)
previsoes = h2o.predict(classificador, newdata = as.h2o(base_teste[-1]))
previsoes = previsoes$predict

previsoes = (previsoes > 0.5)

previsoes = as.vector(previsoes)
matriz_confusao = table(base_teste[, 1], previsoes)
library(caret)
confusionMatrix(matriz_confusao)

# ZeroR 
table(base_teste$default)