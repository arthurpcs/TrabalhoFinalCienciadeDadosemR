baseyr = read.csv('YouthRisk.csv')

baseyr$X = NULL

summary(baseyr)

baseyr$ride.alc.driver = factor(baseyr$ride.alc.driver, levels = c(0,1))
#install.packages("tidyverse")

library(caTools)
set.seed(1)
divisao = sample.split(baseyr$smoke, SplitRatio = 0.75)
base_treinamento = subset(baseyr, divisao == TRUE)
base_teste = subset(baseyr, divisao == FALSE)

library(h2o)

# m�todo para realizar a conex�o com o servidor dedicado
# vamos usar o argumento nthreads N�mero de threads no conjunto de encadeamentos. 
# Isso se relaciona ao n�mero de CPUs usadas. -1 significa usar todas as CPUs no host(padr�o). 
# Um inteiro positivo especifica o n�mero de CPUs diretamente. 
# Este valor � usado somente quando R inicia H2O.
h2o.init(nthreads = -1)

# vamos usar o m�todo h2o.deeplearning. O primeiro par�metro � o y, que vai receber 
# a vari�vel resposta (classe) da base (default). O par�metro � a base de treinamento
# como ela est� no padr�o do R, precisamos transform�-la no padr�o h2o (as.h2o)
# pr�ximo argumento � a fun��o de ativa��o, aqui vamos usar a reLU
# hidden (camada escondida) colocamos em formato de vetor quantas camadas queremos
# se colocarmos o valor 100, significa que termos uma camada oculta com 100 neur�nios
# se colocarmos (100, 100) significa que termos duas camadas ocultas e cada uma ter� 100 neur�nios
# epochs (�pocas) que � o training time, isto � quantas vezes vc vai fazer o ajuste de pesos
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