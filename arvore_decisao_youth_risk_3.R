baseyr = read.csv('YouthRisk.csv')

baseyr$X = NULL

#para trabalharmos com �rvore de decis�o
#install.packages('rpart') 
library(rpart)

# A fun��o rpart recebe uma f�rmula indicando quem s�o 
# os atributos previsores e o atibuto alvo (classe), 
# al�m de receber um argumento data que indica o banco de dados utilizado
# o s�mbolo ~ � usado para representar como se fosse uma concatena��o
# n�o � preciso colocar os outros atributos, basta colocar 
# o ponto (ele representa todos os outros atributos)
classificador = rpart(formula = ride.alc.driver ~ ., data = baseyr)
classificador = rpart(formula = ride.alc.driver ~ ., data = baseyr, control = rpart.control(minbucket = 1))
# Quando a �rvore � muito pequena, geralmente ocorre uma poda quando 
# se usa split ou rpart. Para se evitar isso e que seja gerada a �rvore, 
# usamos o par�metro rpart.control para que, mesmo que
# a base seja pequena, seja gerado os n�s (os splits)

print(classificador)
plot(classificador)
text(classificador)

# Um pacote que � interessante para visualizar a �rvore de decis�o 
# constru�da com o rpart � o rpart.plot
#install.packages('rpart.plot')
library(rpart.plot)
rpart.plot(classificador)

# hist�ria: boa, d�vida: alta, garantias: nenhuma, renda: >35
# hit�stia: ruim, d�vida: alta, garantias: adequada, renda: <15
female = c( 1,  1)
grade = c( 10,  10)
age4 = c(15,  18)
smoke = c(1,  1)
DriverLicense = c(0, 1)
df = data.frame(female, grade, age4, smoke, DriverLicense)

previsoes = predict(classificador, newdata = df)
print(previsoes)








