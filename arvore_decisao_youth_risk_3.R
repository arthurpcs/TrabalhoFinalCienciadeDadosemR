baseyr = read.csv('YouthRisk.csv')

baseyr$X = NULL

#para trabalharmos com árvore de decisão
#install.packages('rpart') 
library(rpart)

# A função rpart recebe uma fórmula indicando quem são 
# os atributos previsores e o atibuto alvo (classe), 
# além de receber um argumento data que indica o banco de dados utilizado
# o símbolo ~ é usado para representar como se fosse uma concatenação
# não é preciso colocar os outros atributos, basta colocar 
# o ponto (ele representa todos os outros atributos)
classificador = rpart(formula = ride.alc.driver ~ ., data = baseyr)
classificador = rpart(formula = ride.alc.driver ~ ., data = baseyr, control = rpart.control(minbucket = 1))
# Quando a árvore é muito pequena, geralmente ocorre uma poda quando 
# se usa split ou rpart. Para se evitar isso e que seja gerada a árvore, 
# usamos o parâmetro rpart.control para que, mesmo que
# a base seja pequena, seja gerado os nós (os splits)

print(classificador)
plot(classificador)
text(classificador)

# Um pacote que é interessante para visualizar a árvore de decisão 
# construída com o rpart é o rpart.plot
#install.packages('rpart.plot')
library(rpart.plot)
rpart.plot(classificador)

# história: boa, dívida: alta, garantias: nenhuma, renda: >35
# hitóstia: ruim, dívida: alta, garantias: adequada, renda: <15
female = c( 1,  1)
grade = c( 10,  10)
age4 = c(15,  18)
smoke = c(1,  1)
DriverLicense = c(0, 1)
df = data.frame(female, grade, age4, smoke, DriverLicense)

previsoes = predict(classificador, newdata = df)
print(previsoes)








