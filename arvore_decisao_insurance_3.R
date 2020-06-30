basei = read.csv('insurance.csv')

basei$smoker = as.numeric(factor(basei$smoker, levels = unique(basei$smoker), labels = c(2, 1)))
basei$sex = as.numeric(factor(basei$sex, levels = unique(basei$sex), labels = c(2, 1)))

unique(basei$region)

basei$region = as.numeric(factor(basei$region, levels = unique(basei$region), labels = c(1, 2, 3, 4)))

transform(basei, smoker = as.numeric(smoker))
transform(basei, sex = as.numeric(sex))
transform(basei, region = as.numeric(region))




library(rpart)

# A função rpart recebe uma fórmula indicando quem são 
# os atributos previsores e o atibuto alvo (classe), 
# além de receber um argumento data que indica o banco de dados utilizado
# o símbolo ~ é usado para representar como se fosse uma concatenação
# não é preciso colocar os outros atributos, basta colocar 
# o ponto (ele representa todos os outros atributos)
classificador = rpart(formula = smoker ~ ., data = basei)
classificador = rpart(formula = smoker ~ ., data = basei, control = rpart.control(minbucket = 5))
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
age = c(19,  21)
sex = c( 1,  2)
bmi = c( 27.740, 26.220)
children = c( 1,  3)
region = c(3, 1)
charges = c(7280.873, 1826.983)
df = data.frame(age, sex, bmi, children, region, charges)

previsoes = predict(classificador, newdata = df)
print(previsoes)