basei = read.csv('insurance.csv')

summary(basei)

basei$smoker = as.numeric(factor(basei$smoker, levels = unique(basei$smoker), labels = c(2, 1)))
basei$sex = as.numeric(factor(basei$sex, levels = unique(basei$sex), labels = c(2, 1)))

unique(basei$region)

basei$region = as.numeric(factor(basei$region, levels = unique(basei$region), labels = c(1, 2, 3, 4)))


# calcular a m�dia das idades sem NA e Negativos e substituir os campos negativos
mean(basei$age [basei$age > 0], na.rm = TRUE)

#Seta a m�dia das idades nos Not Availables
basei$age = ifelse(is.na(basei$age), mean(basei$age [basei$age > 0], na.rm = TRUE), basei$age)

basei[, 1] = scale(basei[, 1])
basei[, 3] = scale(basei[, 3])
basei[, 7] = scale(basei[, 7])

transform(basei, smoker = as.numeric(smoker))
transform(basei, sex = as.numeric(sex))
transform(basei, region = as.numeric(region))




library(rpart)

# A fun��o rpart recebe uma f�rmula indicando quem s�o 
# os atributos previsores e o atibuto alvo (classe), 
# al�m de receber um argumento data que indica o banco de dados utilizado
# o s�mbolo ~ � usado para representar como se fosse uma concatena��o
# n�o � preciso colocar os outros atributos, basta colocar 
# o ponto (ele representa todos os outros atributos)
classificador = rpart(formula = smoker ~ ., data = basei)
classificador = rpart(formula = smoker ~ ., data = basei, control = rpart.control(minbucket = 5))
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
age = c(1,  1.8)
sex = c( 1,  2)
bmi = c( -2,  3)
children = c( 1,  3)
region = c(3, 1)
charges = c(-1, 4)
df = data.frame(age, sex, bmi, children, region, charges)

previsoes = predict(classificador, newdata = df)
print(previsoes)

