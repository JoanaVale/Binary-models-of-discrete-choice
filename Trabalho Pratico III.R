
rm(list = ls())

load("C:/Users/Utilizador/Desktop/Metodos Quantitativos/Trabalho 3/credito.RData")

suppressMessages(library(wooldridge))
str(credito)

attach(credito)

######     A  

# -> Estime um modelo de probabilidade linear

fitpl <- lm(atraso ~ racio + tipo + segur + taxa + mont + score + prazo + var)
summary(fitpl)

# -> Os sinais dos coeficientes estimados sao os especaveis (use erros-padrao robustos)

suppressMessages(library(lmtest))
suppressMessages(library(car))
coeftest(fitpl, vcov=hccm)




######    B 

# Utilize um modelo probit para estimar o modelo em (a).
# Estimação MV do modelo probit:

fitprobit <- glm(atraso ~ racio + tipo + segur + taxa + mont + score + prazo + var, family=binomial(link=probit))
summary(fitprobit)


suppressMessages(library(stargazer))
stargazer(list(fitpl, fitprobit), type = "text")


######    C
# so para a quingentesima (500) e milesima (1000) observacoes
# da amostra usando o modelo de probabilidade linear e o modelo de probit
# Comente os valores obtidos.


xpred1 <- list(racio=c(credito[500,2],credito[1000,2]) , tipo=c(credito[500,3],credito[1000,3]) , segur=c(credito[500,4],credito[1000,4]) , taxa=c(credito[500,5],credito[1000,5]) , mont=c(credito[500,6],credito[1000,6]) , score=c(credito[500,7],credito[1000,7]) , prazo=c(credito[500,8],credito[1000,8]) , var=c(credito[500,9],credito[1000,9]))

predict(fitpl, xpred1)
predict(fitprobit, xpred1)


credito[500,]
credito[1000,]
xpred <- list(racio=c(credito[500,2],credito[1000,2]) , tipo=c(credito[500,3],credito[1000,3]) , segur=c(credito[500,4],credito[1000,4]) , taxa=c(credito[500,5],credito[1000,5]) , mont=c(credito[500,6],credito[1000,6]) , score=c(credito[500,7],credito[1000,7]) , prazo=c(credito[500,8],credito[1000,8]) , var=c(credito[500,9],credito[1000,9]))
predict(fitpl, xpred)
predict(fitprobit,xpred, type = "response")



####    D

# Construa um histograma do score

hist(score)


# Usando o modelo de probabilidade linear e o modelo probit,
# calcule a probabilidade de atraso para um score de 500, 600 e 700
# um credito de 250.000 dolares (mont = 2.5).
# Em relacao as restantes variaveis considere, um racio de 80%, uma taxa
# de 8%, um prazo de 30 anos e as variaveis dummy iguais a 1. 


# modelo de probabilidade linear para score de 500, 600 e 700
xpred2 <-list(racio=c(80,80,80) , tipo=c(1,1,1) , segur=c(1,1,1) , taxa=c(8,8,8) , mont=c(2.5,2.5,2.5) , score=c(500,600,700) , prazo=c(30,30,30) , var=c(1,1,1))

predict(fitpl, xpred2)
predict(fitprobit, xpred2,type = "response")


#####  E

# Calcule o efeito marginal do score sobre a probabilidade de atraso:
# para scores de 500, 600 e 700 
# considerando os valores das restantes variaveis explicativas indicados em (d). 
# Discuta a interpretação do efeito marginal.

# install.packages("mfx")
suppressMessages(library(mfx))



coefficients(fitprobit)[6]*dnorm(predict(fitprobit, xpred2))

probitmfx(atraso ~ racio + tipo + segur + taxa + mont + score + prazo + var, data = credito, atmean=FALSE)

predict(fitprobit, xpred2, type="response")[2] - predict(fitprobit, xpred2, type="response")[1]

predict(fitprobit, xpred2, type="response")[3] - predict(fitprobit, xpred2, type="response")[2]


###### F

# histograma do racio.
hist(racio)

# modelo de probabilidade linear para score de 500, 600 e 7000
xpred3 <-list(racio=c(20,60) , tipo=c(1,1) , segur=c(1,1) , taxa=c(8,8) , mont=c(2.5,2.5) , score=c(600,600) , prazo=c(30,30) , var=c(1,1))

predict(fitpl, xpred3)
predict(fitprobit, xpred3,type = "response")


####### G

predpl1 <- predict(fitpl)
predplbin1 <- ifelse((predpl1<0.5),0,1)
prop1 <- ifelse((credito[ ,1]==predplbin1),1,0)
table(prop1/length(prop1)*100)

predpl2 <- predict(fitprobit)
predplbin2 <- ifelse((predpl2<0.5),0,1)
prop2 <- ifelse((credito[ ,1]==predplbin2),1,0)
table(prop2/length(prop2)*100)
