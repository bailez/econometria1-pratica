#Aula de R 2


#Extraindo base de dados 

install.packages("readxl")
library (readxl)

#minhabase <- read_xls ("C:/localdoarquivo/nomedabase.xls") # se for xlsx basta trocar para read_xlsx

#Se a base de dados for em R

#load ("C:/localdoarquivo/nome da base.RData" )

load("C:/Users/Guilherme/Downloads/wage.RData")

wage <- ("C:/Users/Guilherme/Downloads/wage.RData")

View(data)

#Inspeção de dados

head(data) # vai ler os 6 primeiros elementos da base de dados
tail(data) #vai ler os 6 últimos elementos da base de dados

head(data, n=10) # vai ler os 10 primeiros elementos da base de dados
tail(data, n=10) #vai ler os 10 últimos elementos da base de dados


# Para ver os tipos das variáveis (numérico, texto , etc. )

str(data)

#Separando um parâmetro

X <- data$educ
Y <- data$wage
View(X)
View(Y)


#Estatísticas descritivas

length (Y) # número de elementos
length (X)
max (Y) # maior valor
min(Y) # menor valor
sum (Y) # soma
mean (Y) # média amostral
median (Y) # mediana amostral
var (Y) # variância amostral
sd (Y) # desvio padrão amostral
cov (X,Y) # covariância amostral
cor(X,Y) # correlação amostral

# quantile (x,q) # quantil ( com q=0. 5 é a mediana )

quantile (Y, 0.5)
summary (Y) #obter as estatísticas de modo resumido



#Regressão Simples

covxy <- cov (X,Y) #cov (x,y)
varx <- var(X) #var ( x )
ybarra <- mean (Y) #y médio
xbarra <- mean(X) #x médio
# Parâmetros estimados
b1_chapeu <- covxy/varx # beta1 chapeu
b0_chapeu <- ybarra - b1_chapeu%*%xbarra # beta2 chapeu
# Vetor de y previsto
y_chapeu <- b0_chapeu + b1_chapeu%*%X
# Vetor de resíduos
u_chapeu <- Y - y_chapeu

hist(Y) #Mostra o histograma da variável

# Para as 5 primeiras observações , por exemplo , tem???s e
y_chapeu [1:5]
u_chapeu [1:5]


#Regressão com a função LM

#modelo <- lm( formula , data = data . frame )

lm ( Y ~ X ) # mostra os parâmetros estimados diretamente
regressao <- lm ( formula= wage ~ educ , data=data )
View(regressao)
print(regressao)


names(data) 
bhat <- coef(regressao) 
print(bhat)
print(bhat)
yhat <- fitted(regressao) 
print(yhat)
uhat <- resid (regressao) # extrair resíduos
print(uhat)
nobs<-(regressao)
print(nobs)



cbind(X,Y,yhat,uhat)[1 : 15,]


#Propriedades algébricas do MQO

mean (uhat) 
cov(X, uhat ) # covariância entre x e resíduos
cor(X, uhat ) # correlação entre x e resíduos
cov (yhat,uhat ) # covariância entre y previsto e resíduos

mean(yhat)
print(ybarra)
mean(Y)


#Obter Rquadrado

# 3 formas de obter o R2 quadrado
var ( yhat ) / var ( Y )  #sqe/sqt
1 - var (uhat ) / var ( Y )    # 1 - sqr/sqt
cor (Y , yhat )^2  #correlação ao quadrado - ao R2

#Resumindo dados da regressão

summary(data)
