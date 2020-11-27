#Aula de R 2


#Extraindo base de dados 

library (readxl)

#minhabase <- read_xls ("C:/localdoarquivo/nomedabase.xls") # se for xlsx basta trocar para read_xlsx

#Se a base de dados for em R

#load ("C:/localdoarquivo/nome da base.RData" )

load("wage.RData")

wage <- ("wage.RData")

View(data)

#Inspe??o de dados

head(data) # vai ler os 6 primeiros elementos da base de dados
tail(data) #vai ler os 6 ?ltimos elementos da base de dados

head(data, n=10) # vai ler os 10 primeiros elementos da base de dados
tail(data, n=10) #vai ler os 10 ?ltimos elementos da base de dados


# Para ver os tipos das vari?veis (num?rico, texto , etc. )

str(data)

#Separando um par?metro

X <- data$educ
Y <- data$wage
View(X)
View(Y)


#Estat?sticas descritivas

length (Y) # n?mero de elementos
length (X)
max (Y) # maior valor
min(Y) # menor valor
sum (Y) # soma
mean (Y) # m?dia amostral
median (Y) # mediana amostral
var (Y) # vari?ncia amostral
sd (Y) # desvio padr?o amostral
cov (X,Y) # covari?ncia amostral
cor(X,Y) # correla??o amostral

# quantile (x,q) # quantil ( com q=0. 5 ? a mediana )

quantile (Y, 0.5)
summary (Y) #obter as estat?sticas de modo resumido



#Regress?o Simples

covxy <- cov (X,Y) #cov (x,y)
varx <- var(X) #var ( x )
ybarra <- mean (Y) #y m?dio
xbarra <- mean(X) #x m?dio
# Par?metros estimados
b1_chapeu <- covxy/varx # beta1 chapeu
b0_chapeu <- ybarra - b1_chapeu%*%xbarra # beta2 chapeu
# Vetor de y previsto
y_chapeu <- b0_chapeu + b1_chapeu%*%X
# Vetor de res?duos
u_chapeu <- Y - y_chapeu

hist(Y) #Mostra o histograma da vari?vel

# Para as 5 primeiras observa??es , por exemplo , tem???s e
y_chapeu [1:5]
u_chapeu [1:5]


#Regress?o com a fun??o LM

#modelo <- lm( formula , data = data . frame )

lm ( Y ~ X ) # mostra os par?metros estimados diretamente
regressao <- lm ( formula= wage ~ educ , data=data )
View(regressao)
print(regressao)


names(data) 
bhat <- coef(regressao) 
print(bhat)
print(bhat)
yhat <- fitted(regressao) 
print(yhat)
uhat <- resid (regressao) # extrair res?duos
print(uhat)
nobs<-(regressao)
print(nobs)



cbind(X,Y,yhat,uhat)[1 : 15,]


#Propriedades alg?bricas do MQO

mean (uhat) 
cov(X, uhat ) # covari?ncia entre x e res?duos
cor(X, uhat ) # correla??o entre x e res?duos
cov (yhat,uhat ) # covari?ncia entre y previsto e res?duos

mean(yhat)
print(ybarra)
mean(Y)


#Obter Rquadrado

# 3 formas de obter o R2 quadrado
var ( yhat ) / var ( Y )  #sqe/sqt
1 - var (uhat ) / var ( Y )    # 1 - sqr/sqt
cor (Y , yhat )^2  #correla??o ao quadrado - ao R2

#Resumindo dados da regress?o

summary(data)
