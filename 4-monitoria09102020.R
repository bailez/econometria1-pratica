library(haven)
dados<- read_dta(file= "BWGHT.dta")

#sempre lembrar de trocar os /
#quando eu copio, aparece assim:
# C:\Users\User\Desktop\econometria_renata

View(dados)

#primeiro passo. Sumarizar dados descritivos sobre vari?veis. Quais vari?veis ?
#Aquelas que nao s?o dummy e s?o explicativas do modelo.

#Separando vari?veis

faminc<- dados$faminc
print(dados$faminc)
#como extrair informa??es iniciais de estat?stica descritiva da vari?vel faminc?
summary(faminc)
#histograma da vari?vel faminc
hist(faminc)
#Explicar poss?veis pontos fora da curva com base no paper
cigprice<- dados$cigprice
summary(cigprice)
X<-summary(cigprice)
Y<-summary(faminc)
#Sumarizar todos os dados
summary(dados)


View(dados)
length(dados)

library(ggplot2)
#vamos gerar um gr?fico de pontos
qplot(x = faminc, y = cigprice, data = dados, geom = "point")


install.packages("ggplot2")
library(ggplot2)

#atributos est?ticos
#color=: altera a cor de formas que n?o t?m ?rea (pontos e retas).
#fill=: altera a cor de formas com ?rea (barras, caixas, densidades, ?reas).
#size=: altera o tamanho de formas.
#type=: altera o tipo da forma, geralmente usada para pontos.
#linetype=: altera o tipo da linha.

cigtax<-dados$cigtax

ggplot (dados)+
  geom_point(aes(x = cigtax, y = cigprice, color = "blue")) +
geom_abline(intercept = 110, slope = 1, color = "black")

ggplot(dados) + 
  geom_histogram(aes(x = cigtax), color = "black", fill = "white")

#Ver mais informa??es sobre como plotar gr?ficos em 
# https://www.curso-r.com/material/ggplot/



#Regress?o

lm ( cigprice ~ faminc + cigtax, data=dados )
regressao <- lm(cigprice ~ faminc + cigtax, data=dados)
# Resultados 
summary(regressao)
#dicas 
#analisar significancia e rela??o economica


lm ( cigprice ~ faminc + cigtax+ bwght+ fatheduc, data=dados )
regressao2 <- lm(cigprice ~ faminc + cigtax+ bwght+ fatheduc, data=dados)
# Resultados 
summary(regressao2)

#R2 ajustado aumentou. 




# Estimar betas 

bchapeu<- coef(regressao)
print(bchapeu)

uchapeu<- resid(regressao)

# Vari?ncia estimada do residuo

sigma2<-var(uchapeu)

# Vari?ncia estimada dos betas
summary(regressao)



#como fazer gr?ficos (olhar na barra de graficos)









#------####----------------

#Multivariate OLS - Partialling Out

#Passo 1) roda uma regressao de hsGPA contra ACT, e gera os residuos
regressao_1 <-lm( formula=cigtax ~faminc , data=dados)
print(regressao_1)
u_regressao_1 <- resid(regressao_1)



#Passo 2) roda uma regressao simples de colGPA contra os residuos do passo 1: qual o beta1 dessa regressao?
regressao_2 <- lm( formula=cigprice ~ u_regressao_1, data=dados)
beta_reg2<- coef(lm( formula=cigprice ~ u_regressao_1, data=dados))
print(beta_reg2)
print(bchapeu)

# agora compara esse beta 1 com o beta 1 da regress?o 2: tem que ser iguais!

#Obs: a constante entra em todas as regressoes!




#___________________
#voltando
#Definir valores cr?ticos ( parte padr?o de qualquer rotina, ou quase isso...)
# alpha
alpha <-c(0.05,0.01)
# t bicaudal com 137 graus de liberdade
qt (1-alpha/2 , 1384)
# aproxima??o normal
qnorm(1-alpha/2)
# t unicaudal
qt (1-alpha, 1384)
# F com 3 gl no numerador e 137 g l no denominador
qf (1-alpha ,3, 1384)


#vamos trabalhar com o modelo com 3 vari?veis explicativas


tabela <- summary (lm(cigprice ~ faminc + cigtax+ bwght+ fatheduc, data=dados ))

summary(tabela)

# Extrair coeficientes

dados_coef <- tabela$coefficients
View(dados_coef)
print(dados_coef)

bchapeu <- dados_coef [ , 1 ]
print(bchapeu)
ep <-dados_coef [ , 2 ]
print(ep)
exemplo<-dados_coef[1,2]
print(exemplo)

# Estat?stica t
(tcalc <- bchapeu/ep)
# pvalor
pvalor<- 2*pt(-abs(tcalc),1384)

pvalor



#intervalo de confian?a

# 95%
regressao2
confint(regressao2 )
# IC a 99 por c ento
regressao
confint(regressao, level= 0.99)


#Significancia global

#modelo restrito
print(regressao)
#modelo irrestrito
print (regressao2)

# Obter e mostrar R???quadrado
(r2ir <- summary(regressao2)$r.squared)
(r2r <- summary(regressao)$r.squared)
#teste F
( Fcalc <- (r2ir-r2r)/(1-r2ir)* 1384/3)
# pvalor
1-pf(Fcalc,3,1384)