#Monitoria 3 de R
#Extraindo base de dados

load ( "gpa1.RData")
gpa1 <- data
View(gpa1)


# colGPA = beta0 + \beta1hsGPA + \beta2ACT + u

#Regress?o

lm ( colGPA ~ hsGPA + ACT, data=gpa1 )
regressao <- lm ( colGPA ~ hsGPA + ACT, data=gpa1 )
# Resultados 
summary(regressao)

#-------------------------------------------------
#Regress?o com um par?metro a mais. O R2 aumentou? E o R2 ajustado?

lm ( colGPA ~ hsGPA + ACT + age + alcohol, data=gpa1 )
regressao_ex <- lm ( colGPA ~ hsGPA + ACT +age + alcohol, data=gpa1 )
# Resultados 
summary(regressao_ex)
#---------------------------------------------------------


#Preliminares (muito intuito e ajuda a raciocinar)

#Tamanho da amostra
n <-nrow(gpa1)
print(n)
# vari?veis explicativas
k <- 2
# Vetor y
y <- gpa1$colGPA
# Matriz  de vari?veis explicativas
X <- cbind ( 1 , gpa1$hsGPA, gpa1$ACT)
#Linhas iniciais de X
head (X)


# Estimar betas 

bchapeu<- coef(regressao)
print(bchapeu)
uchapeu<- resid(regressao)
print(uchapeu)
sum(uchapeu)  #eu que fiz, deve ser pr?ximo de zero

# Vari?ncia estimada do residuo

sigma2<-var(uchapeu)
View(sigma2)
print(sigma2)


# Vari?ncia estimada dos betas
Variancia_beta <- var(bchapeu)
print(Variancia_beta)
ep_beta <- sqrt(var(bchapeu))
print(ep_beta)


# EP res?duo
EPR <- sqrt(sigma2)
print(ep_beta)
print(EPR)




#Problema de especifica??o

#Suponha o seguinte modelo: ychapeu = beta_chapeu0 + betachapeu1x1 + betachapeu2x2, (leia tudo em chapeu)
#Omita x2 ytilda =betatilda0 + betatilda1x1 (leia beta tilda)
#seja betatilda1= betachapeu1+ betachapeu2*deltatilda1
# x2tilda= deltatilda0+ deltatilda1*x1
#Ora, ent?o, temos que:

# Obter beta chap?u na regress?o completa
bchapeu_2 <- coef((lm(colGPA~ ACT + hsGPA, data = gpa1)))
print(bchapeu_2)
# Estimar beta1tilda
lm ( colGPA ~ ACT, data = gpa1)
#Estimar deltatilda
delta.tilda <- coef(lm(hsGPA ~ ACT, data = gpa1))
# Ent?o, temos
betatilda1<- bchapeu["ACT"] + bchapeu["hsGPA"] * delta.tilda[ "ACT" ]

print(betatilda1)
print(bchapeu)
print(delta.tilda)
print(bchapeu_2)

#------####----------------

#Multivariate OLS - Partialling Out

#Passo 1) roda uma regressao de hsGPA contra ACT, e gera os residuos
regressao_1 <-lm( formula=hsGPA ~ ACT, data=gpa1)
print(regressao_1)
u_regressao_1 <- resid(regressao_1)



#Passo 2) roda uma regressao simples de colGPA contra os residuos do passo 1: qual o beta1 dessa regressao?
regressao_2 <- lm( formula=colGPA ~ u_regressao_1, data=gpa1)
beta_reg2<- coef(lm( formula=colGPA ~ u_regressao_1, data=gpa1))
print(beta_reg2)
print(bchapeu)


# agora compara esse beta 1 com o beta 1 da regressao multipla de colGPA contra hsGPA e ACT: tem que ser iguais!

#Obs: a constante entra em todas as regressoes!
#---------------





#Segunda parte da monitoria

# instalar os pacotes necess?rios
install.packages("car")
install.packages ("stargazer")
# chamar os pacotes
library (car)
library(stargazer)

# pacote car serve para estat?ticas de teste (estat?stica F, estat?stica T, ec...)


#Definir valores cr?ticos ( parte padr?o de qualquer rotina, ou quase isso...)
# alpha
alpha <-c(0.05,0.01 )
# t bicaudal com 137 graus de liberdade
qt (1-alpha/2 , 137)
# aproxima??o normal
qnorm(1-alpha/2)
# t unicaudal
qt (1-alpha, 137)
# F com 3 gl no numerador e 137 g l no denominador
qf (1-alpha ,3, 137)


#vamos trabalhar com o modelo com 3 vari?veis explicativas


tabela <- summary (lm(colGPA ~ hsGPA + ACT + age , data=gpa1))


# Extrair coeficientes

dados <- tabela$coefficients
View(dados)
print(dados)

bchapeu <- dados [ , 1 ]
print(bchapeu)
ep <-dados [ , 2 ]
print(ep)


# Estat?stica t
(tcalc <- bchapeu/ep)
# pvalor
pvalor<- 2*pt(-abs(tcalc),137)

pvalor



#intervalo de confian?a

# 95%
regressao_ex 
confint(regressao_ex )
# IC a 99 por c ento
confint(regressao_ex, level= 0.99)


#Significancia global

#modelo restrito
print(regressao)
#modelo irrestrito
print (regressao_ex)

# Obter e mostrar R???quadrado
(r2ir <- summary(regressao_ex)$r.squared)
(r2r <- summary(regressao)$r.squared)
#teste F
( Fcalc <- (r2ir-r2r)/(1-r2ir)* 137/3)
# pvalor
1-pf(Fcalc,3,137)



#pacote car

# H0
H0 <-c("hsGPA=0","ACT=0", "age=0" )
H0 <-c("hsGPA=0") # testes meus

linearHypothesis(regressao_ex,H0 )

#Pacote stargazer

mod1<-lm(colGPA ~ hsGPA, data = gpa1 )
mod2 <- lm(colGPA ~ hsGPA + ACT, data = gpa1 )
mod3 <- lm(colGPA ~ hsGPA + ACT + age ,data = gpa1 )

stargazer (list(mod1,mod2,mod3),type="text",keep.stat=c("n","rsq"))                                                                                                " r sq " ) )














