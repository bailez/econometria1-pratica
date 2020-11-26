#monitoria 23/10/2020
library(haven)
dados<- read_dta(file= "C:/Users/User/Desktop/econometria_renata/gpa3.dta")

#sempre lembrar de trocar os /
#quando eu copio, aparece assim:
# C:\Users\User\Desktop\econometria_renata


#Se for detectada heteroscedasticidade com o uso de testes
#estatísticos, é possível estimar erros padrão robustos em
#relação à heteroscedasticidade após a estimação MQO.


#Como os erros-padrão dos estimadores MQO são baseados
#diretamente nessas variâncias, eles não mais são válidos
#para construirmos intervalos de confiança e estatísticas t


#Na presença de heteroscedasticidade, as estatísticas t não
#têm distribuições t, as estatísticas F não têm distribuição F, e
#a estatística LM não tem distribuição qui-quadrada

#Portanto, as estatísticas que usamos para testar hipóteses
#não são válidas na presença de heteroscedasticidade

#É possível ajustar erros-padrão, estatísticas t, F e LM de
#forma a torná-las válidas na presença de
#heteroscedasticidade de forma desconhecida.

#É possível então estimar variâncias consistentes na
#presença de heteroscedasticidade.

#Os erros-padrão robustos são atribuídos a White (1980).
#A estatística t robusta em relação à heteroscedasticidade é
#calculada após obter os erros-padrão robustos

#Em amostras pequenas, as estatísticas t robustas podem ter
#distribuições que não sejam próximas da distribuição t.
#Em amostras grandes, sempre podemos levar em conta
#somente os erros-padrão robustos





#Vamos testar se as interações são significantes ou não
reg<- lm(cumgpa ~female* (sat+hsperc+ tothrs), data=dados)

summary(reg)
install.packages("car")
library(car)

linearHypothesis(reg, matchCoefs(reg,"female"))

install.packages("lmteste")
library(lmteste)

reg_2<- lm(cumgpa~sat+ hsperc+ tothrs +female+ black+white, data=dados)

#analisar teste de significancia dos coeficientes
summary(reg_2)

#  Estimação da variância do coeficiente
#do OLS na presença de
#heterocedasticidade

library(sandwich)
coeftest(reg_2, vcov. = vcovHC(reg_2))


myh0<- c("black", "white")
linearHypothesis(reg_2, myh0)



#redefinindo o teste restrico contra o teste irrestrito usando o erro padrão robusto
linearHypothesis(reg_2,myh0, vcov. = vcovHC(reg_2))

#- Se for constatada que não há homoscedasticidade, os errospadrão robustos em relação à heteroscedasticidade e suas
#estatísticas de testes poderão ser utilizadas


#Teste de Breusch pagan = H0: testar se as variâncias são iguais
# contra H1: heterocedasticidade
# Possui uma distribuição qui- quadrado
# para isso é necessário analisar os graus de liberdade
library(lmtest)
bptest(reg_2)

qchisq(.95, df = 6)

#Logo existe heterocedasticidade neste modelo.

#Os estimadores de mínimos quadrados generalizados
#(MQG) para correção da heteroscedasticidade são
#chamados de estimadores de mínimos quadrados
#ponderados (MQP)ou (WLS)(mesma coisa).

#Os novos betas minimizam a soma ponderada dos
#quadrados dos resíduos.

#A ideia é colocar menos peso nas observações com uma
#variância de erro mais alta.

#O método MQO atribui pesos iguais a todas as observações,
#pois isso é melhor quando a variância do erro é idêntica para
#todas as partições da população

#Juntamente com as variáveis dependentes e independentes
#originais, especificamos a função de ponderação (1/hi).
#Especificamos pesos proporcionais ao inverso da variância.
#Isso nos permite interpretar as estimativas de mínimos
#quadrados ponderados no modelo original


# Vamos trabalhar agora com Weighted least squares
#Exemplo ficcional para fins práticos e não teóricos.

library(foreign)
wlsreg<- lm(cumgpa~sat+ hsperc+ tothrs +female+ black+white, weights = 1/sat, data = dados)
coeftest(wlsreg)
coeftest(reg_2)

coeftest(wlsreg, vcov. = vcovHC(wlsreg))



#_-------------------------------------
#usando um exemplo do livro do Florian com robustez teórica

library(foreign)
d401k <- read_dta(file= "C:/Users/User/Desktop/econometria_renata/401KSUBS.dta")


#WLS
wlsreg_d401<- lm(nettfa~inc + I(age-25) +male+ e401k,weights= 1/inc, data=d401k)
coeftest(wlsreg_d401)

#erros robustos
coeftest(wlsreg_d401,vcov. = vcovHC(wlsreg_d401))




