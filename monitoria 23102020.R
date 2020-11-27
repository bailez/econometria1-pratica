#monitoria 23/10/2020
library(haven)
dados<- read_dta(file= "gpa3.dta")

#sempre lembrar de trocar os /
#quando eu copio, aparece assim:
# C:\Users\User\Desktop\econometria_renata


#Se for detectada heteroscedasticidade com o uso de testes
#estat?sticos, ? poss?vel estimar erros padr?o robustos em
#rela??o ? heteroscedasticidade ap?s a estima??o MQO.


#Como os erros-padr?o dos estimadores MQO s?o baseados
#diretamente nessas vari?ncias, eles n?o mais s?o v?lidos
#para construirmos intervalos de confian?a e estat?sticas t


#Na presen?a de heteroscedasticidade, as estat?sticas t n?o
#t?m distribui??es t, as estat?sticas F n?o t?m distribui??o F, e
#a estat?stica LM n?o tem distribui??o qui-quadrada

#Portanto, as estat?sticas que usamos para testar hip?teses
#n?o s?o v?lidas na presen?a de heteroscedasticidade

#? poss?vel ajustar erros-padr?o, estat?sticas t, F e LM de
#forma a torn?-las v?lidas na presen?a de
#heteroscedasticidade de forma desconhecida.

#? poss?vel ent?o estimar vari?ncias consistentes na
#presen?a de heteroscedasticidade.

#Os erros-padr?o robustos s?o atribu?dos a White (1980).
#A estat?stica t robusta em rela??o ? heteroscedasticidade ?
#calculada ap?s obter os erros-padr?o robustos

#Em amostras pequenas, as estat?sticas t robustas podem ter
#distribui??es que n?o sejam pr?ximas da distribui??o t.
#Em amostras grandes, sempre podemos levar em conta
#somente os erros-padr?o robustos





#Vamos testar se as intera??es s?o significantes ou n?o
reg<- lm(cumgpa ~female* (sat + hsperc + tothrs), data=dados)

summary(reg)

library(car)

linearHypothesis(reg, matchCoefs(reg,"female"))

library(lmtest)

reg_2<- lm(cumgpa~sat+ hsperc+ tothrs +female+ black+white, data=dados)

#analisar teste de significancia dos coeficientes
summary(reg_2)

#  Estima??o da vari?ncia do coeficiente
#do OLS na presen?a de
#heterocedasticidade

coeftest(reg_2, vcov. = vcovHC(reg_2))


myh0<- c("black", "white")
linearHypothesis(reg_2, myh0)



#redefinindo o teste restrico contra o teste irrestrito usando o erro padr?o robusto
linearHypothesis(reg_2,myh0, vcov. = vcovHC(reg_2))

#- Se for constatada que n?o h? homoscedasticidade, os errospadr?o robustos em rela??o ? heteroscedasticidade e suas
#estat?sticas de testes poder?o ser utilizadas


#Teste de Breusch pagan = H0: testar se as vari?ncias s?o iguais
# contra H1: heterocedasticidade
# Possui uma distribui??o qui- quadrado
# para isso ? necess?rio analisar os graus de liberdade
library(lmtest)
bptest(reg_2)

qchisq(.95, df = 6)

#Logo existe heterocedasticidade neste modelo.

#Os estimadores de m?nimos quadrados generalizados
#(MQG) para corre??o da heteroscedasticidade s?o
#chamados de estimadores de m?nimos quadrados
#ponderados (MQP)ou (WLS)(mesma coisa).

#Os novos betas minimizam a soma ponderada dos
#quadrados dos res?duos.

#A ideia ? colocar menos peso nas observa??es com uma
#vari?ncia de erro mais alta.

#O m?todo MQO atribui pesos iguais a todas as observa??es,
#pois isso ? melhor quando a vari?ncia do erro ? id?ntica para
#todas as parti??es da popula??o

#Juntamente com as vari?veis dependentes e independentes
#originais, especificamos a fun??o de pondera??o (1/hi).
#Especificamos pesos proporcionais ao inverso da vari?ncia.
#Isso nos permite interpretar as estimativas de m?nimos
#quadrados ponderados no modelo original


# Vamos trabalhar agora com Weighted least squares
#Exemplo ficcional para fins pr?ticos e n?o te?ricos.

library(foreign)
wlsreg<- lm(cumgpa~sat+ hsperc+ tothrs +female+ black+white, weights = 1/sat, data = dados)
coeftest(wlsreg)
coeftest(reg_2)

coeftest(wlsreg, vcov. = vcovHC(wlsreg))



#_-------------------------------------
#usando um exemplo do livro do Florian com robustez te?rica

library(foreign)
d401k <- read_dta(file= "C:/Users/User/Desktop/econometria_renata/401KSUBS.dta")


#WLS
wlsreg_d401<- lm(nettfa~inc + I(age-25) +male+ e401k,weights= 1/inc, data=d401k)
coeftest(wlsreg_d401)

#erros robustos
coeftest(wlsreg_d401,vcov. = vcovHC(wlsreg_d401))




