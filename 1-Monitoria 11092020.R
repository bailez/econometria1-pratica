#Primeiros atalhos

# Ct r l + R: executa a(s) l i n h a (s) selecionada (s) do script
# Ct r l + A: seleciona todas as linhas do script
# Ct r l + L: limpa o console
#ctrl+ S: salvar
# Esc interrompe execu??o

#Instalar pacotes
#install.packages ("nome do pacote")
#library(nome pacote)

library(ggplot2)


#R como calculadora

1+4
10-9
2*3
6/3


# exponencial
2^3
# e^v

exp(4)
# raiz quadrada
sqrt(16)

# valor absoluto
abs(6)

# log natural de x
log (10)

# log de 8 na base 2
log (8,2)



#Criar Objeto

# gerar o objeto x = 5
X <- 5
# mostrar o valor de x
X
View(X)
print(X)
# gerar e mostrar y = 7
( y <- 7)

# criar um vetor a com valores de 1 a 6

a <- c(1,2,3,4,5,6)
a
# fazer opera??es elemento a elemento com o vetor
b <- a+1
View(b)
c <- a+b
d <- b*c
sqrt (d)


#criar o vetor v
v <- c (10, 9 , 8 , 5 , 3 , 1 , 2 , 6 , 4 , 7)

View(v)
# n?mero de e lementos
length(v)
# maior valor
max(v)
# menor v a l o r
min (v)
# ordenar elementos
sort (v)
# soma dos e lementos
sum (v)
# produto dos elementos
prod (v)



# gerar vetor com n ( ex : 5) zeros: numeric (n)
numeric (5)
# gerar vetor com n elementos iguais a z ( ex : 5 elementos iguais a 2)

rep (2, 5)

# criar vetor com sequ?ncia de n?meros: seq ( t ) ou seq ( 1 : t ) , i e , 1 , 2 , . . . , t
seq (10)
seq ( 1 : 10)
# seq ( f , t ) : f , f+1 , f+2 , . . . , t
seq ( 2 , 7 )
# seq ( f , b , s ) : f , f+s , . . . , t . Ex : de 3 a 30 , de dois em dois
seq ( 3 , 30 , 2*1 )

# vetor com textos
cidades <- c ( "Belo Horizonte" , "Governador Valadares " , " Ipatinga " )
View(cidades)

#Operadores l?gicos no R

# gerar o objeto x = 5
x <- 5
# mostrar o valor de x
x
View(X)
print(X)
# gerar e mostrar y = 7
( y <- 7)

# x igual a y
x == y
# x diferente de y
x != y
# x menor que y
x<y
# x maior que y
x>y
# x menor que ou igual a y
x <= y
# x maior que ou igual a y
x >= y
# n?o ? b ( i e , b ? falso )
! b
# ou a ou b ? ve rdade i r o ( ou ambos )
a | b
# a e b s ?o ambos v e rdade i r o s
a&b

# Vetor categ?rico
x <- c ( 1 , 2 , 3 , 4 , 5 )

meunome <- c ( " discordo totalmente " , " discordo " , "n?o concordo nem discordo
" , " concordo " , " concordo totalmente " )

# Nomear os elementos do vetor x
names ( x ) <- meunome
x
View(x)


# pelo n?mero do elemento
cidades <- c ( "Belo Horizonte " , "Governador Valadares " , " Ipatinga " )
cidades [ 1 ]
cidades [ 2 ]
cidades [ 2 : 3 ]


# definir vetor "ano"
ano <- c (2008 , 2009 , 2010 ,2011 ,2012 ,2013)
# definir matriz de valores
produto1 <- c ( 0 , 3 , 6 , 9 , 7 , 8 )
produto2 <- c ( 1 , 2 , 3 , 5 , 9 , 6 )
produto3 <- c ( 2 , 4 , 4 , 2 , 3 , 2 )
vendas <- cbind ( produto1 , produto2 , produto3 )
View(vendas)
rownames (vendas) <- ano
# mostrar a mat r iz
vendas
# criar um data.frame e mostrar

dadosvendas <- as.data.frame(vendas)
dadosvendas







# gerar e salvar matriz A de m=2 linhas apartir de um vetor v= vetor
vetor <- c ( 2 ,4,1,5,7,0)
View(vetor)

( A <- matrix ( vetor,nrow=2) )


# gerar e salvar matriz a partir de vetores que correspondem ?s linhas

linha1 <- c ( 2 , 3 , 4 )
linha2 <- c ( 4 , 5 , 6 )
econometria <- c (0, 8, 9 )
B <- rbind(linha1, econometria, linha2 ) 
B


# gera r e salvar matriz a partir de vetores que correspondem ?s colunas
coluna1 <- c ( 1 , 2 )
coluna2 <- c ( 3 , 4 )
coluna3 <- c ( 5 , 6 )

(C<- cbind ( coluna1 , coluna3 ,coluna2  ) )
View(C)

# nomear linhas e colunas
colnames(C)<- c("x","y","z")
rownames (C)<- c ("a","b" )
C



# matriz diagonal
diag ( c ( 4 , 2 , 6 ) )


# mat r iz identidade
diag ( 3 )

# extrair elementos da linha2 , coluna 1
C[2 , 1]
# extrair elementos da linha2 , todas as colunas
C[ , 2 ]
# extrair elementos da linha2 , colunas 1 e 3
C[ , c ( 1 , 3 ) ]



# obte r uma ?nica vari?vel
View(dadosvendas)
dadosvendas [ ,"produto2"]

# gerar vendas totais
dadosvendas$totalv1<- dadosvendas$produto1 + dadosvendas$produto2 +
  dadosvendas$produto3
dadosvendas$totalv1
View(dadosvendas$totalv1)
dadosvendas$produto1
View(produto1)
dadosvendas$produto1
# g e r a r vendas t o t a i s ( out ra forma )

dadosvendas$totalv2<- with (dadosvendas,produto1 + produto2 + produto3 )
dadosvendas

# obter um subconjunto dos dados (crit?rio l?gico vale para linhas )
subset( dadosvendas , produto2>=3)


# definir as matrizes A e B a seguir :

A <- matrix (c (2,4,1,5,7,0 ),nrow=2)
B <- matrix (c(2,1,9,3,1,5) , nrow=2)
# ver as matriz escriadas
A
B
# somar elemento a elemento
A + B
# multiplicar elemento a elemento
A*B
# transposta
C <- t (B)
C
# multiplica??o matricial
D <- A %*% C
D
# inversa
solve (D)







