#Codigo aula 17/3 ANADI - Final TP2 + TP3

# TP2
###9) 
library(readr)
Data_D <- read_delim("C:/Users/lolas/Downloads/Data_D.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)
View(Data_D)
#Pressepostos ANOVA - vamos assumir
#variável resistencia com dist normal
#homogeneidade de variancia
#amostras independentes

#Hipoteses:
#H0:  miu1=miu2=miu3=miu4 (miu1=média populacional da resistencia das placas do fornecedor 1)
#H1: Existe pelo menos 1 fornecedor com resistencias média diferentes das restantes
#alfa=0.05 (pré definido quando não se diz o qual é o alfa)

sdados <-stack(Data_D)
View(sdados)
boxplot(values ~ ind, sdados)

res<-aov(values ~ ind,sdados)
summary.aov(res)

#Como p=0.03<0.05 rejeita-se H0. Há evidencias estatisticas de que pelo menos 1 fornecedor 
#Com resistencia media diferente dos restantes, para um nivel sig de 0.05.

###10)
A <- c(1,0.8,1.9,1.1,2.7, NA)
B <- c(1.7,2.5,3,2.2,3.7,1.9)
C <- c(1,1.3,3.2,1.4,1.3,2)
D <- c(3.8,2.8,1.9,3.0,2.5,NA)

all_data <- as.data.frame(cbind(A,B,C,D))
View(all_data)
boxplot(all_data)

install.packages("reshape2")
install.packages("reshape")
library(reshape2)
library(reshape)
Mdados <- melt(all_data, variable.name = "Empresa", value.name = "Custo")
colnames(Mdados) <- c("Empresa", "Custo")
View(Mdados)
#Pressepostos ANOVA - vamos assumir
#variável resistencia com dist normal
#homogeneidade de variancia
#amostras independentes

#Hipoteses:
#H0:  miu1=miu2=miu3=miu4 (miu1=média populacional do custo da empresa)
#H1: Existe pelo menos 1 empresa com custo médio diferentes das restantes
#alfa=0.05 (pré definido quando não se diz o qual é o alfa)
res<-aov(Custo ~ Empresa,Mdados)
res <-aov(Mdados$Custo ~ Mdados$Empresa)
summary.aov(res)

#Como p=0.0359<0.05 rejeita-se H0. Há evidencias estatisticas de que pelo menos 1 empresa 
#Apresenta um custo das restantes, para um nivel sig de 0.05.

#TP3
# Ficha Te?rico-Pr?tica 3
# Testes de Hip?teses n?o Param?tricos

#Ex 1)
#TN - v. a. nº de oficiais, em 25m que tem mais de 50 pontos no teste
#Assumindo que os oficiais tem o memso desempenho que a pop em geral
#TN ~ Bi (n=25; p=0.45)

#Ou seja: H0 p = 0.45 vs H1: p > 0.45
#P(Tn >= 13  | p=0.45)

sum(dbinom(13:25,25,0.45))
#ou 
pbinom(12,25,prob=0.45,lower.tail=F)
# Ou ainda:
test1 <- binom.test(13,25,p=0.45,alternative="greater")
test1

#Nao se rejeita o H0
#Como p=0.306>alfa = 0.05 nao se rejeita H0. Nao ha evidencia estatistica, para nivel de sig 0.05 , para
#Afirmar que os oficiais tem melhor desempenho sob pressao

# Ex2) 
# Tn ~ v.a do nº de crianças, em 19, que preferem o balde vermelho
#Assumindo que a probabilidade é a mesma entre escolher  vermelho ou azul, 
#entao H0: pA = 0.5 vs H1: pA > 0.5
#TN ~ Bi (n=19; pA=0.5)

test2 <- binom.test(15,19,p=0.5,alternative="greater")
test2
#Como p=0.0096 < 0.05 rejeita-se H0. Há evidencias estatisticas para alfa =0.05 para afirmar que as crianças
#preferem a cor vermelha


# Ex 3)

n <- 2000               # > 30
p_est <- 700/2000       

2000*p_est*(1-p_est)    # > 9
#Seja Tn ~Bi(n=2000; p=0.3)
#H0 : pA =0.3 vs H1: pA> 0.3
#teste aproximado (normal)
test3 <-prop.test(700,2000,p=0.3,alternative="greater")
test3


#ou (teste exato): 
binom.test(700,2000,p=0.3,alternative="greater")
#Como p=6.016e-07 <0.05 logo rej. H0 . Há evedencias estatisticas , 0.05, para afirmar que o partido p
#Tem mais de 30% das intencoes de votos

# Ex 4)  
library(readr)
Ex4 <- read_delim("C:/Users/lolas/Downloads/Ex4.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)
View(Ex4)
#H0: PA = pB vs  H1:pA != pB
#ou
#H0: PA = 0.5 vs H1:pB != 0.5

#TPC verificar os pressupostos
#MUDAR O XX E YY
test4 <-prop.test(XX,YY,p=0.5,alternative="two.sided")
test4

#Ex5) 
library(readr)
Ex5 <- read_delim("C:/Users/lolas/Downloads/Ex5.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)
View(Ex5)

# H0: p1 = p2 = p3 = p4 = p5 = p6= 1/6
# H1: Existe pelo menos i,j, i!=j, e pi!=pj, com i, j € {1,2,3,4,5,6}


contador <- numeric(6)
for(i in 1:6)
{
  contador[i] <- sum(Ex5$Resultado == i)
}
contador
prop_est <- contador/60

#Calculo 1 
Ei <- sum(contador)*rep(1/6,6)      
ET5 <- sum((contador-Ei)^2/Ei); ET5  


pval5 <- 1-pchisq(ET5,5); pval5

#Calculo 2
test5 <- chisq.test(contador)  
test5

#ou
test5a <- chisq.test(contador, p=rep(1/6,6))
test5a


