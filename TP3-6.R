##TP3 (cont.)

setwd("C:/Users/Luisa Castro/Dropbox/ISEP/2022_2023/ANADI/Aula_TP3 e TP4")
library(readr)   #para importar dados csv


#|Ex6) (falta o 25.37 no enunciado da TP3)
Ex6 <- c(25.36, 24.64, 25.37 ,25.17, 24.56, 24.56, 24.80, 25.21, 25.38, 24.55)


#Vamos verificar a normalidade dos tempos
#Como n<30 o teste shapiro é adequado
#H0: os tempos de execução provêm de uma distribuição normal
#h1: os tempos de execução não provêm de uma dist. normal
Shap_t <- shapiro.test(Ex6)
Shap_t$p.value
#Como p=0.02<0.05 rej H0, ou seja a distribuição não é normal
# => não podemos aplicar o teste t

#Outra possibilidade que os dados nao provem de uma dist. normal
qqnorm (Ex6)
qqline(Ex6)

#Lilliefors n?o aplic?vel aqui pois n ? <30 (mas fica o c?digo)
library(nortest)
Lill_t <- lillie.test(Ex6)
Lill_t$p.value

#...

hist(Ex6)
library(moments)
skewness(Ex6)   #|skewness| < 0.1  => Distribui??o sim?trica
#A variavel é simetrica = teste de Wilcoxon

#H0 : mediana dos tempos = 25
#H1: mediana dos tempos > 25
wilcox.test(Ex6, alternative = "greater", mu=25, exact=FALSE) #exact=FALSE porque h? empates
#Como p=0.75 > 0.05, não se rejeita H0
#Não há evidencias estatisticas ao nivel de 0.05 para afirmar que a mediana dos tempos é
#siginificativamente superior a 25.



#No caso da amostra n?o ser sim?trica teriamos de recorrer ao teste do sinal.
library(BSDA)
signt <- SIGN.test(Ex6,md=25,alternative = "greater") #only for one sample problems
signt$p.value


#|Ex 7)  
alfa=0.05
t_antes <-  c(14  , 9  , 12.5, 13  , 9.5, 12.1)
t_depois <- c(13.8, 8.9, 12.6, 12.8, 9.2, 14.2)
t_dif <- t_antes - t_depois 
t_dif


#a) Sinal
# Hipoteses:  
#H0: mediana_t_dif = 0
#H1: mediana_t_dif > 0
library(BSDA)
signt <- SIGN.test(t_dif, mu=0, alternative= "greater")
signt$p.value
signt

#Como p=0.34 > 0.05 não rej H0. 
#Não há evidencias estatisticas, ao nivel de 0.05, para afirmar
#Que houve melhoria significativas dos tempos de arranque.

#b) Wilcoxon

hist(t_dif) 
library(moments)
skewness(t_dif)   

# |SK| < 0.1 é simetrica
# 0.1 <|SK| < 1 é moderadamente assimetrica
# |SK| > 1 é fortemente assimetrica

#c) Parametrico- teste t para amostras emparelhadas
# Valor de p < 0.05 para rejeitar a normalidade
#Como p<0.001 < 0.05 rej H0 a dist não é normal

#Logo nao é adequado aplicar o teste parametrico
shapiro.test(t_dif)


#|Ex 8)
#a) Importar dados

library(readr)
cevada <- read_csv("Faculdade(WORK)/3º ANO/ANADI/cevada.csv")
View(cevada)

# 2 amostras emparelhadas

#n>= 30 (TLC) assumir normalidade de x_barra, ou seja ,da distribuição da média 
#teste t de Student

#H0: mui_dif = 0  vs H1: mui_dif != 0 (bilateral)
t.test(cevada$Y1, cevada$Y2, paired=TRUE, alternative="two.sided")
#Como p=00.2 < 0.05 rej H0 . Há evidencia estatistica ao nivel de significancia 0.05, que nos 
#permitem afirmar que as colheitas foram diferentes.


#b) 
colh_dif <- cevada$Y1 - cevada$Y2
hist(colh_dif)
skewness(colh_dif)

#0.1  \sk\ < 1 => ASSIMETRIA MODERADA
#=> podemos aplicar o teste Wilcoxon

#H0: mediana_dif = 0 vs H1: mediana_dif != 0 (bilateral)


#0.1<|skewness|<1  => assimetria moderada
wilcox.test(cevada$Y1, cevada$Y2,paired=TRUE, alternative="two.sided",
            exact=FALSE)

#Como p=0.005 < 0.05 rej H0. Há evidencias estastisticas ao nivel de significancia 0.05 que nos permitem afirmar
#Que as colheitas foram diferentes.

#| Ex 9) Usabilidade medida em escala de 1 a 10 -> Teste n?o param?trico 2 amostras

webA <- c(3,4,2,6,2,5)
webB <- c(9,7,5,10,6,8)

#H0: Mediana_webA  = Mediana_webB
#H0: Mediana_webA < Mediana_webB 
wilcox.test(webA, webB, paired=F, alternative= "less", exact=FALSE)

#Como p =0.006 < 0.05 rej H0. 
#Há evidencia estatistica para alfa=0.05 que nos permitem afirmar
#Que a usabilidade da webB é superior siginificativamente superior à da webA

#| Ex 10) TPC

#Importa-se com o prmieiro tipo de IMPORT

#| Ex11
# Importar dados
library(readr)
dados_vacinas <- read_csv("Faculdade(WORK)/3º ANO/ANADI/dados_vacinas.csv")
View(dados_vacinas)

# Amostras independentes, 3 grupos
# amostras pequenas -> Kruskal-Wallis

#H0: med_ant_VA = med_ant_vB = med_ant_vC
#H1: Há pelo menos uma vacina cuja mediana dos anticorpos produzidos é
#diferente das outras duas

#ou
#H0: A distribuição dos dados da prod. de anticorpos é igual entre as 3 vacinas
#H1: A distribuição dos dados da prod. de anticorpos é diferente para pelo menos uma das 3 vacinas
kruskal.test(anticorpos ~ vacina,data=dados_vacinas)

#p = 0.026 < 0.05 rej H0
#Há pelo menos uma vacina tem uma distribuição diferente de anticorpos das outras
#Rejeita-se H0
boxplot(anticorpos ~ vacina,data=dados_vacinas)
#Tentar encontrar um teste que comprove qual delas era a mais diferente
#O teste mais apropriado para identificar o q mais se distingue comparativamente com os outros
#Correção de bonferroni (teste para comparações multiplas)

#| Ex 12)  TPC


# Ex13) 
# Importar dados
library(readr)
desempenho <- read_delim("Faculdade(WORK)/3º ANO/ANADI/desempenho.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)
View(desempenho)

#H0: Há concordancia entre os utilizadores sobre o ranking do desempenho das 3 marcas
#H1: Não há concordância entre os utilizadores sobre o ranking do desempenho das 3 marcas

friedman.test (desempenho$Resposta, desempenho$Marca, 
               desempenho$Utilizador)
#ou
friedman.test (Resposta~Marca|Utilizador,data=desempenho)

#Rejeita-se H0 não há concordancia entre os utilizadores quanto ao ranking do desempenho
#de pelo menos uma das marcas. para um nivel de significancia de 0.05


# Ex14) 
# Importar dados
library(readr)
aval_restaurantes <- read_csv("aval_restaurantes.csv")


attach(aval_restaurantes)
aval_rest <- data.frame(gastronomo=rep(c(1,2,3,4,5,6),4),
                        restaurante=c(rep("A",6),rep("B",6),rep("C",6),rep("D",6)),
                        avaliacao=c(A,B,C,D))
detach(aval_restaurantes)

# ... (TPC)
