# TP4- Correla??o e Regress?o Linear

#| Ex1)           
anunc <- c(8,12,16,8,8,8,27,12,8,16)
medid <- c(4.8, 5, 17.5, 6.4, 6.7, 4.3, 20.3, 8, 3.5, 6.3)

#H0:tau = 0  vs H1: tau # 0
#Coeficiente de correlação
cor.test(anunc,medid, method ="kendall")
plot(anunc, medid)
# Conclusao:
#O coeficiente de Kendall é 0.597. Como p=0.026 < 0.05, moderada.
#Há evidência da existencia de uma correlação de ordem entre os  tempos
#anunciados e medidos.

#| Ex2)   Spearman (rho)
Notas <- read.delim("~/Faculdade(WORK)/3º ANO/ANADI/Notas.txt")
View(Notas)
colnames(Notas) <- c("Estat","Calc")

#Converter notas Calc e Estat em num?rico (Calc_n e Estat_n)
niveis_Estat <- c("Excelente","Muito Bom","Bom","Suficiente","Insuficiente","Mau")
codigos_Estat<-1:6

niveis_Calc<-c("A","B","C","D","E","F")
codigos_Calc<-1:6

#h0: rho = 0 vs H1: rho > 0
Notas$Estat_n <- codigos_Estat[match(Notas$Estat,niveis_Estat)]
Notas$Calc_n  <- codigos_Calc[match(Notas$Calc,niveis_Calc)]

cor.test(Notas$Estat_n, Notas$Calc_n, method = "spearman")

# Conclusao:
#O coeficiente de Spearman é 0.455, moderada. Como p=0.02 < 0.05,.
#Há evidência da existencia de uma correlação de ordem entre as notas de estatistica
#e as de calculo

#| Ex 3)  TPC


#| Ex 4)
library(readr)
fang_data <- read_csv("Faculdade(WORK)/3º ANO/ANADI/fang_data.csv")
View(fang_data)

library(Hmisc)
rcorr(as.matrix(fang_data[2:5]),type="pearson")    

# Os dados s?o continuos, mas ser?o normais?
library (nortest)
#H0: A dist das cotações no Facebook é normal
#H1: A idst das cotações no Facebook não é normal
lillie.test(fang_data$Facebook) #Pois n > 1008 > 30
lillie.test(fang_data$Amazon)
lillie.test(fang_data$Netflix)
lillie.test(fang_data$Google)
#as Cotações não tem distribuição normal => Pearson não é adequado
# => Usamos o Spearman

hist(fang_data$Facebook)
hist(fang_data$Amazon)
hist(fang_data$Netflix)
hist(fang_data$Google)

#
rcorr(as.matrix(fang_data[2:5]), type="spearman")

#Diferen?as entre os coefs?  Os valores são diferentes mas a interpretação 
#É a mesma
#Conclusão: A correlação de ordem mais forte é entre o Facebook e Amazon.
#de 0.92 , #(p<0.001)
#A correlação entre Amazon e Google é de -0.01, ou seja, muito fraca
#(e não siginificativa: p=0.71 > 0.05). As restantes correlações são
#fracas, negativas e significativas pvalue < 0.001 sendo a mais forte de -0.46 entre
#Facebook e Netflix e Amazon e a mais fraca de -0.27 entre Google e Facebook.

# Ex 5) 
xi <- c(21,24,32,47,50,59,68,74,62,50,41,30)
yi <- c(185.79, 214.47, 288.03, 424.84, 454.58, 539.03, 621.55, 675.06, 562.03, 452.93, 369.95, 273.98)
# 5.a)
plot(xi, yi)
#O gráfico evidencia uma relação linear entre as variaveis
# 5.b) 
reg1 <- lm (yi ~ xi)

summary (reg1)

b0 <- reg1$coefficients[1];b0
b1 <- reg1$coefficients[2];b1

#ou:
coef(reg1)
# Equacao do Modelo: 
#O modelo estimado é
#yi = -6.34 + 9.21xi
y40 <- b0 + b1*40
x0 <- data.frame(xi =40)
#Estimar y(40) - Podemos usar o nosso modelo? (RVM)

x0 <- data.frame (xi=40)
predict(reg1,x0)
#ou:



# 5.c)
cor.test(xi,yi, method ="pearson")
#O quoficiente de pearson é aproximadamente 1, dinica correlação positiva forte 

# 5.d) Pressupostos RL:  
# - Homocedasticidade; 
# - Normalidade dos Residuos e m?dia zero, 
# - Independencia dos res?duos

#####1? Pressuposto: Homocedasticidade = igualdade de variancias:
yi
#Diferenças entre os valores com os valores estimados
fitted(reg1)   
xi_df<- data.frame(xi)
predict(reg1,xi_df)

residuals(reg1)         # = yi-fitted(reg1)


par(mfrow=c(2,1)) #Para criar 2 graficos numa janela
plot (fitted(reg1), residuals(reg1), xlab="Val. Ajustados", ylab="Residuos")
abline(h=0)

plot (xi, residuals(reg1), xlab="xi", ylab="Residuos")
abline(h=0)

#Não se verifica nenhum padrao de dispersão dos dados ou seja os graficos dao a 
#ideia de de Homocedasticidade
#Vamos usar testes para concluir

mx <- median(xi);   mx
#H0: hÁ Homocedasticidade #H1; nÃO HÁ Homocedasticidade
var.test(residuals(reg1)[xi>mx], residuals(reg1)[xi<mx])
#Como p = 0.687 > 0.05 assumimos a homogeneidade de variancias(Homocedasticidade)

#NOTA: Para amostras grandes (n>30), assumindo res?duos independentes 
# e com distribui??o normal,pode-se efectuar um teste de Breusch-Pagan
# (fun??o bptest do package lmtest). 
install.packages("lmtest")
library(lmtest)
bptest(formula = yi ~ xi)

#####2? Pressuposto: Normalidade dos Residuos de media zero
dev.off()
qqnorm(residuals(reg1), xlab="Residuos", ylab="Quantis teoricos")
qqline(residuals(reg1))
hist(residuals(reg1))
#Prossupostos e tudo sobre residuos
#H0: a Dist DOS RESiduos é normal vs H1: a dist dos residuos não é normal
shapiro.test(residuals(reg1))
#Conclusao?
#O p = 0.41 > 0.05 logo podemos assumir que a media dos residuos é zero

#H0: miu = 0 vs h1 miu!= 0
t.test(residuals(reg1),mu=0,alternative="two.sided")
#Conclusao: Como p=1 & p > 0.05 logo podemos assumir que a media dos residuios é zero

###### 3? Pressuposto: independencia dos residuos
#H0: Os residuos sao indepentes vs h1 : Os residuos não sao indepentes
library (car)
durbinWatsonTest(reg1)
#Conclusão como p=0.148 > 0.05 logo podemos assumir independencia dos residos.

#Todos os pressupostos da RL sao verificados e portanto é adequado afazer inferencia com 
#o modelo obtido

