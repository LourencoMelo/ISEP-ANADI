# TP4 - Correla??o e Regress?o Linear

#Ex 6. a)
Rend <- c(14,19,23,12,9,15,22,25,15,10,12,16)
Cap  <- c(31,40,49,20,21,34,54,52,28,21,24,34)

plot(Rend,Cap,xlab="Rendimento Anual (m.u.m.)",ylab="Capital Seguro (m.u.m.)")
 
reg2 <- lm (Cap ~ Rend)    
summary (reg2)
abline (reg2)

# 6.b)
x0 <- data.frame(Rend=20)  # RVM
predict(reg2,x0)


# 6.c) Homocedasticidade e Independencia dos Residuos:

par(mfrow=c(2,1))
plot (fitted(reg2), residuals(reg2), xlab="Val. Ajustados", ylab="Residuos")
abline(h=0)
plot (Rend, residuals(reg2), xlab="Rend", ylab="Residuos")
abline(h=0)


# Breusch-Pagan test, bptest)
medRend <- median(Rend);   medRend
var.test(residuals(reg2)[Rend>medRend], residuals(reg2)[Rend<medRend])

# Teste Goldfeld-Quandt
#Alternativa para testar homogeneidade - teste Goldfel-Quant
library(lmtest)
gqtest(reg2)

#Independencia dos residuos
#H0: os residuos são independetes vs H1: os residuos não são independentes
 
library (car)
durbinWatsonTest(reg2) # tençao que este teste nao é deterministico 
#Como p>0.8> 0.05= alfa podemos assumir independencia dos residuos

#Temos entao evidencia de que os residuos cumprem os 2 pressupoostos de independencia
#e homecedasticidade

#6. d) Birnakudade dis Resudyos e da media zero 
dev.off()
qqnorm(residuals(reg2), xlab="Residuos", ylab="Quantis teoricos")
qqline(residuals(reg2))

hist(residuals(reg2))
shapiro.test(residuals(reg2))
#Os resultados do teste confiram que podemos assumir a distribuição
#dos residuos (p=0.983)


#H0: media dos residuos = 0 vs H1: media dos residuos != 0
t.test(residuals(reg2),mu=0,alternative="two.sided")
#Os resultados confirmam que tambem podemos assumir
#que os resutlados tem media 0 (p=1)


#Ex 7. 
library(readr)
Mont <- read_delim("C:/Users/Luisa Castro/Dropbox/ISEP/2022_2023/ANADI/Aula_TP6 e TP7/ExemploMontegomery.csv",
                   ";", escape_double = FALSE, locale = locale(decimal_mark = ","),
                   trim_ws = TRUE)

View(Mont)
# Observar os dados que temos!

# 7.a)
# Y = beta0 + beta1x1 + beta2x2 + epsilon
#Onde
#Y - é o acabamento(Surface Finish)
#x1 - velocidade (RPM)
#x2 - tipo de corte (0 se a ferrament otipo 302: 1 se ferramente tipo 416)

#Interpretacao:
#Se ferramente corte do tipo 302 (x2 = 0) tem-se o modelo:
#Y= beta0 + beta1x2 + epsilon
#Que será uma reta com devlive beta1 e ordenada na origem beta0
#b0 : o valor do cabaamento para um torno com RPM=0, que neste contexto não tem sentido
#b1: será a modificação no valor do acabamento quando as RPM aumentam de 1 unidade
# Se ferramente corte for do tipo 416 (x2=1) tem-se o modelo:
# Y= (beta0+beta2) + beta1x1 + epsilon
#b0+b2 : o valor do cabamento para um torno com RPM=0, que neste caso nao tem sentido
#b1 : sera a modificação em media no valor do abamento quando incrementam de 1 unidade
#Que será uma reta com declive beta1 e ordenada na origem(beta0+beta2)

#7. b)
#recodificar tipo de ferramente de corte:
colnames(Mont) <- c("n.obs","surface.finish","rpm","type.cutting")

Mont$type.cutting <- factor(Mont$type.cutting)

reg7 <- lm(Mont$surface.finish ~ Mont$rpm + Mont$type.cutting)

reg7

summary(reg7)
#Ou seja:
#b0=14.2762 ; b1 = 0.1411; b2 =-13.2802
#notar que apesar de os niveis de type.cutting serem 302 e 416, como esta factor o
#R o 302 como send "0" e a 416 como sendo "1"

#y^ = 14.2762 + 0.1411 - 132802^* x2      -> Equação estimada pelo modelo
 
# 7.c) 

# 7.d)

plot (fitted(reg7), residuals(reg7), xlab="Val. Ajustados", ylab="Residuos")
abline(h=0)

qqnorm(residuals(reg7), xlab="Residuos", ylab="Quantis teoricos")
qqline(residuals(reg7))
shapiro.test(residuals(reg7))


t.test(residuals(reg7),mu=0,alternative="two.sided")

library(car)
durbinWatsonTest(reg7)

# 7.e) Multicolinearidade 
library(car)
vif(reg7)
