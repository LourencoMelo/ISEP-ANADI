#Ficha TP1
install.packages("readxl")
library(readxl)

getwd()

setwd("C:/Users/lolas/OneDrive/Documentos/Faculdade(WORK)/3º ANO/ANADI")

dias_sem_chuva <- read_excel("PORDATA_N.º-de-dias-sem-chuva.xlsx", range="Quadro!A8:J70")
colnames(dias_sem_chuva)
#Renomeia 
colnames(dias_sem_chuva)[1] <- "Anos"

#Cria o grafico
#c
boxplot(dias_sem_chuva[2:10], main="Dias sem chuva", ylab="Nº de dias")

#d)
dias_sem_chuva[dias_sem_chuva==0] <- NA

#e)

#f)
quantis.CB <- quantile(dias_sem_chuva$'Castelo Branco')
IQR(dias_sem_chuva$'Castelo Branco')
quantis.CB
quantis.P <- quantile(dias_sem_chuva$Porto, na.rm=TRUE)
IQR(dias_sem_chuva$Porto, na.rm=TRUE)
quantis.P

#g)
tab.P <- table(dias_sem_chuva)
tab.P #util?


#tabela com dados em classes definidas empiricamnete
#(ex: 5 classses)
tab.P <- table(cut(dias_sem_chuva$Porto, breaks=5));tab.P

#tabela com os limites das classes especificados (ex : 4classes)
#freq. absolutas
tab.P_clas <- table(cut(dias_sem_chuva$Porto, breaks = c(150,176,201,226,251))); tab.P_clas

prop.table(tab.P_clas)  #freq.relativas

#alternativa
tab.P_frel <- tab.P_clas /sum(tab.P_clas); tab.P_frel

#tabela com dados em classes regra de Sturges
nc <- nclass.Sturges(dias_sem_chuva$Porto)
1+3.3*log10(length(dias_sem:chuva$Porto)); nc

tab.P_St <- table(cut(dias_sem_chuva$Porto, breaks=nc)) #dados repartidos com regra de Sturges

tab.P_St

#1.h) histograma
hist.P <-hist(dias_sem_chuva$Porto) #Supostamente usa o métoda de Sturges, mas adequa para valores mais "Simpáticos" dos limites das classes

summary(hist.P)
hist.P$breaks
hist.P$counts #Freq absolutas
hist.P$counts/sum(hist.P$counts) #Freq relativas

#1. i) Evolução Temporal
#Dados sao series temporais -> usamos funcao ts para colocar dados em objeto de series tmeporais
dsc.P <- ts(dias_sem_chuva$Porto, start = 1960); dsc.P

ts.plot(dsc.P, main="Dias de sem chuva", lty=2, ylab="Numero de dias", xlab="Ano")
legend("bottomleft", legend = "Porto ", lty=3)
dsc.F<- ts(dias_sem_chuva$Faro, start = 1960)

ts.plot(dsc.P, dsc.F, lty=c(1:2), main="Dias sem chuva", ylab="Numero de dias", xlab="Ano")
legend("bottomleft", legend = c("Porto", "Faro"), lty = c(1:2))



install.packages("dplyr")
library(dplyr)

#extrair os dados dos anos de 2010 e 2015
dias_sem_chuva_2010_15 <- filter(dias_sem_chuva, Anos>=2010 & Anos <=2015)
