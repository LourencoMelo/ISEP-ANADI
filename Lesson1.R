#Analse Exploratoria de dados
#Medidas descritivas

amostra1 <- c(4,6,7,8,9,10)

#Medidas de localização
#Média
mean(amostra1)
sum(amostra1)/length(amostra1)


amostra2 <- c(-2,NA, 1 ,6, 8,NA, -4)
amostra2 
mean(amostra2, na.rm=TRUE)

#Mediana
dados <- c(3,5,7,15,16,1,17,24,0,-1,5,2,9)
median(dados)

#Quartis
quantile(dados,0.95)

#Medidas de variabilidade/dispersao
dados=c(0.9,0.8,0.3,1.1,1.2,1.3,0.7,0.5,3.4,2.6)
#Amplitude
max(dados)-min(dados)
#ou
diff(range(dados))

#amplitude inter-quartil
IQR(dados)
#ou
quantile(dados, 0.75)-quantile(dados, 0.25)

#variancia e desvio padrao
dados=c(0.9,0.8,0.3,1.1,1.2,1.3,0.7,0.5,-0.1,0,-0.7)
var(dados)
sqrt(var(dados))
sd(dados)

#Medidas de forma
#Instala os packages
install.packages("e1071")
library(e1071)
set.seed(33) #define um mesmo vetor random
dados <- rnorm(10000,mean=15, sd=5)
mean(dados)
hist(dados) #cria o histograma
skewness(dados) #Assimetria

#
set.seed(55)
dados <- rchisq(5500,0.9,df=5)
skewness(dados)
hist(dados)

#
kurtosis(dados)

#Variaveis qualitativas
#Contruir tabela
#Tabelas de Frequencias
amostra <- c(rep('L',4),rep('C',23),rep('D',16),rep('O',7))
data <- factor(amostra ,c('L','C','D','O'),
               labels=c("Leitura", "Cinema","Desporto", "Outros"))
#Freq absolutas
tabela <- table(data); tabela
#Fre relativa
tabela.rel <- prop.table(tabela)

#graficos
barplot(tabela)
pie(tabela)

#Dados quantitativos
#Graficos de bigodes
dados1=rchisq(100,.2, df=7)
dados2=rchisq(100,5, df=7)
classes=c("dados1","dados2")
aa=boxplot(dados1,dados2,names=classes,col=c(4,2))

boxplot(dados1,dados2)


