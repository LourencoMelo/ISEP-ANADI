#Exercícios 4.1. - 2, 5 e 8(Artur Muiria) - 1161274

# Instalação do pacote readr (se ainda não tiver instalado)
install.packages("readr")
library(readr)
# Leitura do arquivo CSV com os dados
dados <- read_csv("C:/Users/artur.muiria/Desktop/ANADI/tp_anadi_3dg_1191419_1161274_1190811/TP2/ciclismo.csv")
View(dados)

#Exercicio 2

# Instalação do pacote lubridate
install.packages("lubridate")                             
library("lubridate")  

# A linha abaixo calcula a idade de cada ciclista ao fazer a diferença entre a data atual e a data de nascimento
# e insere as idades numa nova coluna dos dados 
dados$Age <- round(time_length(difftime(Sys.Date(), dados$dob), "years")) 



