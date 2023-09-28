#Exercício 1 (José Maia) - 1191419
# Biblioteca readr para ler arquivos CSV
library(readr)
# Biblioteca readxl para ler arquivos Excel
library(readxl)
# Biblioteca dplyr para manipulação de dados_exe1
library(dplyr)
# Lê o arquivo "DADOS1.csv" e não guarda as primeiras duas linhas, isto porque as duas primeiras linhas não fazem parte
#da amostra e apenas são valores informativos
dados_exe1 <- read_csv("C:/Users/josed/Downloads/DADOS1.csv",skip = 2)
View(dados_exe1)

# Renomear as colunas utilizadas nos exercícios
colnames(dados_exe1)[1] <- "tempo" # segundos
colnames(dados_exe1)[5] <- "MT_01" # Temperatura do motor da bomba 1
colnames(dados_exe1)[11] <- "MT_02" # Temperatura do motor da bomba 2
colnames(dados_exe1)[17] <- "MT_03" # Temperatura do motor da bomba 3
colnames(dados_exe1)[28] <- "OL_01" # Medições do “oil rate” da bomba 1
colnames(dados_exe1)[37] <- "OL_02" # Medições do “oil rate” da bomba 1

#1.a) 
# Criar uma coluna nova "Tempo_POSIX" na tabela dados_exe1 onde vamos converter os valores da coluna tempo em objetos
#de tempo POSIXct. A função "as.POSIXct converte a coluna "tempo" em objectos POSIXct, utilizando a origem
#em "1970-01-01" e o fuso horário como "GMT"
dados_exe1$Tempo_POSIX <- as.POSIXct(dados_exe1$tempo, origin = "1970-01-01", tz = "GMT")
View(dados_exe1)

#1.b)
#install.packages("ggplt2")
#Biblioteca para visualização de dados_exe1 em gráficos personalizáveis
library(ggplot2)

# Filtrar dados_exe1 para incluir apenas o dia 4 de agosto de 2013
#Aqui na variavel temp_data temos um subconjunto com as colunas MT_01,MT_02, MT_03 e Time_POSIX respetivas ao dia 4 de Agosto de 2013.
#A função as.Date() é utilizada para converter a classe de data/hora (POSIXct) para a classe de data (Date), o que permite 
#realizar comparações diretas de datas sem levar em consideração a hora.
temp_data <- subset(dados_exe1,as.Date(Tempo_POSIX) == as.Date("2013-08-04"), select = c("MT_01", "MT_02", "MT_03","Tempo_POSIX"))
#View(temp_data)

# Criar gráfico
# A função aes() mapeia as variáveis para os diferentes elementos do gráfico. 
# Aqui, a variável Tempo_POSIX é mapeada para o eixo x e a temperatura do motor das bombas no eixo dos y.
# Utilizamos a função geom_line() para desenhar as linhas das variaveis do eixo do y. Utilizando a variavel color
# e a função scale_color_manual() definimos as cores correspondentes a cada bomba.
# A função labs() é utilizada para adicionar titulos.
# Finalmente, a função scale_x_datetempo() define os intervalos de tempo e a theme_bw() para o fundo branco do gráfico
ggplot(temp_data, aes(x = Tempo_POSIX)) +
  geom_line(aes(y = MT_01, color = "Bomba 1")) +
  geom_line(aes(y = MT_02, color = "Bomba 2")) +
  geom_line(aes(y = MT_03, color = "Bomba 3")) +
  labs(title = "Temperatura do motor nas bombas 1, 2 e 3 no dia 4 de agosto de 2013",x = "Hora", y = "Temperatura (K)", color = "Bomba") +
  scale_x_datetime(date_breaks = "4 hours", date_labels = "%H:%M") +
  scale_color_manual(values = c("red", "blue", "green"), labels = c("Bomba 1", "Bomba 2", "Bomba 3")) +
  theme_bw()

#1.c)
# A função boxplot() cria um boxplot para visualizar a distribuição das temperaturas 
# dos motores das três bombas no dia 04/08/2013. Excluimos apenas a coluna do Tempo_POSIX e com o main,xlab e ylab
# definimos o titulo do grafico, do eixo do x e do y.
boxplot(temp_data[1:3], main = "Temperaturas dos motores dia 04/08/2013", xlab = "Bomba", ylab = "Temperatura (K)")

#1.d)
#i) 
#Filtrar os dados_exe1 somente de 03 de 2014. O argumento select é para selecionar as colunas pretendidas.
#A função format() extrai o ano e o mês da coluna ""Tempo_posix" para depois filtrar o mês e ano pretendido.
barris_data <- subset(dados_exe1, format(Tempo_POSIX, "%Y-%m") == "2014-03",select = c("OL_01", "OL_02","Tempo_POSIX"))
#View(barris_data)
# Calcular a média diária das colunas A e B
# A função aggregate junta os dados da bomba1 e 2 da tabela "barris_data" e calcula a média de "oil rate" de cada bomba para cada dia
media_por_dia <- aggregate(cbind(OL_01, OL_02) ~ as.Date(Tempo_POSIX), barris_data, mean)
#View(media_por_dia)
# Mudar o nome da coluna as.Date(Tempo_POSIX) para dia
colnames(media_por_dia)[1] = "dia"

# Usamos o ggplot para criar um gráfico de barras para comparar a produção de barris diária das bombas1 e 2
# A função aes() mapeia as variáveis para os diferentes elementos do gráfico. 
# Adicionamos as barras com o geom_bar e definimos a cor das mesmas.
# A função labs() é utilizada para adicionar titulos.
# theme_bw() para o fundo branco do gráfico
ggplot(media_por_dia, aes(x = dia, y = OL_01)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  geom_bar(aes(y = OL_02), stat = "identity", fill = "black") +
  labs(title = "Barris produzidos diariamente Bomba A(azul) e Bomba B (preto)", x = "Dia", y = "Total") +
  theme_bw()

#ii)
# Filtrar os dados_exe1 para serem somente entre os dias 1-6-2013 e 31-5-2014 e da bomba 1(OL_01)
barril_data <- subset(dados_exe1, as.Date(Tempo_POSIX) >= as.Date("2013-06-01") & as.Date(Tempo_POSIX) <= as.Date("2014-05-31"), select = c("OL_01","Tempo_POSIX"))
# Calcular a média diária de cada dia
# A função aggregate junta os dados da bomba1 da tabela "barril_data" e calcula a média de "oil rate" da bomba para cada dia
medias_por_dia <- aggregate(cbind(OL_01) ~ as.Date(Tempo_POSIX), barril_data, mean)
#View(medias_por_dia)
# Extrair apena o mês da coluna do tempo com a função format()
medias_por_dia$mes <- format(medias_por_dia$`as.Date(Tempo_POSIX)`, "%m")
View(medias_por_dia)
# Calcular o total de barris produzidos por mês da bomba 1
# A função aggregate junta os dados da bomba1 da tabela "barril_data" e calcula a soma das médias da bomba 1 para cada mês
total_por_mes <- aggregate(OL_01 ~ mes, medias_por_dia, sum)
#View(total_por_mes)
# Analisa a tabela e retorna o mes com a maior produção
# "which.max()" encontra o índice da primeira ocorrência do valor máximo na 
# coluna "OL_01" (que é de produção de barris daquele mês)
# A notação [1,1] serve para retornar o primeiro elemento da primeira coluna da linha encontrada que contém o nome do mês
mes_maior_producao <- total_por_mes[which.max(total_por_mes$OL_01), 1]
View(mes_maior_producao)
# R: A bomba 1 extraiu mais barris de petróleo no mês de agosto de 2013

#iii)
# definir semente aleatória
set.seed(300)

# gerar amostra aleatória de dias
amostra_dias <- sample(1:365, 10)

# Filtrar dados_exe1 pela amostra aleatória
dados_dias_amostra <- subset(dados_exe1, as.Date(dados_exe1$Tempo_POSIX) %in% 
                          seq(as.Date("2013-06-01"), as.Date("2014-05-31"), by="day")[amostra_dias], select = c("OL_01", "OL_02", "Tempo_POSIX"))
View(dados_dias_amostra)

# Mostra os dias obtidos apartir da amostra aleatória -> 2014-02-23 esta data vem na amostra aleatória mas não existe nos dados
# dias_amostra <- seq(as.Date("2013-06-01"), as.Date("2014-05-31"), by="day")[amostra_dias]
# print(dias_amostra)

# Calcular a media diaria das bombas na amostra aleatória
# A função aggregate junta os dados da bomba1 e 2 da tabela "dados_dias_amostra" e calcula a média de "oil rate" de cada bomba para cada dia
media_por_dia <- aggregate(cbind(OL_01, OL_02) ~ as.Date(Tempo_POSIX), dados_dias_amostra, mean)
View(media_por_dia)
# Criar um boxplot com os dados da amostra aleatória obtidos
# A função boxplot() cria um boxplot para visualizar a produção diária das bombas nos dias da amostra aleatória 
# Com o main,xlab e ylab
# definimos o titulo do gráfico, o eixo do x e do y.
boxplot(media_por_dia[2:3], main = "Produção diária nos dias da amostra aleatória", xlab = "Bomba", ylab = "Barris produzidos")

#iv)
# Como as médias das bombas são colhidas nos mesmo dias o teste efectuado é um t-test com amostras emparelhadas
# H0 -> A média da produção diária de petróleo da bomba 1 é igual à da bomba 2
# vs
# H1 -> A média da produção diária de petróleo da bomba 1 é maior do que da bomba 2.
# Foi escolhido adotar um nível de significância de 5% , alfa = 0.05

test.emp <- t.test(media_por_dia$OL_01,media_por_dia$OL_02,paired = TRUE, alternative = "greater")
test.emp

#p = 2.175e-05
# O resultado indica que existe uma diferença significativa entre a bomba 1 e 2. O valor de t = 8.0043 e o valor de p é extremamente
# baixo. Como  p < 0.05 rejeitamos H0. Há evidências estatisticas nesta amostra para conlcuir que a média de produção
# diária de petróleo da bomba 1 é maior do que da bomba 2, para um nível de significancia de 0.05.


# v) Analisando o boxplot da alínea iii) podemos concluir que o resultado obtido na alínea anterior corresponde à realidade.
# Podemos verificar que os valores de produção diária da bomba 1 são todos maiores que os da bomba 2.


###################################################################################################################################
#Exercício 2 (Artur Muiria) - 1161274

# Instalação do pacote readr (se ainda não tiver instalado)
install.packages("readr")
library(readr)
# Leitura do arquivo CSV com os dados
dados <- read_csv("C:/Users/PC/Desktop/ANADI/tp_anadi_3dg_1191419_1161274_1190811/DADOS2.csv")
View(dados)



#a)
library(Hmisc)
rcorr(as.matrix(dados[3:8]), type="pearson")
#os dados são continuos com amostras independentes


#Com base na matriz de correlações, conseguimos observar que existem
#correlações positivas fortes entre os pares de algoritmos GB e SVM (0.86), 
#RF e DT (0.88), ML e KN (0.85).

#Uma correlação positiva forte é indicada quando o coeficiente de correlação de 
#Pearson é próximo de 1 que se aplica neste caso.Pode-se observar que existe 
#pelo menos uma forte relação linear entre duas variáveis em que ambas aumentam 
#ou diminuem juntas. 
#Isto significa que, quando uma das variáveis aumenta, a outra também tende a 
#aumentar, e quando uma delas diminui, a outra também tende a diminuir. 



#b)

#Aqui será usado o Teste de ANOVA para efetuar o teste de hipótese e 
#verificar se existem diferenças significativas entre a precisão dos diferentes algoritmos.
#Optou-se pela ANOVA porque esta é usada para comparar médias de duas ou mais
#amostras independentes e testar se existem diferenças estatisticamente significativas.

#É necessário fazer a formatação dos dados e dividi-los apenas em duas colunas
#(algoritmo, precisao) para poder aplicar o teste Friedman

library(reshape2)
df <- melt(dados[3:8], variable.name = "algoritmo", value.name = "precisao")
View(df)
#precisão: variável dependente
#algoritmo: variável independente


#Pressupostos do teste ANOVA:

#Teste Shapiro : n<30
# O nível de significância adotado será de 5% (α = 0,05).
#H0 : p-value > 0.05    H1:p-value <= 0.05

#Teste de normalidade: Podemos usar a função shapiro.test() para testar a
#normalidade.

shapiro.test(df$precisao)

#Como p-value é 0.0007857 < 0.05 portanto rejeita-se H0 e podemos assumir que os
#dados da amostra não segue uma distribuição normal.

#Como a normalidade foi rejeitada é necessário recorrer-se a métodos alternativos como
#o teste de Kruskal-Wallis. Este teste é a alternativa não paramétrica do teste
#One-Way ANOVA

#Teste Kruskal-Wallis
# O nível de significância adotado será de 5% (α = 0,05).
#H0: p-value > 0.05 vs H1: p-value <= 0.05
#H0: Não há evidências de diferenças significativas entre a precisão dos diferentes algoritmos
#H1: Há evidências estatisticas que existem diferenças significativas entre a 
#precisão dos diferentes algoritmos


kruskal.test(precisao ~ algoritmo, df)

#Como p-value é 0.3335 > 0.05 portanto rejeita-se H1 e pode-se concluir que não há
#evidências de diferenças significativas entre a precisão dos diferentes algoritmo



#c)

#Na alínea anterior, realizou-se um teste de hipótese utilizando a função 
#kruskal.test() para comparar a precisão dos diferentes algoritmos de 
#Machine Learning. A decisão de utilizar esse teste foi baseada no fato de que 
#o teste de normalidade de Shapiro indicar que a distribuição dos dados
#em cada grupo não ser normal.

#O teste de Kruskal-Wallis indicou que não há diferenças significativas (p-value > 0.05).
#Como não houve diferenças significativas não foi necessário fazer o estudo post-hoc do teste.

###################################################################################################################################
#Exercício 3 (Lourenço Melo) - 1190811
#Importar os dados do ficheiro "DADOS3.csv"
#Primeiramente caso não esteja instalado o package é necessario instalá-lo
#Depois importamos o package
#Efetuamos a leitura e visualizamos os dados
install.packages("readr")
library(readr)
dados <- read_csv("Faculdade(WORK)/3º ANO/ANADI/Trabalho 1/DADOS3.csv")
View(dados)

#Neste exercício tomamos o alfa como 0.05 como a convenção demanda, pois não foi
#referido um valor diferente para o alfa no enunciado do exercício.


#Mostra o tipo de dados que temos na variável "DADOS3"
#E número de observações que tem cada coluna
#Também mostra exemplo dos primeiros valores das colunas
#É bom para ter uma overview dos dados que estamos a lidar
str(dados)

#Criar subconjuntos de dados para cada nº de cilindros diferente
cil4 <- subset(dados, dados$Cylinders == 4)
cil6 <- subset(dados, dados$Cylinders == 6)
cil8 <- subset(dados, dados$Cylinders == 8)

#Verificar as medidas de tendência central
summary(cil4$Acceleration)
summary(cil6$Acceleration)
summary(cil8$Acceleration)

#Para este tipo de dados resolvemos remover os outliers para ter uma comparação mais justa
#Graficamente podemos ver uma diferença consideravel de dados entre os veiculos
#de 8 cilindros e os restantes veiculos 
boxplot(cil4$Acceleration, cil6$Acceleration, cil8$Acceleration, names = c("4 cilindros", "6 cilindros", "8 cilindros"), outline = FALSE)

#Antes de realizarmos o teste de ANOVA necessitamos devemos fazer teste à normalidade
#Para dados com mais de 30 observações iremos realizar o teste de Lilliefors
library(nortest)
#Já para dados com menos de 30 observações iremos realizar o teste de Shapiro
#4 cilindros: 
#n>30  => Lilliefors
#H0 : p-value > 0.05 H1: p-value <= 0.05
lillie.test(cil4$Acceleration)
#Como p-value = 0.2894 não ha evidencias estatisticas suficientes para rejeitar
#a hipotese nula de que a distribuição dos dados é normal

#6 cilindros: 
#Teste Shapiro : n<30
#H0 : p-value > 0.05    H1:p-value <= 0.05
#Como o valor p-value = 0.03628 indica que há evidencias suficientes para
#se rejeitar H0 e aceita-se H1
#Assim podemos assumir que os dados da amostra dos veiculos de 6 cilindros não
#seguem uma distribuição normal
shapiro.test(cil6$Acceleration)

#8 cilindros:
#n>30  => Lilliefors
#H0 : p-value > 0.05 H1: p-value <= 0.05
lillie.test(cil8$Acceleration)
#Como p-value = 0.7804 não ha evidencias estatisticas suficientes para rejeitar
#a hipotese nula de que a distribuição dos dados é normal


#Inicialmente ao começar o exercício pensou-se em realizar o Teste ANOVA
#Como os dados dos veiculos de 6 cilindros não seguem uma distribuição normal
#Achamos pertinente substituir pelo teste de Kruskal-Wallis
#Teste Kruskal-Wallis
#H0: p-value > 0.05 H1: p-value <= 0.05
#H0: Não há diferença estatisticamente significativa entre as medianas dos grupos
#H1: Há evidencias estatisticas que existem diferenças significativas nas medianas entres os grupos
lista_dados <- list(cil4$Acceleration, cil6$Acceleration, cil8$Acceleration)
resultado_Kruskal <- kruskal.test(lista_dados)
resultado_Kruskal
#Existe evidencias estatisticas siginificativas para rejeitar a hipotese nula
#Assim pode-se afirmar que há pelo menos uma diferença significativa na aceleração
#Entre pelo menos dois grupos de carros
#Com esta informação convém saber qual dos grupos é que tem essa diferença significativa
#Para isso vamos utilizar metodo de correção de Bonferroni como fora indicado na TP3 no ex11
#Fazemos o Teste de Dunn
install.packages("dunn.test")
library(dunn.test)
aceleracao <- dados$Acceleration
cilindros <- dados$Cylinders

# Cria uma matriz com as variáveis aceleracao e cilindros 
Data <- data.frame(aceleracao, cilindros)

#H0: p > 0.05 H1: p <= 0.05
#Ou seja se o valor do p-value do resultado do teste Dunn for menor que alfa = 0.05
#Vamos rejeita H0 e concluir que existe diferenças significativas entre os gurpos 
#em questão
resultado_Dunn <- dunn.test(Data$aceleracao, Data$cilindros, method = "bonferroni")
#Com os valores obtidos podemos concluir que existem diferenças significativas entre os 
#veiculos de 4 e 8  cilindros pois o seu p-value é menor do que 0.0001 assim menor que alfa. Ou seja rejeita-se H0
#Com os valores obtidos podemos concluir que existem diferenças significativas entre os
#veiculos de 6 e 8  cilindros pois o seu p-value é menor do que 0.0001 assim menor que alfa. Ou seja rejeita-se H0
#Com os valores obtidos podemos concluir que não existem diferenças significativas entre os
#veiculos de 4 e 6 cilindros pois o seu p-value é maior que alfa.

#3 b) i. 
#Passar a variavel numerica para categorica
#Evita que a relação entre os cilindros e a aceleração seja considerada linear
#Assim a regressao linear tera valores ajustados às categorias
dados$Cylinders <- factor(dados$Cylinders)

# Encontrar o modelo de regressão linear
modelo <- lm(Acceleration ~ Cylinders + Weight + Horsepower , data = dados)

# Exibir os resultados
summary(modelo)
#
#Com base nos resultados da regressão linear podemos concluir o seguinte
#Os cilindros, o peso e a potencia tem uma influencia significativa na aceleração
#dos veiculos de acordo com os valores de t.
#Apesar do coeficiente de regressão do peso ser positivo,ou seja, quanto maior
# o peso maior a aceleração mas pelo valor estimado podemos dizer que 
#não tem uma grande influencia na aceleração comparativamente com os cilindros 
#e o "Horsepower"
#Os coeficientes de regressão dos cilindros é negativo e podemos concluir que 
#quantos mais cilindros mais negativamente será afetada a aceleração do veiculo
#O "Horsepower" também tem um coeficiente de regressão negativo ou seja quanto maior
#for o horsepower do veiculo menor será a aceleração.
#Como Pr(>|t|) nos dois tipos de cilindros, peso e horsepower foram abaixo de alfa=0.05
#Podemos dizer que mais significativo é o seu coeficiente, ou seja, estes 
#são menos provaveis de terem acontecido por acaso


#3 b) ii.
#Dados do veiculo para estimar a aceleração
novo_dado <- data.frame(Cylinders = 4, Weight = 2950, Horsepower = 100)
#Definir de novo o cilindro como categoria
novo_dado$Cylinders <- factor(novo_dado$Cylinders)
# usar o modelo para prever a aceleração
previsao <- predict(modelo, novo_dado)

# exibir a previsão
previsao
# O resultado previsto médio para um veiculo com 2950 kg, 100 de hp e 4 cilindros 
#seria de 17.30784 m/s^2. Ou seja aceleração prevista seria de 17.30784.