library(readr)

# 1. Comece por carregar o ficheiro (“ciclismo.csv”) para o ambiente do R, verifique a
#sua dimensão e obtenha um sumário dos dados. 
#dados <- read_csv("C:/Users/josed/OneDrive/Ambiente de Trabalho/ISEP/ANADI/ciclismo.csv")
dados <- read_csv("Faculdade(WORK)/3º ANO/ANADI/Trabalho 1/TP2/ciclismo.csv")
#dados <- read_csv("C:/Users/artur.muiria/Desktop/ANADI/tp_anadi_3dg_1191419_1161274_1190811/TP2/ciclismo.csv")

View(dados)

#Exercicio 2

# Instalação do pacote lubridate
#install.packages("lubridate")                             
library("lubridate")  

# A linha abaixo calcula a idade de cada ciclista ao fazer a diferença entre a data atual e a data de nascimento
# e insere as idades numa nova coluna dos dados 
dados$Age <- round(time_length(difftime(Sys.Date(), dados$dob), "years"))

dim(dados)
summary(dados)

#Exercicio 3
# Analise os atributos do conjunto de dados mais significativos, usando gráficos,
#análises estatísticas e/ou outros métodos apropriados. 

#Intervalos de idade
intervalos <- c(0, 20, 30, max(dados$Age))

# Criar a nova coluna categórica para representar os intervalos de idade
dados$intervalo_idades <- cut(dados$Age, breaks = intervalos, labels = c("0-20", "21-30", "31-40"))

# Boxplot dos vo2_results
boxplot(dados$vo2_results,
        ylab = "VO2_results")

library(ggplot2)
#Em relação á altitude
ggplot(dados, aes(x = Background, y = altitude_results, fill = intervalo_idades)) +
  geom_boxplot() +
  labs(title = "Boxplot - Resultados do Treino Altitude",
       x = "Perfil do ciclista",
       y = "altitude_results",
       fill = "Intervalo de Idades") +
  facet_wrap(~ gender, ncol = 2) +
  theme_bw() +
  theme(legend.position = "top")

# Boxplot dos vo2_results filtrados por genero, background e intervalo de idade
ggplot(dados, aes(x = Background, y = vo2_results, fill = intervalo_idades)) +
  geom_boxplot() +
  labs(x = "Perfil do ciclista",
       y = "vo2_results",
       fill = "Intervalo de Idades") +
  facet_wrap(~ gender, ncol = 2) +
  theme_bw() +
  theme(legend.position = "top")

#Agora para hr_results
ggplot(dados, aes(x = Background, y = hr_results, fill = intervalo_idades)) +
  geom_boxplot() +
  labs(x = "Background", 
       hr_results = "hr_results",
       fill = "Intervalo de Idades") +
  facet_wrap(~ gender, ncol = 2) +
  theme_bw() +
  theme(legend.position = "top")


# 4. Realize o pré-processamento dos dados:
colnames(dados)[5] <- "pro_level"
colnames(dados)[6] <- "winter_camp"

# a) Faça a identificação de NA e limpe o dataSet, se aplicável 

#Identifica os NA, se existirem
which(is.na(dados),arr.ind=TRUE)
#Elimina as linhas, se existirem
dados<-dados[complete.cases(dados), ]

# b) Identifique dados inconsistentes e outliers, se aplicável 
#install.packages("reshape2")    
library(reshape2)
#install.packages("ggplot2")    
library(ggplot2)

boxplot(altitude_results ~ gender ,data=dados,main = names(dados[2]))
boxplot(altitude_results ~ Team ,data=dados,main = names(dados[3]))
boxplot(altitude_results ~ Background ,data=dados,main = names(dados[4]))
boxplot(altitude_results ~ pro_level ,data=dados,main = names(dados[5]))
boxplot(altitude_results ~ winter_camp ,data=dados,main = names(dados[6]))
boxplot(altitude_results ~ vo2_results ,data=dados,main = names(dados[8]))
boxplot(altitude_results ~ hr_results ,data=dados,main = names(dados[9]))
boxplot(altitude_results ~ dob ,data=dados,main = names(dados[10]))
boxplot(altitude_results ~ Continent ,data=dados,main = names(dados[11]))
boxplot(altitude_results ~ Age ,data=dados,main = names(dados[12]))
boxplot(dados$altitude_results, main = names(dados[7]))

# c) Implemente a seleção de atributos, se aplicável 
dados$ID<-NULL
#install.packages("randomForest")
library(randomForest)

# Atributos preditores (X) e a variável alvo (Y)
X <- dados[, c("Background", "pro_level", "winter_camp", "altitude_results", "hr_results", "dob", "Continent", "Age", "gender")]
Y <- dados$vo2_results
X$Background <- as.factor(X$Background)
X$winter_camp <- as.factor(X$winter_camp)
X$Continent <- as.factor(X$Continent)
X$Age <- as.factor(X$Age)
X$pro_level <- as.factor(X$pro_level)
X$gender <- as.factor(X$gender)

model_randomforest <- randomForest(X, Y, importance = TRUE)
importancia <- importance(model_randomforest)
print(importancia)
# winter_camp, altitude_results, hr_results e o age têm a maior importância

# d)  Implemente a normalização dos dados, se necessário 
para_norm <- c("altitude_results", "hr_results", "vo2_results")

for (aux in para_norm) {
  min_valor <- min(dados[[aux]], na.rm = TRUE)
  max_valor <- max(dados[[aux]], na.rm = TRUE)
  dados[[aux]] <- (dados[[aux]] - min_valor) / (max_valor - min_valor)
}

# Passa os dados do tipo "categorias" para numerico
cor(dados[,sapply(dados,is.numeric)])

dados[sapply(dados, is.factor)] <- data.matrix(dados[sapply(dados, is.factor)])

dados[] <- lapply(dados, function(x) {
  if (is.character(x)) {
    as.numeric(factor(x))
  } else {
    x
  }
})

View(dados)

#5 Crie um diagrama de correlação entre todos os atributos. Comente o que observa. 
#install.packages("corrplot")
library(corrplot)

# Criação do diagrama de correlação
corrplot(round(cor(dados[,sapply(dados,is.numeric)]), digits=3), tl.cex = 0.75)
correlation_matrix <- cor(dados[,sapply(dados,is.numeric)])

head(dados)

#6 Obtenha um modelo de regressão linear simples para determinar a variável
set.seed(1) #para os valores serem sempre os mesmos
index <- sample(1:nrow(dados),as.integer(0.7*nrow(dados)))
conjunto_treino <- dados[index,]
conjunto_teste <- dados[-index,]

#a) Apresente a função linear resultante. 
rls <- lm(altitude_results~vo2_results,data = conjunto_treino)
summary(rls)
#AltitudeResults=0.03397+(vo2_results*0.85563)

#b)Visualize a reta correspondente ao modelo de regressão linear simples e o 
#respetivo diagrama de dispersão. 

#Diagrama de dispersão
plot(conjunto_treino$vo2_results,conjunto_treino$altitude_results, col = "orange", pch = 20)
#Reta correspondente ao modelo de regressãp linear simples
abline(rls$coefficients[1],rls$coefficients[2],col = 'blue')

#c) Calcule o erro médio absoluto (MAE) e raiz quadrada do erro médio (RMSE) do 
#modelo sobre os 30% casos de teste. 

rls_predict <- predict(rls, conjunto_teste)
drls <- conjunto_teste$altitude_results-rls_predict

#MAE
mae <- mean(abs(drls))
print(paste("MAE:", mae))
#RMSE
rmse <- sqrt(mean(drls^2))
print(paste("RMSE:", rmse))

#d)
# Modelo de regressão linear múltipla
rl_multipla <- lm(altitude_results ~ vo2_results + hr_results, data = conjunto_treino)
rl_predict_multipla <- predict(rl_multipla, newdata = conjunto_teste)

#MAE
mae <- mean(abs(rl_predict_multipla - conjunto_teste$altitude_results))
#RMSE
rmse <- sqrt(mean((rl_predict_multipla - conjunto_teste$altitude_results)^2))

# Exibir as métricas
print(paste("MAE:", mae))
print(paste("RMSE:", rmse))

#Podemos concluir que os resultados são os mesmos

#7/8
#a)
#instalar se nao tiverem instalado
library("caret")
library("car")

# Ajustar o modelo de regressão linear múltipla
modelo_lm <- lm(formula = vo2_results ~ gender + pro_level + winter_camp + altitude_results + hr_results, data = conjunto_treino)

# Visualizar os coeficientes do modelo
summary(modelo_lm)

# Avaliar o desempenho do modelo de regressão linear múltipla
predict_lm <- predict(modelo_lm, newdata = conjunto_teste)
MAE_lm <- caret::MAE(predict_lm, conjunto_teste$vo2_results)
RMSE_lm <- caret::RMSE(predict_lm, conjunto_teste$vo2_results)
R2_lm <- caret::R2(predict_lm, conjunto_teste$vo2_results)

# Exibir métricas de desempenho
cat("Regressão Linear Múltipla:\n")
cat("MAE:", MAE_lm, "\n")
cat("RMSE:", RMSE_lm, "\n")
cat("R^2:", R2_lm, "\n")

avPlots(modelo_lm)
#Resultados
#MAE: 0.04177241
#RMSE: 0.05171938
#R^2: 0.9125436 

# Podemos concluir que a partir do MAE que representa a média das diferenças 
# absolutas entre as previsões do modelo e os valores que o desempenho do modelo
#foi preciso por ser um valor próximo de 0.
# Já na Raiz Quadrada do Erro médio tem a capacidade de avaliar a precisão do modelo
# Como também o valor é bastante proximo de 0 demonstra que o modelo foi preciso
# Agora no R^2 (Coeficiente de Determinação) que varia de 0 a 1 em que ' é impreciso
# e 1 de um ajuste perfeito. Demonstra que o existe cerca de 91,25% de variabilidade do 
#v02 pode ser explicada pelas variaveis independentes incluídas no modelo.
#Esse valor é bastante postivo demonstrando que a regressão foi bem feita.

#7/8
#b)
library(rpart)
library(rpart.plot)

#Arvore de regressao e definição de 3 digitos
rpart.model <- rpart(vo2_results ~ gender + pro_level + winter_camp + altitude_results + hr_results, data = conjunto_treino)
rpart.plot(rpart.model, digits = 3)

#Previsao dos resultados com o conjunto teste
rpart_predict <- predict(rpart.model, conjunto_teste)

# Cálculo do MAE
MAE <- MAE(rpart_predict, conjunto_teste$vo2_results)

# Cálculo do RMSE
RMSE <- RMSE(rpart_predict, conjunto_teste$vo2_results)

# Cálculo do R^2
R2 <- R2(rpart_predict, conjunto_teste$vo2_results)

# Exibir as métricas
cat("MAE:", MAE, "\n")
cat("RMSE:", RMSE, "\n")
cat("R^2:", R2, "\n")

#Resultados
#MAE: 0.04798744
#RMSE: 0.06048881 
#R^2: 0.8795292

#7/8
#c)
#importar a biblioteca da rede neuronal
library(neuralnet)

#funçao que cria a rede neuronal
redeNeuronal <- neuralnet(vo2_results ~ gender + pro_level + winter_camp + altitude_results + hr_results, data = conjunto_treino, hidden = c(9,2))

#Mostra o grafico da rede neuronal
plot(redeNeuronal)

#Previsao dos resultados com o conjunto teste 
redeNeuronalPredict <- predict(redeNeuronal, conjunto_teste)

#Cálculo das metricas
MAERedeNeuronal <- MAE(redeNeuronalPredict, conjunto_teste$vo2_results)

RMSERedeNeuronal <- RMSE(redeNeuronalPredict, conjunto_teste$vo2_results)

R2RedeNeuronal <- R2(redeNeuronalPredict, conjunto_teste$vo2_results)

# Exibir as métricas
cat("MAE:", MAERedeNeuronal, "\n")
cat("RMSE:", RMSERedeNeuronal, "\n")
cat("R^2:", R2RedeNeuronal, "\n")

#Resultados
#MAE: 0.04303626
#RMSE: 0.05315448 
#R^2: 0.9077979 

#9)

#Apesar de serem poucas observações
#Foi realizado o t.test apenas para termos um teste formal para esta caso

tRegLin = c(MAE_lm, RMSE_lm)
tNeuronal = c(MAERedeNeuronal, RMSERedeNeuronal)

#Testar se há diferenças significativas entre os dois modelos
#H0: tRegLin = tNeuronal
#H1: tRegLin != tNeuronal
t.test(tNeuronal, tRegLin)
#p-value = 0.9652
#Como p > alfa (0.05) não se pode rejeita rH0 e por isso e assim não é possível
#Demonstrar que existem diferenças significativas entre os dois modelos.


# ---------------------------------------------------------- 4.2 Classificação -----------------------------------------------------------
#1. Estude a capacidade preditiva relativamente ao atributo “Pro_level” usando
#os seguintes métodos: árvore de decisão; rede neuronal;K-vizinhos-mais-próximos.
#install.packages("caret")
library(caret)
# Arvore de Decisão --------------------------------------

#holdout
dados_previsao <- subset(dados, select = c(pro_level,gender, altitude_results, vo2_results, hr_results))
amostra<-sample(1:nrow(dados_previsao), 0.7*nrow(dados_previsao))
dados_modelo <- amostra_normalizada[amostra, ]
dados_teste <- amostra_normalizada[-amostra, ]

library(rpart)
arvore <- rpart(pro_level ~.,data = dados_modelo, method="class")
#plot tree
par(xpd = TRUE)
plot(arvore, compress = TRUE);
text(arvore, use.n = TRUE)
#install.packages("rpart.plot")
library("rpart.plot")
rpart.plot(arvore)


# Avaliação do modelo de decisão
arvore_predict <- predict(arvore,dados_teste, type='class')
head(arvore_predict)

print("matriz de confusão árvore decisão")
m.conf<-table(dados_teste$pro_level,arvore_predict)
print(m.conf)

taxa_precisao<-100*round((m.conf[1,1])/(m.conf[1,1]+m.conf[1,2]),4)
cat(paste("precisão árvore decisão: ",taxa_precisao,"%")) 

taxa_accuracy <- 100*round((m.conf[1,1]+m.conf[2,2])/sum(m.conf),4)
cat(paste("accuracy árvore decisão: ",taxa_accuracy,"%")) 

taxa_erro<-100-taxa_accuracy
cat(paste("taxa de erro árvore decisão: ",taxa_erro,"%")) 

taxa_sensitivity<-100*round((m.conf[1,1])/(m.conf[1,1]+m.conf[2,1]),4)
cat(paste("sensitivity árvore decisão: ",taxa_sensitivity,"%")) 

taxa_specificity <-100*round(specificity(m.conf),4)
cat(paste("specificity árvore decisão: ",taxa_specificity,"%")) 

taxa_f1<-round(2*taxa_precisao*taxa_sensitivity/(taxa_precisao+taxa_sensitivity),4)
cat(paste("F1 árvore decisão: ",taxa_f1,"%"))


# Rede Neuronal----------------------------------------------
dados_previsao <- subset(dados, select = c(pro_level,gender, altitude_results, vo2_results, hr_results))

# Função que Normalizacao os dados
minmax <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}


amostra_normalizada <- as.data.frame(apply(dados_previsao,2,minmax))

amostra <- sample(1:nrow(amostra_normalizada),as.integer(0.7*nrow(amostra_normalizada))) #Método de holdout para os dados normalizados 
amostra_treino_normalizado <- amostra_normalizada[amostra,]
amostra_teste_normalizado <- amostra_normalizada[-amostra,]

#install.packages("neuralnet")
library(neuralnet)

#-------------------------------
# 1 internal node
numnodes <- 1
#rede neuronal
redeNeuronal2 <- neuralnet(pro_level ~ gender + altitude_results + vo2_results + hr_results
                           ,data = amostra_treino_normalizado, hidden = numnodes) #rede neuronal
redeNeuronal2$result.matrix
plot(redeNeuronal2)

#-------------------------------
# 3 internal nodes
numnodes <- 3
#rede neuronal
redeNeuronal2 <- neuralnet(pro_level ~ gender + altitude_results + vo2_results + hr_results
                           ,data = amostra_treino_normalizado, hidden = numnodes) #rede neuronal
redeNeuronal2$result.matrix
plot(redeNeuronal2)

#-------------------------------
# 2 internal levels: 6,2 nodes
numnodes <- c(6, 2)
redeNeuronal2 <- neuralnet(pro_level ~ gender + altitude_results + vo2_results + hr_results
                           ,data = amostra_treino_normalizado, hidden = numnodes, linear.output = F) #rede neuronal
redeNeuronal2$result.matrix
plot(redeNeuronal2)


# Avaliação do modelo de decisão
redeNeuronal_predict<-compute(redeNeuronal2,amostra_teste_normalizado)
redeNeuronal_predict$net.result<-sapply(redeNeuronal_predict$net.result,round,digits=0)

print("matriz de confusão rede neuronal")
m.conf_RN<-table(amostra_teste_normalizado$pro_level,redeNeuronal_predict$net.result)
print(m.conf_RN)

taxa_precisao_RN<-100*round((m.conf_RN[1,1])/(m.conf_RN[1,1]+m.conf_RN[1,2]),4)
cat(paste("precisão rede neuronal: ",taxa_precisao_RN,"%")) 

taxa_accuracy_RN <- 100*round((m.conf_RN[1,1]+m.conf_RN[2,2])/sum(m.conf_RN),4)
cat(paste("accuracy rede neuronal: ",taxa_accuracy_RN,"%")) 

taxa_erro_RN<-100-taxa_accuracy_RN
cat(paste("taxa de erro rede neuronal: ",taxa_erro_RN,"%")) 

taxa_sensitivity_RN<-100*round((m.conf_RN[1,1])/(m.conf_RN[1,1]+m.conf_RN[2,1]),4)
cat(paste("sensitivity rede neuronal: ",taxa_sensitivity_RN,"%")) 

taxa_specificity_RN <-100*round(specificity(m.conf_RN),4)
cat(paste("specificity rede neuronal: ",taxa_specificity_RN,"%")) 

taxa_f1_RN<-round(2*taxa_precisao_RN*taxa_sensitivity_RN/(taxa_precisao_RN+taxa_sensitivity_RN),4)
cat(paste("F1 rede neuronal: ",taxa_f1_RN,"%"))


#  c) K-vizinhos-mais-próximos ----------------------------------
library(class)
#install.packages("FNN")
library(FNN)

ran <- sample(1:nrow(amostra_normalizada), 0.7 * nrow(amostra_normalizada))

#Training Labels
train_labels <- amostra_normalizada[ran, "pro_level"]
#Testing Labels
test_labels <- amostra_normalizada[-ran, "pro_level"]

#Training data
data_train <- amostra_normalizada[ran,2:5]
#Testing data
data_test <- amostra_normalizada[-ran,2:5]

k <- c()
accuracy <- c()
taxa_precisao_K_max<-c()
taxa_accuracy_K_max<-c()
taxa_sensitivity_K_max<-c()
taxa_specificity_K_max<-c()
taxa_f1_K_max<-c()

for (i in seq(1, 50, 2)) {
  knn.pred <- knn(train=data_train,
                  test=data_test,
                  cl=train_labels,
                  k=i) 
  
  cfmatrix <- table(knn.pred,test_labels)
  
  accuracy <- c(accuracy, sum(diag(cfmatrix))/sum(cfmatrix))
  
  taxa_precisao_K <- round((cfmatrix[1,1])/(cfmatrix[1,1]+cfmatrix[1,2]),4)
  taxa_precisao_K_max<-c(taxa_precisao_K_max, taxa_precisao_K)
  
  taxa_accuracy_k <- round((cfmatrix[1,1]+cfmatrix[2,2])/sum(cfmatrix),4)
  taxa_accuracy_K_max<-c(taxa_accuracy_K_max, taxa_accuracy_k)
  
  taxa_sensitivity_k<-round((cfmatrix[1,1])/(cfmatrix[1,1]+cfmatrix[2,1]),4)
  taxa_sensitivity_K_max<-c(taxa_sensitivity_K_max, taxa_sensitivity_k)
  
  taxa_specificity_k <-round(specificity(cfmatrix),4)
  taxa_specificity_K_max<-c(taxa_specificity_K_max, taxa_specificity_k)
  
  taxa_f1_k<-round(2*taxa_precisao_K*taxa_sensitivity_k/(taxa_precisao_K+taxa_sensitivity_k),4)
  taxa_f1_K_max<- c(taxa_f1_K_max, taxa_f1_k)
  
  k <- c(k,i)
}

k_max<-k[which.max(taxa_accuracy_K_max)]
cat("k max: ",k[which.max(taxa_accuracy_K_max)])

max_accuracy<-which.max(taxa_accuracy_K_max)
cat(paste("accuracy K-vizinhos-mais-próximos: ",taxa_accuracy_K_max[max_accuracy]*100,"%"))

max_precisao<-which.max(taxa_precisao_K_max)
cat(paste("precisão K-vizinhos-mais-próximos: ",taxa_precisao_K_max[max_precisao]*100,"%"))

max_sensitivity<- which.max(taxa_sensitivity_K_max)
cat(paste("sensitivity K-vizinhos-mais-próximos: ",taxa_sensitivity_K_max[max_sensitivity]*100,"%"))

max_specificity<-which.max(taxa_specificity_K_max)
cat(paste("specificity K-vizinhos-mais-próximos: ",taxa_specificity_K_max[max_specificity]*100,"%"))

max_f1<-which.max(taxa_f1_K_max)
cat(paste("F1 K-vizinhos-mais-próximos: ",taxa_f1_K_max[max_f1]*100,"%"))

#========================================================

#a) Usando o método k-fold cross validation obtenha a média e o desvio padrão da
#taxa de acerto da previsão do atributo “Pro_level” com os dois melhores
#modelos obtidos na alínea anterior.
# e
#c) Verifique se existe diferença significativa no desempenho dos dois melhores
#modelos obtidos anteriormente (use um nível de significância de 5%).
#Identifique o modelo que apresenta o melhor desempenho

#(aplicou-se rede neuronal(accuracy: 69%) e k-vizinhos-mais-próximos
#(accuracy: 67.33 %): 2 melhores modelos)

kf<-10

# Rede Neuronal 
accuracy_rede<-numeric()
recall_rede<- numeric()
precision_rede<-numeric()

# KNN
accuracy_knn_list<-numeric()
recall_knn<- numeric()
precision_knn<-numeric()

folds<-sample(1:kf, nrow(amostra_normalizada),replace = TRUE)
k_value<- k_max #29
cv.error<-matrix(nrow = kf,ncol = 2)


for(i in 1:kf){
  
  train.cv<-amostra_normalizada[folds!=i, 2:5]
  test.cv<-amostra_normalizada[folds==i, 2:5]
  
  train.pro_level<-amostra_normalizada[folds!=i, "pro_level"]
  test.pro_level<-amostra_normalizada[folds==i, "pro_level"]
  
  #Rede Neuronal 
  rede<-as.formula("pro_level ~ gender + altitude_results + vo2_results + hr_results")
  pro_level.net <-neuralnet(rede, data = amostra_normalizada[folds==i, ], hidden = c(10,10), rep = 5,
                            act.fct = "logistic", linear.output = F,
                            lifesign = "minimal", stepmax = 1000000, threshold = 0.001)
  
  
  pro_level.prediction <- compute(pro_level.net, amostra_normalizada[folds!=i, ])
  pro_level.prediction$net.result <- sapply(pro_level.prediction$net.result, round, digits = 0)
  m.conf_rede<-table(pro_level.prediction$net.result, train.pro_level)
  
  accuracy_rede[i] =(m.conf_rede [1,1] + m.conf_rede[2,2])/sum(m.conf_rede)
  recall_rede[i]= m.conf_rede [1,1] / (m.conf_rede[1,1] + m.conf_rede[1,2])
  precision_rede[i]=m.conf_rede [1,1] /(m.conf_rede[1,1] + m.conf_rede[2,1])
  
  # KNN
  knn.pred<-knn(train=train.cv, test=test.cv,cl= train.pro_level)
  m.conf_knn<-table(test.pro_level, knn.pred)
  
  accuracy_knn_list[i] =(m.conf_knn [1,1] + m.conf_knn[2,2])/sum(m.conf_knn)
  recall_knn[i]= m.conf_knn [1,1] / (m.conf_knn[1,1] + m.conf_knn[1,2])
  precision_knn[i]=m.conf_knn [1,1] /(m.conf_knn[1,1] + m.conf_knn[2,1])
  
  #ex:1c)
  cv.error[i,]<-c(sum(diag(m.conf_knn) )/sum(m.conf_knn),sum(diag(m.conf_rede))/sum(m.conf_rede))
}
cat(paste("taxa acerto media rede neuronal:",100* mean(accuracy_rede,4)," desvio padrao:",sd(accuracy_rede,4)))
cat(paste("recall rede neuronal:",100* mean(recall_rede,4)," desvio:",sd(recall_rede,4)))
cat(paste("precisao rede neuronal:",100* mean(precision_rede,4)," desvio:",sd(precision_rede,4)))

cat(paste("taxa acerto media knn:",100* mean(accuracy_knn_list,4)," desvio padrao:",sd(accuracy_knn_list,4)))
cat(paste("recall knn:",100* mean(recall_knn,4)," desvio:",sd(recall_knn,4)))
cat(paste("precisao knn:",100* mean(precision_knn,4)," desvio:",sd(precision_knn,4)))

#ex:1c)
t.test(cv.error[1,],cv.error[2,])
#p-value = 0.5341
#p-value dá maior que 0.05, o que se conclui que não se pode rejeitar a hipotese
#nula, e que nao existem evidencias estatistica com o nivel 
#de significancia de 5% que provem que existem diferenças entre os 2 modelos

#1b) Dos três modelos o kNN é um algoritmo de aprendizagem supervisionada que 
#armazena os exemplos de treino com rótulos durante a fase de treino,
#ele procura os k exemplos mais próximos no conjunto de treino para cada 
#exemplo de teste e faz uma previsão com base nos rótulos desses vizinhos mais próximos. 
# Por esse motivo o kNN também é conhecido por algoritmo "Lazy Learning"
#porque o processamento dos exemplos de treino é adiado até a realização
#novamente de previsões, o treino consiste em armazenar apenas os dados de treino.
#As implicações deste tipo de modelo são as seguintes: não requer um treino
#explícito, é sensível aos dados de treino e pode ser computacionalmente intensivo
#durante a fase de teste. È necessário uma escolha adequada do valor k para obter
#bons resultados.

#1d)
#Accuracy
#Árvore de decisão: 65%
#Rede neuronal: 69%
#K-vizinhos-mais-próximos: 67.33%

#Sensitivity
#Árvore de decisão: 79.17%
#Rede neuronal: 46.88%
#K-vizinhos-mais-próximos: 36.45%

#Specificity 
#Árvore de decisão: 63.77%
#Rede neuronal: 71.64%
#K-vizinhos-mais-próximos: 92.23%

#F1
#Árvore de decisão: 26.5786%
#Rede neuronal: 24.3871%
#K-vizinhos-mais-próximos: 40.24%




#2. Estude a capacidade preditiva relativamente ao atributo “Winter_training_camp”
#usando os seguintes métodos: árvore de decisão; rede neuronal;

set.seed(1)

# Arvore de Decisão --------------------------------------

#holdout
dados_previsao_winter_camp <- subset(dados, select = c(winter_camp, altitude_results, vo2_results, hr_results))
amostra_normalizada_winter_camp <- as.data.frame(apply(dados_previsao_winter_camp,2,minmax))

amostra_winter_camp<-sample(1:nrow(dados_previsao_winter_camp), 0.7*nrow(dados_previsao_winter_camp))
dados_modelo_winter_camp <- amostra_normalizada_winter_camp[amostra_winter_camp, ]
dados_teste_winter_camp <- amostra_normalizada_winter_camp[-amostra_winter_camp, ]

library(rpart)
arvore_winter_camp <- rpart(winter_camp ~.,data = dados_modelo_winter_camp, method="class")
#plot tree
par(xpd = TRUE)
plot(arvore_winter_camp, compress = TRUE);
text(arvore_winter_camp, use.n = TRUE)
#install.packages("rpart.plot")
library("rpart.plot")
rpart.plot(arvore_winter_camp)


# Avaliação do modelo de decisão
arvore_winter_camp_predict <- predict(arvore_winter_camp,dados_teste_winter_camp, type='class')
head(arvore_winter_camp_predict)

print("matriz de confusão árvore decisão")
m.conf_winter_camp<-table(dados_teste_winter_camp$winter_camp,arvore_winter_camp_predict)
print(m.conf_winter_camp)

taxa_precisao_winter_camp<-100*round((m.conf_winter_camp[1,1])/(m.conf_winter_camp[1,1]+m.conf_winter_camp[1,2]),4)
cat(paste("precisão árvore decisão: ",taxa_precisao_winter_camp,"%")) 

taxa_accuracy_winter_camp <- 100*round((m.conf_winter_camp[1,1]+m.conf_winter_camp[2,2])/sum(m.conf_winter_camp),4)
cat(paste("accuracy árvore decisão: ",taxa_accuracy_winter_camp,"%")) 

taxa_erro_winter_camp<-100-taxa_accuracy_winter_camp
cat(paste("taxa de erro árvore decisão: ",taxa_erro_winter_camp,"%")) 

taxa_sensitivity_winter_camp<-100*round((m.conf_winter_camp[1,1])/(m.conf_winter_camp[1,1]+m.conf_winter_camp[2,1]),4)
cat(paste("sensitivity árvore decisão: ",taxa_sensitivity_winter_camp,"%")) 

taxa_specificity_winter_camp <-100*round(specificity(m.conf_winter_camp),4)
cat(paste("specificity árvore decisão: ",taxa_specificity_winter_camp,"%")) 

taxa_f1_winter_camp<-round(2*taxa_precisao_winter_camp*taxa_sensitivity_winter_camp/(taxa_precisao_winter_camp+taxa_sensitivity_winter_camp),4)
cat(paste("F1 árvore decisão: ",taxa_f1_winter_camp,"%"))


# Rede Neuronal----------------------------------------------

redeNeuronal3 <- neuralnet(winter_camp ~ altitude_results + vo2_results + hr_results
                           ,data = dados_modelo_winter_camp, hidden = numnodes, linear.output = F) #rede neuronal

plot(redeNeuronal3)


# Avaliação do modelo de decisão
redeNeuronal_predict_winter_camp<-compute(redeNeuronal3,dados_teste_winter_camp)
redeNeuronal_predict_winter_camp$net.result<-sapply(redeNeuronal_predict_winter_camp$net.result,round,digits=0)

print("matriz de confusão rede neuronal")
m.conf_RN_winter_camp<-table(dados_teste_winter_camp$winter_camp,redeNeuronal_predict_winter_camp$net.result)
print(m.conf_RN_winter_camp)

taxa_precisao_RN_winter_camp<-100*round((m.conf_RN_winter_camp[1,1])/(m.conf_RN_winter_camp[1,1]+m.conf_RN_winter_camp[1,2]),4)
cat(paste("precisão rede neuronal: ",taxa_precisao_RN_winter_camp,"%")) 

taxa_accuracy_RN_winter_camp <- 100*round((m.conf_RN_winter_camp[1,1]+m.conf_RN_winter_camp[2,2])/sum(m.conf_RN_winter_camp),4)
cat(paste("accuracy rede neuronal: ",taxa_accuracy_RN_winter_camp,"%")) 

taxa_erro_RN_winter_camp<-100-taxa_accuracy_RN_winter_camp
cat(paste("taxa de erro rede neuronal: ",taxa_erro_RN_winter_camp,"%")) 

taxa_sensitivity_RN_winter_camp<-100*round((m.conf_RN_winter_camp[1,1])/(m.conf_RN_winter_camp[1,1]+m.conf_RN_winter_camp[2,1]),4)
cat(paste("sensitivity rede neuronal: ",taxa_sensitivity_RN_winter_camp,"%")) 

taxa_specificity_RN_winter_camp <-100*round(specificity(m.conf_RN_winter_camp),4)
cat(paste("specificity rede neuronal: ",taxa_specificity_RN_winter_camp,"%")) 

taxa_f1_RN_winter_camp<-round(2*taxa_precisao_RN_winter_camp*taxa_sensitivity_RN_winter_camp/(taxa_precisao_RN_winter_camp+taxa_sensitivity_RN_winter_camp),4)
cat(paste("F1 rede neuronal: ",taxa_f1_RN_winter_camp,"%"))


#a) Usando o método k-fold cross validation obtenha a média e o desvio padrão da
#taxa de acerto da previsão do atributo “Winter_training_camp” com os dois melhores
#modelos obtidos na alínea anterior.
# e
#b) Verifique se existe diferença significativa no desempenho dos dois melhores
#modelos obtidos anteriormente (use um nível de significância de 5%).
#Identifique o modelo que apresenta o melhor desempenho

kf<-10

#Arvore Decisão
accuracy_arvore<-numeric()
recall_arvore<- numeric()
precision_arvore<-numeric()

# KNN
accuracy_knn_list<-numeric()
recall_knn<- numeric()
precision_knn<-numeric()

folds<-sample(1:kf, nrow(amostra_normalizada_winter_camp),replace = TRUE)
k_value<- k_max #29
cv.error<-matrix(nrow = kf,ncol = 2)


for(i in 1:kf){
  
  train.cv<-amostra_normalizada_winter_camp[folds!=i, 2:4]
  test.cv<-amostra_normalizada_winter_camp[folds==i, 2:4]
  
  train.winter_camp<-amostra_normalizada_winter_camp[folds!=i, "winter_camp"]
  test.winter_camp<-amostra_normalizada_winter_camp[folds==i, "winter_camp"]
  
  #Árvore de decisão
  arvore<-rpart(winter_camp ~., method="class",data=amostra_normalizada_winter_camp[folds!=i, ])
  arvore_predict<-predict(arvore, amostra_normalizada_winter_camp[folds==i, ],type='class')
  m.conf_arvore<-table(amostra_normalizada_winter_camp[folds==i, ]$winter_camp,arvore_predict)
  
  accuracy_arvore[i] =(m.conf_arvore [1,1] + m.conf_arvore[2,2])/sum(m.conf_arvore)
  recall_arvore[i]= m.conf_arvore [1,1] / (m.conf_arvore[1,1] + m.conf_arvore[1,2])
  precision_arvore[i]=m.conf_arvore [1,1] /(m.conf_arvore[1,1] + m.conf_arvore[2,1])
  
  #Rede Neuronal 
  rede<-as.formula("winter_camp ~ altitude_results + vo2_results + hr_results")
  winter_camp.net <-neuralnet(rede, data = amostra_normalizada_winter_camp[folds==i, ], hidden = c(10,10), rep = 5,
                            act.fct = "logistic", linear.output = F,
                            lifesign = "minimal", stepmax = 1000000, threshold = 0.001)
  
  
  winter_camp.prediction <- compute(winter_camp.net, amostra_normalizada_winter_camp[folds!=i, ])
  winter_camp.prediction$net.result <- sapply(winter_camp.prediction$net.result, round, digits = 0)
  m.conf_rede<-table(winter_camp.prediction$net.result, train.winter_camp)
  
  accuracy_rede[i] =(m.conf_rede [1,1] + m.conf_rede[2,2])/sum(m.conf_rede)
  recall_rede[i]= m.conf_rede [1,1] / (m.conf_rede[1,1] + m.conf_rede[1,2])
  precision_rede[i]=m.conf_rede [1,1] /(m.conf_rede[1,1] + m.conf_rede[2,1])
  
  #ex:2)
  cv.error[i,]<-c(sum(diag(m.conf_arvore) )/sum(m.conf_arvore),sum(diag(m.conf_rede))/sum(m.conf_rede))
}
cat(paste("taxa acerto media arvore:",100* mean(accuracy_arvore,4)," desvio padrao:",sd(accuracy_arvore,4)))
cat(paste("recall arvore:",100* mean(recall_arvore,4)," desvio:",sd(recall_arvore,4)))
cat(paste("precisao arvore:",100* mean(precision_arvore,4)," desvio:",sd(precision_arvore,4)))

cat(paste("taxa acerto media rede:",100* mean(accuracy_rede,4)," desvio padrao:",sd(accuracy_rede,4)))
cat(paste("recall rede:",100* mean(recall_rede,4)," desvio:",sd(recall_rede,4)))
cat(paste("precisao rede:",100* mean(precision_rede,4)," desvio:",sd(precision_rede,4)))

#ex:2b)
t.test(cv.error[1,],cv.error[2,])
#p-value = 0.4617
#p-value dá maior que 0.05, o que se conclui que não se pode rejeitar a hipotese
#nula, e que nao existem evidencias estatistica com o nivel 
#de significancia de 5% que provem que existem diferenças entre os 2 modelos


#2c)
#Accuracy
#Árvore de decisão: 67.67%
#Rede neuronal: 67.33%

#Sensitivity
#Árvore de decisão: 55%
#Rede neuronal: 52.73%

#Specificity 
#Árvore de decisão: 69.62%
#Rede neuronal: 70.61%

#F1
#Árvore de decisão: 31.2034%
#Rede neuronal: 37.1778%

#2a) 
#taxa acerto media arvore: 69.3772893772894  desvio padrao: 0.0430219096456042
#taxa acerto media rede: 60.8322466891117  desvio padrao: 0.0238806625079828