#Ficha TP2 2022/2023
espes <- c(30, 30, 30, 30, 31, 32, 32, 32, 32, 33, 33, 34, 34, 34, 35)

mean(espes)

mu0 <- 32.5
alfa=0.05

tobs=(mean(espes)-mu0)/(sd(espes)/sqrt(length(espes))); tobs

p_valor=2*pt(abs(tobs), lower.tail=F, df=length(espes)-1); p_valor

#Usando funcao t.test

result1 <- t.test(espes, mu=mu0, alternativa = "two.sided", conf.level = 1-alfa)
result1

#Como p=0.4132 > 0.05=alfa, nao se rejeita H0. Não há evidencias que nos
#permitam afirmar que a espessura média na população seja significativamente 
#diferente de 32.5

#2
tempos <- c(5, 4, 4, 5, 5, 5, 6, 5, 4, 4, 3, 4, 4, 5, 5, 7, 6, 5, 6, 4, 6, 5, 5, 6, 6, 6, 4, 4, 5, 5, 5, 3, 6, 3, 6, 5)
#a) 
mu0 = 5
#alpha =0.01
#bilateral
tobs=(mean(tempos)-5)/(sd(tempos)/sqrt(length(tempos))); tobs


#b) Para conclui calculamos o p-value associado a to
p_val <- pt(tobs, lower.tail = FALSE, df=length(tempos)-1)
p_val

#c) Resolver e concluir
#COMO VER SE O CLIENTE TEM RAZÃO
# Se p>= alfa -> Nao se rejeita H0
# Se p < alfa -> Rejeita-se H0
alfa <-0.01
result2 <- t.test(tempos, mu=mu0, alternative="greater", conf.level = 1-alfa)
result2

#3
#a) Amostrar independetes pois ,pelo enunciado, não há qualquer emparelhamento 
#entre os ratos das 2 amostras
#Resposta: Independente
#b) H0:miu_valcomfio = miu_velsemfio vs H1:miu_velcomfio |= miu_velsemfio
#(teste bilateral, dist normal e var. desconhecido)



vel_com_fio <- c(2300, 2000, 2800, 2000, 2400, 2200, 2000, 1800, 1900, 2100, 2200, 2400)
vel_sem_fio <- c(2400, 2200, 1800, 1900 ,1800, 1900, 2100, 2050, 2200, 2000, 1900, 2000)
dados <- c(vel_com_fio, vel_sem_fio) ; dados
grupos <- as.factor(c(rep(1, length(vel_com_fio)), rep(2, length((vel_sem_fio))))); grupos

alfa = 0.01


install.packages("car")
library(car)
leveneTest(dados, grupos, center=mean)

#Teste t para 2 amostra independentes, bilateral
#Recap H0: miu_veçcomfio = miu_velsemfio vs H1:miu_velcom |= miu_velsemfio
t.test(vel_com_fio, vel_sem_fio, mu=0, conf.level = 1-alfa, var.equal = T, paired = F)
#Mu=0 e paired =F é o default, não é preciso colocar no argumento
#alternative="two.sided" é o default

#p = 0.1172 > alfa=0.01 -> Não Rejeitar H0
#Com nível de significancia 1%, não existe evidência estatisitica de diferença 
#entre os valores médios de velocidade entre os ratos com e sem fios.


#4
#a)
tensao <- c(263, 254, 261, 236, 228, 253, 249, 262, 250, 252, 257, 258)
alfa =0.05
t.test(tensao, mu=225, alternative="greater", conf.level=1-alfa)

#Como p=0.8345 > 0.05=alfa, nao se reijeita H0. Não há evidencias que nos
#permitam afirmar que a tensao media seja significativamente superior a 255, ou seja,
# os dados nao indicam que a tensao esperada de rotura nao e inferior a 255.

#Nao se rejeita H0
#b) P(X>tr) =0.95     X=tensão
#Como P(X>tr)=0.95 então podemos obter uma estimativa de tr assim:
tr=qnorm(0.95, lower.tail = F, mean=255, sd=sd(tensao)); tr

estat = pnorm(tr,lower.tail = F, mean=255, sd=sd(tensao) ); estat

#5 Dados em Data_C.csv
#Normalidade na variavel => usar test t para amostras emparelhadas
# H0: miu_D60<=miu_D0 vs H1: miu_D60> miu_D0 (unilateral)
t.test(Data_C$D60, Data_C$DO, alternative ="greater", paired=T, conf.level = 1)

#6
dados_ling <- stack(Data_A)
library(car)
leveneTest(dados_ling$values, dados_ling$ind, center = mean)
leveneTest(values~ind, dados_ling, center = mean)

#p=0.6499 > 0.05 => nao rej H0. Podemos assumir homog de variancias 
#Test t: H0 miu_gI >= miugII vs H1: miu_gI<miu_gII
t.test(Data_A$Grupo_1, Data_A$Grupo_2, alternative = "less", paired = F, v.equal=T)

#Como p=0.4527 nao se rej. H0. Não há evid. est. para afirmar que o método II é
#Significativamente melhoor que o métoodo I, para um nível de sig. de 0.05.
