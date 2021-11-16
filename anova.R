#Estatística paramétrica a não paramétrica
#Dia 3: Anova e Anova com medidas repetidas no R 
#Professora: Elaine Marques
#Monitora: Macileide Oliveira


#1. Importando as bibliotecas -----
install.packages("car") 
library(car) 

#2. Importando os dados -----

#Vendo o caminho de arquivo do diretório de trabalho atual
getwd()

#Definindo o diretório de trabalho para dir
setwd("C:/Users/criso/Desktop/estatística_univasf/aula3_5nov/")

medicamento <- read.csv("anova1.csv",sep=";", header=T)
medicamento

# Visualize sua planilha de dados
View(medicamento)

#3. Aplicando a Anova para um único fator -----

#A função do R que executa a ANOVA é a aov ou lm.
# exemplo: aov(V.Dependente ~ V.Independente , data=dados)
modeloanova <- aov(HORAS ~ REMEDIO, data =medicamento)
modeloanova

#Resultado: Residual standard error: 2.645751

# Função summary forncece mais informações em relação ao modelo
summary(modeloanova)

#variável dependente: HORAS
#Aplicando a Anova para dois fatores
modeloanova2 =  aov(HORAS ~ REMEDIO * SEXO, data=medicamento)
modeloanova2

#Resultado: Residual standard error: 2.645751
#Apresentou o mesmo resultado, isso significa que a variável sexo não interfere no resultado

summary(modeloanova2)

#Identifique se existe difirença entre os grupos
tukey = TukeyHSD(modeloanova2)
tukey
plot(tukey)

#Testando as premissas da ANOVA

#3.1. Homogeneidade das amostras -----

#Para homogeneidade de variância entre os grupos
leveneTest(HORAS ~ REMEDIO, data = medicamento)

#3.2. Normalidade dos resíduos -----

#A premissa da ANOVA referente à normalidade dos resíduos pode ser 
#testada através do teste de Shapiro-Wilk:

#H0 = distribuição dos dados = normal --> p >0.05
#H1 = distribuição dos dados diferente normal --> p >0.05
shapiro.test(resid(modeloanova))

#Resultado:W = 0.93962, p-value = 0.2359

#Nosso valor de p (p-value) é maior que 0.05 (5%) e por isso, aceitamos H0 (hipótese nula)
#Logo existem evidências que os dados seguem uma distribuição normal
#Neste caso, devemos usar testes paramétricos

#ANOVA DE MEDIDAS REPETIDAS
avalia<-c(2,5,1,7,2,6,4,7,3,9,4,8,3,5,1,9,6,8,3,6,2,8,1,4)
sujeito<-c(1:6, 1:6, 1:6, 1:6)
juiz<-c(rep("1",6), rep("2",6) , rep("3",6), rep("4",6))
b<-lm(avalia ~as.factor(sujeito) + as.factor(juiz))
anova(b)

#Alpha de Cronbach
install.packages("ltm")
library(ltm)
juiz1<-c(2,5,1,7,2,6)
juiz2<-c(4,7,3,9,4,8)
juiz3<-c(3,5,1,9,6,8)
juiz4<-c(3,6,2,8,1,4)
dados<-data.frame(juiz1,juiz2,juiz3, juiz4)
cronbach.alpha(dados, standardized = FALSE, CI = TRUE, 
               probs = c(0.025, 0.975), B = 1000, na.rm = FALSE)