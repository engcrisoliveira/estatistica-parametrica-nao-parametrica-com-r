#Estatística Paramétrica e não paramétrica
#Dia 5: Testes Não Paramétricos para dois grupos:Mann-Whitney e Wilcoxon
#Professora: Erika Fialho

#1. Importando bibliotecas -----
library(rio)
library(tidyverse)

#Erro ao importar a biblioteca tidyverse: there is no package called ‘reprex’
#Para corrigir o erro, instalei a biblioteca "reprex"
install.packages("reprex", dependencies = TRUE)

library(gridExtra)
library(ggplot2)

#2. Importando a base de dados ----

#Vendo o caminho de arquivo do diretório de trabalho atual
getwd()

#Definindo o diretório de trabalho para dir
setwd("C:/Users/criso/Desktop/estatística_univasf/aula5_9nov")

#Lendo o arquivo
library(readxl)
seed <- read_excel("seed.xls")
View(seed)

#3.Teste Mann-Whitney -----

##3.1 Exemplo 1: amostra pequena -----

#Uma pesquisa de mercado foi realizada em uma cidade com a finalidade de comparar duas marcas de
#refrigerante, A e B. Cada participante provou o refrigerante sem que soubesse qual das duas marcas estava
#provando e atribuiu uma nota entre 0 e 10

A <- c(5,7,8,8,4,8,6,7,3,8)
B <- c(9,6,8,7,6,9,7,7,8,6)
Refrigerantes <- data.frame(Marca = c(rep("A",10), rep("B",10)),
                            Nota = c(A, B))
Refrigerantes


###Investigação Inicial -----
ggplot(Refrigerantes, aes(x = Marca, y = Nota)) + geom_boxplot() +
  labs(x = "Marca", y = "Nota", title =
         "Boxplot para as notas de cada refrigerante") + theme_bw()

ggplot(Refrigerantes, aes(Nota, fill= Marca)) + geom_density(alpha=.5) +
  labs(title="Densidade das notas por Refrigerante", x="Notas", y="Densidade")

###Teste bilateral de Wicoxon -----
wilcox.test(A,B, paired = FALSE, alternative = "two.sided", conf.level = 0.95)

#Resultado: W = 38.5, p-value = 0.3937

#Não há evidências contra a hipótese H0, não parece haver diferença estatísticamente significativa
#entre os tipos de refrigerantes.

##3.2 Exemplo 2: amostra grande -----

#Vamos considerar o mesmo exemplo, agora com um banco de dados maior

A2 <- rnorm(2000, 8, 0.5)
B2 <- rnorm(2000, 7.5, 1)
Refrigerantes2 <- data.frame(Marca = c(rep("A",2000), rep("B",2000)), Nota = c(A2, B2))
Refrigerantes2[1:10,]

###Investigação Inicial -----
ggplot(Refrigerantes2, aes(x = Marca, y = Nota)) + geom_boxplot() +
  labs(x = "Marca", y = "Nota", title =
         "Boxplot para as notas de cada refrigerante") + theme_bw()

ggplot(Refrigerantes2, aes(Nota, fill= Marca)) + geom_density(alpha=.3) +
  labs(title="Densidade das notas por Refrigerante", x="Notas", y="Densidade")

###Teste unilateral de Wilcoxon -----

wilcox.test(A2,B2, paired = FALSE, alternative = "two.sided", conf.level = 0.95)
#Resultado: W = 2736633, p-value < 2.2e-16

wilcox.test(A2, B2, paired = FALSE, alternative = "greater", conf.level = 0.95)
#Resultado: W = 2736633, p-value < 2.2e-16


#Há evidências contra a hipótese H0, pode-se dizer que parece haver diferença estatísticamente
#significativa entre os tipos de refrigerante. Além disso, com o teste unilateral, pode-se ainda dizer que a
#mediana das notas do Refrigerante A parece ser maior do que do Refrigerante B.