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

#3.Teste Wilcoxon -----

##3.1 Exemplo 1: amostra pequena -----

#Os dados para este exemplo são referentes a colheitas de espigas de milho (em libras por acre) de dois
#diferentes tipos de sementes (normais e secas no forno) que foram usados em lotes adjacentes. O objetivo do
#estudo é testar se existe diferença entre a colheita utilizando ambas as sementes.

dados_seed <- dados <- data.frame(Semente = c(rep("Normal",11), rep("Seco",11)),
                                  Rendimento = c(seed$'Regular seed',seed$'Kiln-dried seed'))
Seca <- dados_seed[dados_seed$Semente == "Seco",]
Normal <- dados_seed[dados_seed$Semente == "Normal",]
completo <- data.frame(Normal,Seca)
completo

###Investigação Inicial -----
ggplot(dados_seed, aes(x = Semente, y = Rendimento)) + geom_boxplot() +
  labs(x = "Semente", y = "Rendimento", title =
         "Boxplot para o rendimento dos tipos de sementes") + theme_bw()

ggplot(dados_seed, aes(Rendimento, fill= Semente)) + geom_density(alpha=.5) +
  labs(title="Densidade do Rendimento por semente", x="Rendimento", y="Densidade")

###Teste bilateral de Wicoxon -----
wilcox.test(Normal$Rendimento,Seca$Rendimento,paired = T,alternative =
              "two.sided", conf.level = 0.95)

#Resultado: V = 15, p-value = 0.123
#alternative hypothesis: true location shift is not equal to 0

#O resultado do teste mostra que não há evidências contra a hipótese H0, ou seja,
#não parece haver diferença estatísticamente significativa entre os tipos de sementes.

##3.2 Exemplo 2: amostras grandes -----

#Vamos considerar o mesmo exemplo, agora com um banco de dados maior

Rendimento1 <- rnorm(500, 1300, 80)
Rendimento2 <- rnorm(500, 1700, 100)
Seed <- data.frame(Semente = c(rep("Normal",500), rep("Seco",500)),
                   Rendimento = c(Rendimento1, Rendimento2))

###Investigação Inicial -----
ggplot(Seed, aes(x = Semente, y = Rendimento)) + geom_boxplot() +
  labs(x = "Semente", y = "Rendimento", title =
         "Boxplot para o rendimento dos tipos de sementes") + theme_bw()

ggplot(Seed, aes(Rendimento, fill= Semente)) + geom_density(alpha=.5) +
  labs(title="Densidade do Rendimento por semente", x="Rendimento", y="Densidade")

###Teste unilateral de Wilcoxon -----
wilcox.test(Rendimento1, Rendimento2, paired = T, alternative =
              "two.sided", conf.level = 0.95)

#Resultado: V = 0, p-value < 2.2e-16

wilcox.test(Rendimento1, Rendimento2, paired = T, alternative =
              "greater", conf.level = 0.95)

#Há evidências contra a hipótese H0, pode-se dizer que parece haver diferença estatísticamente
#significativa entre os tipos de sementes. Além disso, com o teste unilateral, pode-se ainda dizer que o
#Rendimento2 (sementes secas) parece ser maior do que o Rendimento1 (sementes normais).