#Estatística Paramétrica e não paramétrica
#Dia 4: Teste de Kruskal-Wallis e Freadman
#Professora: Edneide Ramalho

#Exercício 1: Tempo e trabalho -----

#1. Instalando as bibliotecas -----

library(tidyverse)
#Erro ao importar a biblioteca tidyverse: there is no package called ‘reprex’
#Para corrigir o erro, instalei a biblioteca "reprex"
install.packages("reprex", dependencies = TRUE)

library(ggpubr)
library(rstatix)

#2. Importando a base de dados ----
getwd()
setwd("C:/Users/criso/Desktop/estatística_univasf/aula4_8nov/")

library(readxl)
tempo_trabalho <- read_excel("tempo_trabalho_.xlsx")


#Erro ao chamar a função glimpse: could not find function "glimpse"
#Para contornar o erro importei a biblioteca "dplyr" antes de rodar a função glimpse
library(dplyr)
glimpse(tempo_trabalho)

#Salvando a base de dados original em outro objeto
dados <- tempo_trabalho

#3. Estatística Descritiva ----

estatistica_descritiva <- dados %>% 
  group_by(idade) %>% 
  get_summary_stats(tempo, type = "full")

#Erro ao chamar a função group_by
#Para contornar o erro importei a biblioteca "magrittr" antes de rodar a função group_by
library(magrittr)

#4. Visualização ------
#? ggboxplot (para pedir help no rstudio)
ggboxplot(dados,  x = "idade", y = "tempo", bxp.errorbar = TRUE, add= "jitter")

#5. Computando o teste ------
#se não passar no teste de normalidade, nem precisa ir para o anova
dados %>% 
  group_by(idade) %>% 
  shapiro_test(tempo)

#Os p-value são maiores que 0.05, ou seja,
#Existem evidências que os dados seguem uma distribuição normal
#Vamos usar um teste paramétrico

resultado_kruskal_wallis <- dados %>% 
  kruskal_test(tempo ~ idade)
resultado_kruskal_wallis

#H0: Há influência do fator Idade sobre a variável tempo (em dias) para conseguir um emprego
#H1: Não há influência do fator Idade sobre a variável tempo (em dias) para conseguir um emprego

#Resultado: p-value = 0.039

#Teste de Dunn com correção de bonferroni
#faz a correção para gente não inflar o erro do tipo 1

resultado_post_hoc <- dados %>% 
  dunn_test(tempo ~ idade,
            p.adjust.method = "bonferroni")
resultado_post_hoc

#Tamanho do efeito
dados %>% 
  kruskal_effsize(tempo ~ idade)

#Resultado: tempo    14   0.405 eta2[H] large 
#Ou seja, o efeito entre essas duas variáveis é grande
estatistica_descritiva

#Exercício 2: Arco e Flecha -----

#1. Instalando as bibliotecas -----

library(tidyverse)
#Erro ao importar a biblioteca tidyverse: there is no package called ‘reprex’
#Para corrigir o erro, instalei a biblioteca "reprex"
install.packages("reprex", dependencies = TRUE)

library(ggpubr)
library(rstatix)

#2. Importando a base de dados ----
getwd()
setwd("C:/Users/criso/Desktop/estatística_univasf/aula4_8nov/")

library(readxl)
arco_flecha <- read_excel("arco_e_flecha.xlsx")


#Erro ao chamar a função glimpse: could not find function "glimpse"
#Para contornar o erro importei a biblioteca "dplyr" antes de rodar a função glimpse
library(dplyr)
glimpse(arco_flecha)

#Salvando a base de dados original em outro objeto
dados <- arco_flecha

#3. Estatística Descritiva ----

estatistica_descritiva <- dados %>% 
  group_by(jogador) %>% 
  get_summary_stats(pontuacao, type = "full")

#Erro ao chamar a função group_by
#Para contornar o erro importei a biblioteca "magrittr" antes de rodar a função group_by
library(magrittr)

#4. Visualização ------
#? ggboxplot (para pedir help no rstudio)
ggboxplot(dados,  x = "jogador", y = "pontuacao", bxp.errorbar = TRUE, add= "jitter")

#5. Computando o teste ------
#se não passar no teste de normalidade, nem precisa ir para o anova
dados %>% 
  group_by(jogador) %>% 
  shapiro_test(pontuacao)

resultado_kruskal_wallis <- dados %>% 
  kruskal_test(jogador ~ pontuacao)
resultado_kruskal_wallis

#Resultado: p-value = 0.797

#Teste de Dunn com correção de bonferroni
#faz a correção para gente não inflar o erro do tipo 1

resultado_post_hoc <- dados %>% 
  dunn_test(jogador ~ pontuacao,
            p.adjust.method = "bonferroni")
resultado_post_hoc

#Tamanho do efeito
dados %>% 
  kruskal_effsize(jogador ~ pontuacao)

#Resultado: jogador    24  -0.146 eta2[H] large
#Ou seja, o efeito entre essas duas variáveis é grande
estatistica_descritiva