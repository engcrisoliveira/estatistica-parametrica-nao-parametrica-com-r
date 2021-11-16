#Estatística Paramétrica e não paramétrica
#Dia 4: Teste de Kruskal-Wallis e Friedman
#Professora: Edneide Ramalho

#1. Instalando as bibliotecas -----

library(tidyverse)
#Erro ao importar a biblioteca tidyverse: there is no package called ‘reprex’
#Para corrigir o erro, instalei a biblioteca "reprex"
install.packages("reprex", dependencies = TRUE)

library(ggpubr)
library(rstatix)
library(datarium)

#2. Importando a base de dados ----
getwd()
setwd("C:/Users/criso/Desktop/estatística_univasf/aula4_8nov/")

library(readr)
velocidade_atletas <- read_xlsx("velocidade_atletas.xlsx")
df <- velocidade_atletas

#Criando a variável id
df <- df %>% 
  mutate(id = 1:6)

#Erro ao chamar a função glimpse: could not find function "glimpse"
#Para contornar o erro importei a biblioteca "dplyr" antes de rodar a função glimpse
library(dplyr)
glimpse(df)

#Transformando para o formato longo
df2 <- df %>% 
  gather(key = "trecho", value = "velocidade", trecho_a, trecho_b, trecho_c, trecho_d) %>% 
  convert_as_factor(id, trecho)

glimpse(df2)
head(df2)

#3. Estatísticas de Resumo ----

estatisticas_resumo <- df2 %>% 
  group_by(trecho) %>% 
  get_summary_stats(atleta, type = "full")
estatisticas_resumo

#Erro ao chamar a função group_by
#Para contornar o erro importei a biblioteca "magrittr" antes de rodar a função group_by
library(magrittr)

#4. Visualização ------
df2 %>% 
  ggboxplot(x = "trecho", y = "velocidade", add = "jitter", bxp.errorbar = TRUE)

#5. Computando o teste ------

df2 %>% 
  group_by(trecho) %>% 
  shapiro_test(velocidade)

resposta_friedman <- df2 %>% 
  friedman_test(velocidade ~ trecho | id)
resposta_friedman

#Resultado: p = 0.00553

#Tamanho do efeito
df2 %>% 
  friedman_effsize(velocidade ~ trecho | id)

#Resultado: effsize = 0.701
#Ou seja, o tamanho do efeito é grande

#6. Comparações múltiplas -----
teste_post_hoc <- df2 %>% 
  wilcox_test(velocidade ~ trecho, paired = TRUE, p.adjust.method = "bonferroni")
teste_post_hoc

#7. Resultados -----
teste_post_hoc <- teste_post_hoc %>% 
  add_xy_position(x = "trecho")

teste_post_hoc$y.position <- c(7, 6.6, 6.5)

ggboxplot(df2, x = "trecho", 
          y = "velocidade", 
          add = "jitter",
          bxp.errorbar = TRUE) +
  stat_pvalue_manual(teste_post_hoc, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(resposta_friedman, type = "expression", detailed = TRUE),
    caption = get_pwc_label(teste_post_hoc, type = "expression"),
    x = "Trecho", y = "Velocidade"
  )

#Resultado:x²(3) = 12.62, p = 0.0055, n = 6