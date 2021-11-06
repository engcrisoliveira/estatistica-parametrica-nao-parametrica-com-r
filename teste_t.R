# 1. Pacotes -----

library(tidyverse)
library(ggpubr)
library(rstatix)
library(dplyr)

# 2. Carregando a base de dados -----

library(readxl)
genderweight <- read_excel("C:/Users/criso/Desktop/estatística_p/genderweight.xlsx")
View(genderweight)


glimpse(genderweight)

table(genderweight$group)

#Símbolo %>% é o Pipe
#Atalho control + shift + M
# %>% 

#Exemplo do uso do pipe
x <- c(12, 15, 17, 18, 18)
mean(x)

x %>% mean()

#3. Estatística descritiva------

names(genderweight)


genderweight %>%
  groupby(group) %>%
  get_summary_stats(weight, type = "mean_sd")

# 4. Visualização -------

genderweight %>% 
ggboxplot(x="group", y="weight", add = "jitter", bxp.errobar = TRUE)

# 5. Checando suposições/ pressupostos do teste --------

## 5.1 Outliers extremos ------
genderweight %>% 
  group_by(group) %>% 
  identify_outliers(weight)

## 5.2 Normalidade ------

#Teste de Shapiro-Wilk
genderweight %>% 
  group_by(group) %>% 
  shapiro_test(weight)

#H0: dados normais
#H1: dados não normais

#p-valor < 0.05 ou 0.01
# Os dados são normais!!!! \0/

# QQplot

genderweight %>% 
  ggqqplot(x="weight", facet.by= "group") 

## 5.3 Igualdade de variância-------
genderweight %>% 
  levene_test(weight ~ group)

# p < 0.05 -> As variâncias não são iguais

# 6. Computando o teste t -------
resultado_do_teste_t <- genderweight %>% 
  t_test(weight ~ group, var.equal = FALSE) %>% 
  add_significance()

# tamanho do efeito
genderweight %>% 
  cohens_d(weight ~ group)

# d = 6.57 (tamanho do efeito grande!!)

#gráfico final
resultado_do_teste_t <- resultado_do_teste_t %>% 
  add_xy_position(x="group")

bxp_peso <- genderweight %>% 
  ggboxplot(x="group", y= "weight", add="jitter", bxp.errorbar = TRUE)

bxp_peso

# Melhorar o gráfico
bxp_peso +
  stat_pvalue_manual(resultado_do_teste_t, tip.length = 0) +
  labs(subtitle = get_test_label(resultado_do_teste_t, detailed = TRUE, (x="Gênero", y = "Peso")))
    ggsave("teste_t.jpg")
#?ggsave
  
  
  


