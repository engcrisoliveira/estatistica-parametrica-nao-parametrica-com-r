#Estatística Paramétrica e não Paramétrica
#Dia 2: Exercício teste-t pareado
#Professora: Edneide Ramalho


#1. Importando as bibliotecas -----
library(tidyverse)
library(ggpubr)
library(rstatix)

#Erro ao importar a biblioteca tidyverse: there is no package called ‘reprex’
#Para corrigir o erro, instalei a biblioteca "reprex"
install.packages("reprex", dependencies = TRUE)

#2. Importando os dados -----
library(readxl)
notas <- read_excel("notas.xlsx")

#Transformando os dados par ao formato longo
notas_long <- notas %>% 
  gather(key= "group", value = "notas", antes, depois)
head(notas_long)

#3. Teste t -----

#3.1 Estatística descritiva -----

#Visão geral das variáveis na base de dados

#Quantas observações há em cada grupo?
#Resposta: Há 20 observações com notas em "antes" e com 20 notas em "depois"

#Gerar as estatísticas de resumo para a variável len agrupada por supp
notas_long %>% 
  group_by(group) %>% 
  get_summary_stats(notas, type = "mean_sd")


#Criando a coluna diferença na base de dados notas, para computar a diferença nas notas antes e depois do curso.
notas <-  notas %>% 
  dplyr::mutate(diferenca = depois - antes)
head(notas)


#3.2 Visualização -----

#Visualizando os dados
bxp_pareado <-  ggpaired(notas_long,
                         x = "group",
                         y = "notas",
                         order = c("antes", "depois"),
                         ylab = "Notas",
                         xlab = "Grupos")
bxp_pareado



#3.3 Suposições do teste -----


#Testando as suposições para a realização de um teste t pareado

#Cheque se há outliers extremos nos dados
notas %>% 
  identify_outliers(diferenca)

#Resultado: <0 linhas> (ou row.names de comprimento 0)
#Ou seja, não há outliers extremos

#Checando normalidade
notas %>% 
  shapiro_test(diferenca)
#statistic = 0.923
#p-value = 0.114

#Executando o teste t pareado
teste_pareado <-  notas_long %>% 
  t_test(notas ~ group, paired = TRUE) %>% 
  add_significance()
View(teste_pareado)

#Calculando o tamanho do efeito
tamanho_efeito_pareado <-  notas_long %>% 
  cohens_d(notas ~group, paired = TRUE)

#Gerando o gráfico final do teste t pareado usando o ggpaired e também o ggboxplot
teste_pareado <- teste_pareado %>% 
  add_xy_position(x = "group")

bxp_pareado +
  stat_pvalue_manual(teste_pareado, tip.length =  0) +
  labs(subtitle = 
         get_test_label(teste_pareado, detailed = TRUE))