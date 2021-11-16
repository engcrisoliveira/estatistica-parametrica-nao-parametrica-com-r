#Estatística Paramétrica e não Paramétrica
#Dia 2: Exercício teste-t para amostras independentes
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
tooth_growth <- read_excel("ToothGrowth.xlsx")

#3. Teste t -----

#3.1 Estatística descritiva -----

#Visão geral das variáveis na base de dados

#Erro ao chamar a função glimpse: could not find function "glimpse"
#Para contornar o erro importei a biblioteca "dplyr" antes de rodar a função glimpse
library(dplyr)
glimpse(tooth_growth)

#Quantas observações há em cada grupo de suplemento?

#Fazendo uma tabela para a variável suplemento
tooth_growth %>%
  sample_n(5)
table(tooth_growth$supp)

#Resposta: Há 30 observações há em cada grupo de suplemento

#Gerar as estatísticas de resumo para a variável len agrupada por supp
tooth_growth %>% 
  group_by(supp) %>% 
  get_summary_stats(len, type="common") #para "n", the number of individuals

#Usar média e desvio padrão como as estatísticas descritivas escolhidas

#média
#OJ: comprimento médio = 20.7
#VC: comprimento médio = 17.0

#desvio padrão
#OJ: desvio padrão = 6.61
#VC: desvio padrão = 8.27

#3.2 Primeira Visualização -----

#Gerar um gráfico (boxplot) para comparar a variável "len" em relação à "supp"

#Visualizando os dados
bxp_supp<- tooth_growth %>% 
  ggboxplot(x="supp", y="len")
bxp_supp

#Inserindo barra de erros
bxp_supp <- tooth_growth %>% 
  ggboxplot(x="supp", y="len", add="jitter")
bxp_supp

#3.3 Suposições do teste -----
  
#Cheque se há outliers extremos nos dados
tooth_growth %>% 
  group_by(supp) %>% 
  identify_outliers(len)

#Resultado: 0 linhas> (ou row.names de comprimento 0)
#Ous seja, não há outliers extremos

#Cheacar a normalidade dos dados entre os grupos usando o teste de Shapiro-Wilk
tooth_growth %>% 
  group_by(supp) %>% 
  shapiro_test(len)

#Resultado:
#OJ: p-value = 0,0236
#VC: p-value = 0,428

#Checar a normalidade dos dados entre os grupos usando o QQplot

#Checando a normalidade com QQ plot
tooth_growth %>% 
  ggqqplot(x="len", facet.by="supp")

#Podemos observar que o qqplot mostra que os dados do suplemento "OJ" estão mais distantes da linha
#Além disso, o (p-value) resultante do teste de shapiro é menor que 0.05
#Isso quer dizer que, tanto o qqplot quanto o teste de Shapiro_Wilk
#mostram que os dados não são normalmente distribuídos

#Podemos observar que o qqplot mostra que os dados do suplemento "VC" estão mais próximos da linha
#Além disso, o (p-value) resultante do teste de shapiro é maior que 0.05
#Isso quer dizer que, tanto o qqplot quanto o teste de Shapiro_Wilk
#mostram que os dados são normalmente distribuídos

#Checar se ambos os grupos de suplemento tem variâncias iguais

#Para avaliar a igualdade de variâncias, podemos usar o teste de Levene
tooth_growth %>% 
  levene_test(len ~ supp)

#O p-value é 0,275, ou seja, as variâncias são iguais
#Pois, as variância são iguais sempre q o p-value for maior que 0,05

#Calculando o tamanho do efeito
teste_comprimento_suplemento <- tooth_growth %>% 
  t_test(len ~ supp, var.equal = FALSE) %>% 
  add_significance()
teste_comprimento_suplemento

#O teste_comprimento_suplemento, tem p-value = 0,0606, ou seja, 
#o resultado do teste informa que o efeito é não significativo.

#Finalizando a visualização inicial, com a modificação do título dos eixos
#e adicionando as informações do teste realizado

teste_comprimento_suplemento <-  teste_comprimento_suplemento %>% 
  add_xy_position(x= "supp")

bxp_supp +
  stat_pvalue_manual(teste_comprimento_suplemento, tip.length = 0) +
  labs(subtitle = get_test_label(teste_comprimento_suplemento, detailed = TRUE),
       x = "Suplemento", y= "Comprimento (cm)")

#Resultado do Teste

#Resultado: t(55.31) = 1.92, p = 0.061, n=60
#O teste t de Welch para duas amostras foi utilizado e mostrou que a diferença entre os 
#comprimentos foi estatiscamente significativa