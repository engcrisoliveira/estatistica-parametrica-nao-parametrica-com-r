#Estatística Paramétrica e não paramétrica
#Dia 1: Como instalar o R/ Normalidade dos dados/Dados pareados e não pareados
#Professora: Paula Shinozaki

#1. Importando a base de dados ----

#Vendo o caminho de arquivo do diretório de trabalho atual
getwd()

#Definindo o diretório de trabalho para dir
setwd("C:/Users/criso/Desktop/estatística_univasf/aula1_3nov/")

#Lendo o arquivo
tarefa <-  read.table("tarefa.txt", head=T)

#Anexando no R as minhas notas
attach(tarefa)#ctrl+R$
Tarefa

#2. Shapiro-Wilk Normality Test ----

#H0: Os dados seguem uma distribuição normal
#H1: Os dados não seguem uma distribuição normal

shapiro.test(Tarefa)
#Resultado: W = 0.94049, p-value = 0.1033

#Nosso valor de p (p-value) é maior que 0.05 (5%) e por isso, aceitamos H0 (hipótese nula)
#Logo existem evidências que os dados seguem uma distribuição normal
#Neste caso, devemos usar testes paramétricos

#3. Teste Kolmogorov-Smirnov ----
x <- rnorm(29)
x

#H0: Os dados seguem uma distribuição normal
#H1: Os dados não seguem uma distribuição normal

#Help de como usar o teste de kolmogorov
#??kolmogorov

ks.test(Tarefa,x)
#Resultado: D = 0.27586, p-value = 0.2221
#Alternative hypothesis: two-sided

#Como o p-value > 0.05, aceita a H0 (hipótese nula)
#Logo, existem evidências que os dados seguem uma distribuição normal
