#0. Preparar o ambiente -----

library(tidyverse)
library(ggpubr)
library(rstatix)


#transformar os dados para o formato longo

mice2.long %>% 
  group_by(group) %>% 
  get_summary_stats(weight, type = "mean_sd")

#3. Visualização -----

bxp_pareado <- ggpaired(mice.long, 
                         x = "group",
                         y = "weight",
                        order = c("before", "after"),
                        ylab = "weight(g)",
                        xlab = "Group")
bxp_pareado

#4. Suposições do teste -----

# iremos acrescentar uma coluna chamada diferenças

mice2 <- mice2 %>% 
  mutate(diferencas = after - before)
mice2

#4.1 normalidade:

# p-value(normalidade)

mice2 %>% 
  shapiro_test(diferencas)

##4.2 Outliers extremos:
 mice2 %>% 
   identify_outliers(diferencas)
 
 #5. Computando o teste ----
  teste_pareado <- mice2.long %>% 
   t_test(weight ~group, paired = TRUE) %>% 
   add_significance()
 
 teste_pareado
 View(teste_pareado)
 
 #tamanho do efeito
 
 mice2.long %>% 
   cohens_d(weight~group, paired = TRUE)
 
 #6. Reportando o resultado ------
 
 teste_pareado <- teste_pareado %>% 
   add_xy_position(x= "group")
 
 bxp_pareado +
   stat_pvalue_manual(teste_pareado, tip.length = 0) +
   lab(substitle = 
         get_test_label(teste_pareado,
                        detailed = TRUE)
            