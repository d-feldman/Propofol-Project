###Visualizing things
library(ggplot2)
library(ggpubr)
#manipulating data
library(dplyr)
library(tidyr)

my_comparisons<- list(c('Pre','Post'))

###Sadness
sad_barplot <- ggbarplot(data = D_results, x = 'Time', y = 'PANAS_Sadness',
                         fill = 'Time', color = 'black', add = c('mean_se', 'dotplot'), 
                         palette = c('#EFC000','#0073C2'), alpha = .6) + 
  stat_compare_means(comparisons = my_comparisons, method = 't.test', aes(label = ..p.signif..), label.y = c(101)) +
  xlab('Time Relative to Treatment') + ylab('Standardized Sadness Rating') + theme(legend.position= 'none')

sad_barplot <- annotate_figure(sad_barplot,
                                 top = text_grob("PANAS Sadness Rating",
                                                 color = 'black', face = 'bold', size = 12)) 


### Fear
fear_barplot <- ggbarplot(data = D_results, x = 'Time', y = 'PANAS_Fear',
                         fill = 'Time', color = 'black', add = c('mean_se', 'dotplot'), 
                         palette = c('#EFC000','#0073C2'), alpha = .6) + 
  stat_compare_means(comparisons = my_comparisons, method = 't.test', aes(label = ..p.signif..), label.y = c(85)) +
  xlab('Time Relative to Treatment') + ylab('Standardized Fear Rating') + theme(legend.position= 'none')

fear_barplot <- annotate_figure(fear_barplot,
                               top = text_grob("PANAS Fear Rating",
                                               color = 'black', face = 'bold', size = 12)) 

## Joviality
joviality_barplot <- ggbarplot(data = D_results, x = 'Time', y = 'PANAS_Joviality',
                         fill = 'Time', color = 'black', add = c('mean_se', 'dotplot'), 
                         palette = c('#EFC000','#0073C2'), alpha = .6) + 
  stat_compare_means(comparisons = my_comparisons, method = 't.test', aes(label = ..p.signif..), label.y = c(85)) +
  xlab('Time Relative to Treatment') + ylab('Standardized Joviality Rating') + theme(legend.position= 'none')

joviality_barplot <- annotate_figure(joviality_barplot,
                               top = text_grob("PANAS Joviality Rating",
                                               color = 'black', face = 'bold', size = 12)) 
##Attentiveness
attentiveness_barplot <- ggbarplot(data = D_results, x = 'Time', y = 'PANAS_Attentiveness',
                               fill = 'Time', color = 'black', add = c('mean_se', 'dotplot'), 
                               palette = c('#EFC000','#0073C2'), alpha = .6)  +theme(legend.position= 'none') +
          xlab('Time Relative to Treatment') + ylab('Standardized Attentiveness Rating')

attentiveness_barplot <- annotate_figure(attentiveness_barplot,
                                     top = text_grob("PANAS Attentiveness Rating",
                                                     color = 'black', face = 'bold', size = 12)) 

## Fatigue
fatigue_barplot <- ggbarplot(data = D_results, x = 'Time', y = 'PANAS_Fatigue',
                               fill = 'Time', color = 'black', add = c('mean_se', 'dotplot'), 
                               palette = c('#EFC000','#0073C2'), alpha = .6) + theme(legend.position = 'none') + 
  xlab('Time Relative to Treatment') + ylab('Standardized Fatigue Rating')

fatigue_barplot <- annotate_figure(fatigue_barplot,
                                     top = text_grob("PANAS Fatigue Rating",
                                                     color = 'black', face = 'bold', size = 12)) 

## Serenity
serenity_barplot <- ggbarplot(data = D_results, x = 'Time', y = 'PANAS_Serenity',
                               fill = 'Time', color = 'black', add = c('mean_se', 'dotplot'), 
                               palette = c('#EFC000','#0073C2'), alpha = .6) + 
  stat_compare_means(comparisons = my_comparisons, method = 't.test', aes(label = ..p.signif..), label.y = c(105)) +
  xlab('Time Relative to Treatment') + ylab('Standardized Serenity Rating') + theme(legend.position= 'none')

serenity_barplot <- annotate_figure(serenity_barplot,
                                     top = text_grob("PANAS Serenity Rating",
                                                     color = 'black', face = 'bold', size = 12)) 

###Arrange
PANAS_bar_comb <- ggarrange(sad_barplot,fear_barplot,joviality_barplot,attentiveness_barplot,fatigue_barplot,serenity_barplot,
                            ncol=2, nrow=3,
                      labels = c('a','b', 'c', 'd', 'e', 'f'))

PANAS_bar_comb <- annotate_figure(PANAS_bar_comb,
                            top = text_grob("Short-Term Mood Effects of First Propofol Anesthesia",
                                            color = 'black', face = c('bold'), size = 16)) + theme_bw()

##Print
print(PANAS_bar_comb)



