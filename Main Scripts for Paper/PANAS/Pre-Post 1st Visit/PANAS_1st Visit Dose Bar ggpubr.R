###Visualizing things
library(ggplot2)
library(ggpubr)
#manipulating data
library(dplyr)
library(tidyr)

D_results$Group <- factor(D_results$Group, levels = c('Low', 'High'))

names(D_results)[names(D_results) == 'Group'] <- 'Dose'

###Sadness
sad_barplot <- ggbarplot(data = D_results, x = 'Time', y = 'PANAS_Sadness',
                         fill = 'Dose', color = 'black', add = c('mean_se'), 
                         palette = c('#3263a8','#a8323e','#3263a8','#a8323e'), alpha = .6, position = position_dodge(.7), width = .5) + 
  xlab('Time Relative to Treatment') + ylab('Mean Sadness Rating') + theme(legend.position= 'top')

sad_barplot <- annotate_figure(sad_barplot,
                                 top = text_grob("PANAS Sadness Rating",
                                                 color = 'black', face = 'bold', size = 12)) 


### Fear
fear_barplot <- ggbarplot(data = D_results, x = 'Time', y = 'PANAS_Fear',
                          fill = 'Dose', color = 'black', add = c('mean_se'), 
                          palette = c('#3263a8','#a8323e','#3263a8','#a8323e'), alpha = .6, position = position_dodge(.7), width = .5) + 
  xlab('Time Relative to Treatment') + ylab('Mean Fear Rating') + theme(legend.position= 'top')

fear_barplot <- annotate_figure(fear_barplot,
                               top = text_grob("PANAS Fear Rating",
                                               color = 'black', face = 'bold', size = 12)) 

## Joviality
joviality_barplot <- ggbarplot(data = D_results, x = 'Time', y = 'PANAS_Joviality',
                               fill = 'Dose', color = 'black', add = c('mean_se'), 
                               palette = c('#3263a8','#a8323e','#3263a8','#a8323e'), alpha = .6,position = position_dodge(.7), width = .5) + 
  xlab('Time Relative to Treatment') + ylab('Mean Joviality Rating') + theme(legend.position= 'none')

joviality_barplot <- annotate_figure(joviality_barplot,
                               top = text_grob("PANAS Joviality Rating",
                                               color = 'black', face = 'bold', size = 12)) 
##Attentiveness
attentiveness_barplot <- ggbarplot(data = D_results, x = 'Time', y = 'PANAS_Attentiveness',
                                   fill = 'Dose', color = 'black', add = c('mean_se'), 
                                   palette = c('#3263a8','#a8323e','#3263a8','#a8323e'), alpha = .6, position = position_dodge(.7), width = .5) + 
  theme(legend.position= 'none') + xlab('Time Relative to Treatment') + ylab('Mean Attentiveness Rating')

attentiveness_barplot <- annotate_figure(attentiveness_barplot,
                                     top = text_grob("PANAS Attentiveness Rating",
                                                     color = 'black', face = 'bold', size = 12)) 

## Fatigue
fatigue_barplot <- ggbarplot(data = D_results, x = 'Time', y = 'PANAS_Fatigue',
                             fill = 'Dose', color = 'black', add = c('mean_se'), 
                             palette = c('#3263a8','#a8323e','#3263a8','#a8323e'), alpha = .6, position = position_dodge(.7), width = .5)  + 
  theme(legend.position = 'none') + xlab('Time Relative to Treatment') + ylab('Mean Fatigue Rating')

fatigue_barplot <- annotate_figure(fatigue_barplot,
                                     top = text_grob("PANAS Fatigue Rating",
                                                     color = 'black', face = 'bold', size = 12)) 

## Serenity
serenity_barplot <- ggbarplot(data = D_results, x = 'Time', y = 'PANAS_Serenity',
                              fill = 'Dose', color = 'black', add = c('mean_se'), 
                              palette = c('#3263a8','#a8323e','#3263a8','#a8323e'), alpha = .6, position = position_dodge(.7), width = .5) + 
  xlab('Time Relative to Treatment') + ylab('Mean Serenity Rating') + theme(legend.position= 'none') 
#+   annotate("text", x =2.03, y= 69, label = "." , size = 8, color= 'black')

serenity_barplot <- annotate_figure(serenity_barplot,
                                     top = text_grob("PANAS Serenity Rating",
                                                     color = 'black', face = 'bold', size = 12)) 

###Arrange
PANAS_bar_comb <- ggarrange(sad_barplot,fear_barplot,joviality_barplot,attentiveness_barplot,fatigue_barplot,serenity_barplot,
                            ncol=2, nrow=3,
                      labels = c('a','b', 'c', 'd', 'e', 'f'))

PANAS_bar_comb <- annotate_figure(PANAS_bar_comb,
                            top = text_grob("Dose Interaction with Mood Effects of First Propofol Anesthesia",
                                            color = 'black', face = c('bold'), size = 16)) + theme_bw()

##Print
print(PANAS_bar_comb)



