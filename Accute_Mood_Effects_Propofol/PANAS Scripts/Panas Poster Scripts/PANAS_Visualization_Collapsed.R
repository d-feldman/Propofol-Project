#PANAS
#visualization
library(ggplot2)
library (ggpubr)


#####High Vs Low Dose Randomized#####
###Negative Scales
PANAS_Sadness <- ggline(D_hl, x= 'Visit_Number', y= 'PANAS_Sadness_Difference', group= 'Dose',
                   color= 'Dose' , palette = 'jco', add = 'mean_se', 
                   linetype = c('dashed','dashed','dashed','dashed','dashed','dashed',
                                'solid','solid','solid','solid','solid','solid')) + 
                        xlab('Visit Number') + ylab('Sadness Difference Scores')  + 
                        theme(legend.position = 'top', legend.background = element_rect(fill = "#F4F6F6"),
                              plot.background = element_rect(fill = "#F4F6F6"))

PANAS_Fear <- ggline(D_hl, x= 'Visit_Number', y= 'PANAS_Fear_Difference',
                     color= 'Dose' , palette = 'jco', add = 'mean_se', 
                     linetype = c('dashed','dashed','dashed','dashed','dashed','dashed',
                                  'solid','solid','solid','solid','solid','solid')) +
                        labs (title = '') + xlab('Visit Number') + ylab('Fear Difference Scores') +
                        theme(legend.position = 'NONE', plot.background = element_rect(fill = "#F4F6F6"))

PANAS_Neg <- ggarrange(PANAS_Sadness, PANAS_Fear, ncol=1, nrow=2) 
PANAS_Neg <-  annotate_figure(PANAS_Neg,
              top = text_grob("PANAS Negative Emotive States",
              color = 'black', face = 'bold', size = 14))



###Positive Scales###
PANAS_Joviality <- ggline(D_hl, x= 'Visit_Number', y= 'PANAS_Joviality_Difference', group= 'Dose',
                        color= 'Dose' , palette = 'jco', add = 'mean_se', 
                        linetype = c('dashed','dashed','dashed','dashed','dashed','dashed',
                                     'solid','solid','solid','solid','solid','solid')) + 
  xlab('Visit Number') + ylab('Joviality Difference Scores')  + 
  theme(legend.position = 'top', legend.background = element_rect(fill = "#F4F6F6"),
        plot.background = element_rect(fill = "#F4F6F6"))

PANAS_Attentiveness <- ggline(D_hl, x= 'Visit_Number', y= 'PANAS_Attentiveness_Difference',
                     color= 'Dose' , palette = 'jco', add = 'mean_se', 
                     linetype = c('dashed','dashed','dashed','dashed','dashed','dashed',
                                  'solid','solid','solid','solid','solid','solid')) +
  labs (title = '') + xlab('Visit Number') + ylab('Attentiveness Difference Scores') +
  theme(legend.position = 'NONE', plot.background = element_rect(fill = "#F4F6F6"))

PANAS_Pos <- ggarrange(PANAS_Joviality, PANAS_Attentiveness, ncol=1, nrow=2) 
PANAS_Pos <-  annotate_figure(PANAS_Pos,
                              top = text_grob("PANAS Positive Emotive States",
                                              color = 'black', face = 'bold', size = 14))



####Fatigue####
PANAS_Fatigue <- ggline(D_hl, x= 'Visit_Number', y= 'PANAS_Fatigue_Difference', group= 'Dose',
                          color= 'Dose' , palette = 'jco', add = 'mean_se', 
                          linetype = c('dashed','dashed','dashed','dashed','dashed','dashed',
                                       'solid','solid','solid','solid','solid','solid')) + 
  xlab('Visit Number') + ylab('Fatigue Difference Scores')  + 
  theme(legend.position = 'top', legend.background = element_rect(fill = "#F4F6F6"),
        plot.background = element_rect(fill = "#F4F6F6"))
  
PANAS_Fatigue <- annotate_figure(PANAS_Fatigue,
                  top = text_grob("PANAS Fatigue",
                             color = 'black', face = 'bold', size = 14))


####Serenity###
PANAS_Serenity <- ggline(D_hl, x= 'Visit_Number', y= 'PANAS_Serenity_Difference', group= 'Dose',
                        color= 'Dose' , palette = 'jco', add = 'mean_se', 
                        linetype = c('dashed','dashed','dashed','dashed','dashed','dashed',
                                     'solid','solid','solid','solid','solid','solid')) + 
  xlab('Visit Number') + ylab('Serenity Difference Scores')  + 
  theme(legend.position = 'top', legend.background = element_rect(fill = "#F4F6F6"),
        plot.background = element_rect(fill = "#F4F6F6"))

PANAS_Serenity <- annotate_figure(PANAS_Serenity,
                                 top = text_grob("PANAS Serenity",
                                                 color = 'black', face = 'bold', size = 14))
###Arrange
PANAS_HL <- ggarrange(PANAS_Neg, PANAS_Pos, PANAS_Fatigue, PANAS_Serenity, ncol=2, nrow=2,
                    labels = c('A','B', 'C', 'D'), heights=(c(2,1)))

PANAS_HL <- annotate_figure(PANAS_HL,
              top = text_grob("Randomized Dose-Dependent Acute Mood Effects of Propofol",
                                  color = 'black', face = 'bold', size = 16))

##Print
print(PANAS_HL)


