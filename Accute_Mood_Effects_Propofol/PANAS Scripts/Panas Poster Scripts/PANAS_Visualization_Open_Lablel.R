#PANAS
#visualization
library(ggplot2)
library (ggpubr)


#####High Vs Low Dose Randomized#####
###Negative Scales
PANAS_Sadness <- ggline(D_OL_long, x= 'Visit_Number', y= 'PANAS_Sadness_Difference', group= 'Dose',
                   color= 'Dose' , palette = c('#0073C2', '#CD534C'), add = 'mean_se', 
                   linetype = c('dashed','dashed','dashed','dashed','dashed','dashed',
                                'solid','solid','solid','solid','solid','solid')) + 
                        xlab('Visit Number') + ylab('Sadness Difference Scores')  + labs(title= 'E1)') +
                        theme(legend.position = 'top', legend.background = element_rect(fill = "#F4F6F6"),
                              plot.background = element_rect(fill = "#F4F6F6")) 

PANAS_Fear <- ggline(D_OL_long, x= 'Visit_Number', y= 'PANAS_Fear_Difference',
                     color= 'Dose' , palette = c('#0073C2', '#CD534C'), add = 'mean_se', 
                     linetype = c('dashed','dashed','dashed','dashed','dashed','dashed',
                                  'solid','solid','solid','solid','solid','solid')) +labs(title = 'E2)') +
                       xlab('Visit Number') + ylab('Fear Difference Scores') +
                        theme(legend.position = 'NONE', plot.background = element_rect(fill = "#F4F6F6")) 

PANAS_Neg <- ggarrange(PANAS_Sadness, PANAS_Fear, ncol=1, nrow=2, heights = c(1.2,1)) 
PANAS_Neg <-  annotate_figure(PANAS_Neg,
              top = text_grob("PANAS Negative Emotive States",
              color = 'black', face = 'bold', size = 14))



###Positive Scales###
PANAS_Joviality <- ggline(D_OL_long, x= 'Visit_Number', y= 'PANAS_Joviality_Difference', group= 'Dose',
                        color= 'Dose' , palette = c('#0073C2', '#CD534C'), add = 'mean_se', 
                        linetype = c('dashed','dashed','dashed','dashed','dashed','dashed',
                                     'solid','solid','solid','solid','solid','solid')) + 
  xlab('Visit Number') + ylab('Joviality Difference Scores')  + labs(title='F1)') +
  theme(legend.position = 'top', legend.background = element_rect(fill = "#F4F6F6"),
        plot.background = element_rect(fill = "#F4F6F6"))

PANAS_Attentiveness <- ggline(D_OL_long, x= 'Visit_Number', y= 'PANAS_Attentiveness_Difference',
                     color= 'Dose' , palette = c('#0073C2', '#CD534C'), add = 'mean_se', 
                     linetype = c('dashed','dashed','dashed','dashed','dashed','dashed',
                                  'solid','solid','solid','solid','solid','solid')) +
  labs (title = 'F2)') + xlab('Visit Number') + ylab('Attentiveness Difference Scores') +
  theme(legend.position = 'NONE', plot.background = element_rect(fill = "#F4F6F6"))

PANAS_Pos <- ggarrange(PANAS_Joviality, PANAS_Attentiveness, ncol=1, nrow=2, heights = c(1.2,1)) 
PANAS_Pos <-  annotate_figure(PANAS_Pos,
                              top = text_grob("PANAS Positive Emotive States",
                                              color = 'black', face = 'bold', size = 13))



####Fatigue####
PANAS_Fatigue <- ggline(D_OL_long, x= 'Visit_Number', y= 'PANAS_Fatigue_Difference', group= 'Dose',
                          color= 'Dose' , palette = c('#0073C2', '#CD534C'), add = 'mean_se', 
                          linetype = c('dashed','dashed','dashed','dashed','dashed','dashed',
                                       'solid','solid','solid','solid','solid','solid')) + 
  xlab('Visit Number') + ylab('Fatigue Difference Scores')  + labs(title = '') +
  theme(legend.position = 'NONE', legend.background = element_rect(fill = "#F4F6F6"),
        plot.background = element_rect(fill = "#F4F6F6"))
  
PANAS_Fatigue <- annotate_figure(PANAS_Fatigue,
                  top = text_grob("PANAS Fatigue",
                             color = 'black', face = 'bold', size = 13))


####Serenity###
PANAS_Serenity <- ggline(D_OL_long, x= 'Visit_Number', y= 'PANAS_Serenity_Difference', group= 'Dose',
                        color= 'Dose' , palette = c('#0073C2', '#CD534C'), add = 'mean_se', 
                        linetype = c('dashed','dashed','dashed','dashed','dashed','dashed',
                                     'solid','solid','solid','solid','solid','solid')) + 
  xlab('Visit Number') + ylab('Serenity Difference Scores')  + labs(title = '') +
  theme(legend.position = 'NONE', legend.background = element_rect(fill = "#F4F6F6"),
        plot.background = element_rect(fill = "#F4F6F6")) 

PANAS_Serenity <- annotate_figure(PANAS_Serenity,
                                 top = text_grob("PANAS Serenity",
                                                 color = 'black', face = 'bold', size = 13))
###Arrange
PANAS_HL <- ggarrange(PANAS_Neg, PANAS_Pos, PANAS_Fatigue, PANAS_Serenity, ncol=2, nrow=2,
                    labels = c('E)','F)', 'G)', 'H)'), heights=(c(2,1)))

PANAS_HL <- annotate_figure(PANAS_HL,
              top = text_grob("Propofol Open Label Trial Mood Ratings",
                                  color = 'black', face = c('bold'), size = 16)) +
        theme(plot.background = element_rect(fill = "#FADBD8"))

##Print
print(PANAS_HL)


