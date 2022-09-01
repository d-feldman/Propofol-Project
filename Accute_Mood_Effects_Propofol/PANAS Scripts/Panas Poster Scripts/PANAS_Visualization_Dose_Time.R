#PANAS
#visualization
library(ggplot2)
library (ggpubr)


#####High Vs Low Dose Randomized#####
###Negative Scales
PANAS_Sadness <- ggline(D_hl, x= 'Visit_Number', y= 'PANAS_Sadness_Difference', color = '#2980B9', add = 'mean_se', 
                   linetype = c('solid','solid','solid','solid','solid','solid')) + 
                        xlab('Visit Number') + ylab('Difference Scores')  + labs(title= 'A1) Sadness          p<0.001 ***') +
                        theme(legend.position = 'top', legend.background = element_rect(fill = "#F4F6F6"),
                              plot.background = element_rect(fill = "#F4F6F6"), plot.title = element_text(face='bold')) +
                              expand_limits( x= 0, y= 0) + scale_x_discrete(position = 'top')

PANAS_Fear <- ggline(D_hl, x= 'Visit_Number', y= 'PANAS_Fear_Difference',
                     color = '#2980B9', add = 'mean_se', 
                     linetype = c('solid','solid','solid','solid','solid','solid')) +labs(title = 'A2) Fear                  p<0.001 ***') +
                       xlab('Visit Number') + ylab('Difference Scores') +
                        theme(legend.position = 'NONE', plot.background = element_rect(fill = "#F4F6F6"), 
                              plot.title = element_text(face='bold')) + expand_limits( x= 0, y= 0) + 
                        scale_x_discrete(position = 'top')

PANAS_Neg <- ggarrange(PANAS_Sadness, PANAS_Fear, ncol=1, nrow=2, heights = c(1.2,1)) 
PANAS_Neg <-  annotate_figure(PANAS_Neg,
              top = text_grob("PANAS Negative Emotive States",
              color = 'black', face = 'bold', size = 14))



###Positive Scales###
PANAS_Joviality <- ggline(D_hl, x= 'Visit_Number', y= 'PANAS_Joviality_Difference',
                        color = '#F5B041', add = 'mean_se', 
                        linetype = c('solid','solid','solid','solid','solid','solid')) + 
  xlab('Visit Number') + ylab('Difference Scores')  + labs(title='B1) Joviality            p<0.01 **') +
  theme(legend.position = 'top', legend.background = element_rect(fill = "#F4F6F6"),
        plot.background = element_rect(fill = "#F4F6F6"), plot.title = element_text(face='bold')) + expand_limits( x= 0, y= 0)

PANAS_Attentiveness <- ggline(D_hl, x= 'Visit_Number', y= 'PANAS_Attentiveness_Difference',
                     color = '#F5B041', add = 'mean_se', 
                     linetype = c('solid','solid','solid','solid','solid','solid')) +
  labs (title = 'B2) Attentiveness') + xlab('Visit Number') + ylab('Difference Scores') +
  theme(legend.position = 'NONE', plot.background = element_rect(fill = "#F4F6F6"), 
        plot.title = element_text(face='bold')) + expand_limits( x= 0, y= 0) + 
         scale_x_discrete(position = 'top')

PANAS_Pos <- ggarrange(PANAS_Joviality, PANAS_Attentiveness, ncol=1, nrow=2, heights = c(1.2,1)) 
PANAS_Pos <-  annotate_figure(PANAS_Pos,
                              top = text_grob("PANAS Positive Emotive States",
                                              color = 'black', face = 'bold', size = 13))



####Fatigue####
PANAS_Fatigue <- ggline(D_hl, x= 'Visit_Number', y= 'PANAS_Fatigue_Difference', color = '#CD6155', add = 'mean_se', 
                          linetype = c('solid','solid','solid','solid','solid','solid')) + 
  xlab('Visit Number') + ylab('Difference Scores')  + labs(title = 'C) Fatigue') +
  theme(legend.position = 'NONE', legend.background = element_rect(fill = "#F4F6F6"),
        plot.background = element_rect(fill = "#F4F6F6"), plot.title = element_text(face='bold')) + expand_limits( x= 0, y= 0)
  
PANAS_Fatigue <- annotate_figure(PANAS_Fatigue,
                  top = text_grob("PANAS Fatigue",
                             color = 'black', face = 'bold', size = 13))


####Serenity###
PANAS_Serenity <- ggline(D_hl, x= 'Visit_Number', y= 'PANAS_Serenity_Difference', color = '#17A589', add = 'mean_se', 
                        linetype = c('solid','solid','solid','solid','solid','solid')) + 
  xlab('Visit Number') + ylab('Difference Scores')  + labs(title = 'D) Serenity ') +
  theme(legend.position = 'NONE', legend.background = element_rect(fill = "#F4F6F6"),
        plot.background = element_rect(fill = "#F4F6F6"), plot.title = element_text(face='bold'))  + expand_limits( x= 0, y= 0)

PANAS_Serenity <- annotate_figure(PANAS_Serenity,
                                 top = text_grob("PANAS Serenity",
                                                 color = 'black', face = 'bold', size = 13))
###Arrange
PANAS_HL <- ggarrange(PANAS_Neg, PANAS_Pos, PANAS_Fatigue, PANAS_Serenity, ncol=2, nrow=2,
                    labels = c('A','B', 'C', 'D'), heights=(c(2,1)))

PANAS_HL <- annotate_figure(PANAS_HL,
              top = text_grob("Acute Mood Effects of Propofol Over Treatment Course",
                                  color = 'black', face = c('bold'), size = 16)) + 
              theme(plot.background = element_rect(fill = "#FADBD8"))

##Print
print(PANAS_HL)


