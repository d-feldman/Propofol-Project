#PANAS
#visualization
library(ggplot2)
library(ggpubr)



#####High Vs Low Dose Randomized#####
###Negative Scales
PANAS_Sadness_BW <- ggplot(data = D_hl, aes(x=Dose, y= PANAS_Sadness_Difference, fill = Dose)) + geom_boxplot(alpha = .75) +
                  xlab('Dose') + ylab('Difference Scores')  + labs(title= 'A1) Sadness             p<0.001 ***')+
                    theme(plot.background = element_rect(fill = "#F4F6F6"), legend.position = 'bottom',
                          legend.background = element_rect(fill = '#EAEDED' ), plot.title = element_text(face='bold')) +
                     guides(color=guide_legend(title = 'Dose')) + scale_fill_manual(values = c('#C0392B','#2471A3')) + 
                    annotate('rect', xmin = .4, xmax =3, ymin = -.25, ymax =.25, alpha = 1) 
                        

PANAS_Fear_BW <-  ggplot(data = D_hl, aes(x=Dose, y= PANAS_Fear_Difference, fill = Dose)) + geom_boxplot(alpha = .75) +
                  xlab('Dose') + ylab('Difference Scores')  + labs(title= 'A2) Fear                    p<0.01 **')+
                  theme(plot.background = element_rect(fill = "#F4F6F6"), legend.position = 'none', 
                 legend.background = element_rect(fill = "#F4F6F6"), plot.title = element_text(face='bold')) +
                   guides(color=guide_legend(title = 'Dose')) + scale_fill_manual(values = c('#C0392B','#2471A3')) +
                   annotate('rect', xmin = .4, xmax =3, ymin = -.25, ymax =.25, alpha = 1)

PANAS_Neg_BW <- ggarrange(PANAS_Sadness_BW, PANAS_Fear_BW, ncol=1, nrow=2, heights = c(1.3,1)) 
PANAS_Neg_BW <-  annotate_figure(PANAS_Neg_BW,
              top = text_grob("PANAS Negative Emotive States",
              color = 'black', face = 'bold', size = 14))



###Positive Scales###
PANAS_Joviality_BW <- ggplot(data = D_hl, aes(x=Dose, y= PANAS_Joviality_Difference, fill = Dose)) + geom_boxplot(alpha = .75) +
                       xlab('Dose') + ylab('Difference Scores')  + labs(title= 'B1) Joviality           p<0.001 ***')+
                      theme(plot.background = element_rect(fill = "#F4F6F6"), legend.position = 'bottom', 
                       legend.background = element_rect(fill = "#EAEDED"), plot.title = element_text(face='bold')) +
                               guides(color=guide_legend(title = 'Dose')) + scale_fill_manual(values = c('#C0392B','#2471A3')) + 
                      annotate('rect', xmin = .4, xmax =3, ymin = -.3, ymax =.3, alpha = 1)

PANAS_Attentiveness_BW <- ggplot(data = D_hl, aes(x=Dose, y= PANAS_Attentiveness_Difference, fill = Dose)) + geom_boxplot(alpha = .75) +
                          xlab('Dose') + ylab('Difference Scores')  + labs(title= 'B2) Attentiveness')+
                          theme(plot.background = element_rect(fill = "#F4F6F6"), legend.position = 'none', 
                           legend.background = element_rect(fill = "#F4F6F6"), plot.title = element_text(face='bold')) +
                             guides(color=guide_legend(title = 'Dose')) + scale_fill_manual(values = c('#C0392B','#2471A3')) + 
                            annotate('rect', xmin = .4, xmax =3, ymin = -.25, ymax =.25, alpha = 1)

PANAS_Pos_BW <- ggarrange(PANAS_Joviality_BW, PANAS_Attentiveness_BW, ncol=1, nrow=2, heights = c(1.3,1)) 
PANAS_Pos_BW <-  annotate_figure(PANAS_Pos_BW,
                              top = text_grob("PANAS Positive Emotive States",
                                              color = 'black', face = 'bold', size = 13))



####Fatigue####
PANAS_Fatigue_BW <- ggplot(data = D_hl, aes(x=Dose, y= PANAS_Fatigue_Difference, fill = Dose)) + geom_boxplot(alpha = .75) +
                    xlab('Dose') + ylab('Difference Scores')  + labs(title= 'C) Fatigue')+
                    theme(plot.background = element_rect(fill = "#F4F6F6"), legend.position = 'none', 
                    legend.background = element_rect(fill = "#F4F6F6"), plot.title = element_text(face='bold')) +
                    guides(color=guide_legend(title = 'Dose')) + scale_fill_manual(values = c('#C0392B','#2471A3')) +
                      annotate('rect', xmin = .4, xmax =3, ymin = -.3, ymax =.3, alpha = 1)
  
PANAS_Fatigue_BW <- annotate_figure(PANAS_Fatigue_BW,
                  top = text_grob("PANAS Fatigue",
                             color = 'black', face = 'bold', size = 13))


####Serenity###
PANAS_Serenity_BW <- ggplot(data = D_hl, aes(x=Dose, y= PANAS_Serenity_Difference, fill = Dose)) + geom_boxplot(alpha = .75) +
                    xlab('Dose') + ylab('Difference Scores')  + labs(title= 'D) Serenity             p<0.05 *') +
                     theme(plot.background = element_rect(fill = "#F4F6F6"), legend.position = 'none', 
                           legend.background = element_rect(fill = "#F4F6F6"), plot.title = element_text(face='bold')) + 
                    guides(color=guide_legend(title = 'Dose')) + scale_fill_manual(values = c('#C0392B','#2471A3')) +
                    annotate("text", x =1.5, y= 75, label = "*" , size = 8, color= 'black') +
                    annotate('rect', xmin = 1.15, xmax = 1.85, ymin = 74.5, ymax =75, alpha = 1) + 
                    annotate('rect', xmin = 1.14, xmax = 1.15, ymin = 70, ymax =75, alpha = 1) +
                    annotate('rect', xmin = 1.84, xmax = 1.85, ymin = 70, ymax =75, alpha = 1) + 
                     annotate('rect', xmin = .4, xmax =3, ymin = -.4, ymax =.4, alpha = 1)

PANAS_Serenity_BW <- annotate_figure(PANAS_Serenity_BW,
                                 top = text_grob("PANAS Serenity",
                                                 color = 'black', face = 'bold', size = 13))
###Arrange
PANAS_BW <- ggarrange(PANAS_Neg_BW, PANAS_Pos_BW, PANAS_Fatigue_BW, PANAS_Serenity_BW, ncol=2, nrow=2,
                    labels = c('A','B', 'C', 'D'), heights=(c(2,1)))

PANAS_BW <- annotate_figure(PANAS_BW,
              top = text_grob("Acute Mood Effects of Propofol Averaged Across Visits",
                                  color = 'black', face = c('bold'), size = 16)) + 
              theme(plot.background = element_rect(fill = "#D6EAF8"))

##Print
print(PANAS_BW)


