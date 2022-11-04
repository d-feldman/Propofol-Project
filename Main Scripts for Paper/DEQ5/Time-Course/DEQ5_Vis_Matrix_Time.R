#PANAS
#visualization
library(ggplot2)
library (ggpubr)
library(lme4)

##graphing

DEQ5_Drug <- ggline(D2, x= 'TX_Number', y= 'Drug_Effect',
                    color = 'Group', palette = c('#3263a8', '#a8323e'), add = 'mean_se') +
  labs (title = "DEQ-5 'Drug Effect' Rating") + xlab ('Treatment Number') +
  ylab('Mean Drug Effect Rating')
DEQ5_Like <- ggline(D2, x= 'TX_Number', y= 'Like',
                    color = 'Group', palette = c('#3263a8', '#a8323e'), add = 'mean_se') +
  labs (title = "DEQ-5 'Like' Rating") + xlab ('Treatment Number') + ylab('Mean Like Rating')
DEQ5_Dislike <- ggline(D2, x= 'TX_Number', y= 'Dislike',
                    color = 'Group', palette = c('#3263a8', '#a8323e'), add = 'mean_se') +
  labs (title = "DEQ-5 'Dislike' Rating") + xlab ('Treatment Number') + theme(legend.position = 'none') + ylab('Mean Dislike Rating')
DEQ5_High <- ggline(D2, x= 'TX_Number', y= 'High',
                    color = 'Group', palette = c('#3263a8', '#a8323e'), add = 'mean_se') +
  labs (title = "DEQ-5 'High' Rating") + xlab ('Treatment Number') + theme(legend.position = 'none') + ylab('Mean High Rating')
DEQ5_Want <- ggline(D2, x= 'TX_Number', y= 'Want_More',
                    color = 'Group', palette = c('#3263a8', '#a8323e'), add = 'mean_se') +
  labs (title = "DEQ-5 'Want More' Rating") + xlab ('Treatment Number') +
  ylab('Mean Want More Rating') + theme(legend.position = 'none')


figure <- ggarrange(DEQ5_Drug, DEQ5_Like, DEQ5_Dislike, DEQ5_High, DEQ5_Want, 
                    ncol=2, nrow=3,
                              labels = c('a','b', 'c', 'd', 'e')) 


annotated_figure <- annotate_figure(figure,
    top = text_grob("DEQ5 Post-Treatment Scores Over Propofol Treatment Course",
                    color = 'black', face = 'bold', size = 14)) + theme_bw()




print(annotated_figure)
