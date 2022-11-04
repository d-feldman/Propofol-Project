###Visualization and statsfor pre-post time course
library(ggplot2)
library (ggpubr)


###Sadness
sad_line <- ggline(D_results, x= 'Visit_Number', y= 'PANAS_Sadness',
                     color = 'Time', palette = c('#EFC000','#0073C2'), add = c('mean_se')) + 
xlab('Treatment Number') + ylab('Mean Sadness Rating') + theme(legend.position= 'top')

sad_line <- annotate_figure(sad_line,
                                    top = text_grob("PANAS Sadness Ratings Over Time",
                                                    color = 'black', face = 'bold', size = 12))

###Fear
fear_line <- ggline(D_results, x= 'Visit_Number', y= 'PANAS_Fear',
                   color = 'Time', palette = c('#EFC000','#0073C2'), add = c('mean_se')) + 
  xlab('Treatment Number') + ylab('Mean Fear Rating') + theme(legend.position= 'top')

fear_line <- annotate_figure(fear_line,
                            top = text_grob("PANAS Fear Ratings Over Time",
                                            color = 'black', face = 'bold', size = 12))


###Joviality
jov_line <- ggline(D_results, x= 'Visit_Number', y= 'PANAS_Joviality',
                   color = 'Time', palette = c('#EFC000','#0073C2'), add = c('mean_se')) + 
  xlab('Treatment Number') + ylab('Mean Joviality Rating') + theme(legend.position= 'none')

jov_line <- annotate_figure(jov_line,
                            top = text_grob("PANAS Joviality Ratings Over Time",
                                            color = 'black', face = 'bold', size = 12))


###Attentiveness
att_line <- ggline(D_results, x= 'Visit_Number', y= 'PANAS_Attentiveness',
                   color = 'Time', palette = c('#EFC000','#0073C2'), add = c('mean_se')) + 
  xlab('Treatment Number') + ylab('Mean Attentiveness Rating') + theme(legend.position= 'none')

att_line <- annotate_figure(att_line,
                            top = text_grob("PANAS Attentiveness Ratings Over Time",
                                            color = 'black', face = 'bold', size = 12))


###Fatigue
fat_line <- ggline(D_results, x= 'Visit_Number', y= 'PANAS_Fatigue',
                   color = 'Time', palette = c('#EFC000','#0073C2'), add = c('mean_se')) + 
  xlab('Treatment Number') + ylab('Mean Fatigue Rating') + theme(legend.position= 'none')

fat_line <- annotate_figure(fat_line,
                            top = text_grob("PANAS Fatigue Ratings Over Time",
                                            color = 'black', face = 'bold', size = 12))


###Serenity
ser_line <- ggline(D_results, x= 'Visit_Number', y= 'PANAS_Serenity',
                   color = 'Time', palette = c('#EFC000','#0073C2'), add = c('mean_se')) + 
  xlab('Treatment Number') + ylab('Mean Serenity Rating') + theme(legend.position= 'none')

ser_line <- annotate_figure(ser_line,
                            top = text_grob("PANAS Serenity Ratings Over Time",
                                            color = 'black', face = 'bold', size = 12))


###Sadness
sad_line <- ggline(D_results, x= 'Visit_Number', y= 'PANAS_Sadness',
                   color = 'Time', palette = c('#EFC000','#0073C2'), add = c('mean_se')) + 
  xlab('Treatment Number') + ylab('Mean Sadness Rating') + theme(legend.position= 'top')

sad_line <- annotate_figure(sad_line,
                            top = text_grob("PANAS Sadness Ratings Over Time",
                                            color = 'black', face = 'bold', size = 12))




###Arrange
PANAS_line_comb <- ggarrange(sad_line, fear_line, jov_line, att_line, fat_line, ser_line,
                            ncol=2, nrow=3,
                            labels = c('a','b', 'c', 'd', 'e', 'f'))

PANAS_line_comb <- annotate_figure(PANAS_line_comb,
                                  top = text_grob("Short-Term Mood Effects of Propofol Anesthesia over Treatment Course",
                                                  color = 'black', face = c('bold'), size = 16)) + theme_bw()

##Print
print(PANAS_line_comb)


