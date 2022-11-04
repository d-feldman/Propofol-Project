###Visualizing things
library(ggplot2)
library(ggpubr)
#manipulating data
library(dplyr)
library(tidyr)

### 1st visit Pre-post paired
paired<- ggpaired(D_results_randomized, x = 'Time', y = 'PANAS_Sadness', color = 'Time',
                  line.color = 'Gray', line.size = .4, 
                  xlab = 'Time', ylab = 'PANAS Sadness Score', 
                  title = '                                        PANAS Sadness Pre-Post Propofol Administration', legend = 'none', 
                  palette = c('#C7102B', '#1D6FF2'))  +
  annotate("text", x =1, y= 105, label = "                                                                                                p<.001 ***" , size = 4, color= 'black') +
  annotate('rect', xmin = 1, xmax = 2, ymin = 103, ymax =103.2, alpha = 1) + 
  annotate('rect', xmin = 1, xmax = 1.003, ymin =100 , ymax =103, alpha = 1) +
  annotate('rect', xmin = 1.998, xmax = 2, ymin = 100, ymax =103, alpha = 1) 


print(paired)
