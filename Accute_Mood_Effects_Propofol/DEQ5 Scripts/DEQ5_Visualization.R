#PANAS
#visualization
library(ggplot2)
library (ggpubr)

##graphing

DEQ5_line <- ggline(D2, x= 'TX_Number', y= 'High',
                    color = 'Dose_Time', palette = 'jco', add = 'mean_se') +
  labs (title = 'DEQ5 High Post-Treatment Scores By Dose and Treatment Number') 


##Print
print(DEQ5_line)
