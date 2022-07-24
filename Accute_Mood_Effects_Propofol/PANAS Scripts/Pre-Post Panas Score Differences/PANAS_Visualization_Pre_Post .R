#PANAS
#visualization
library(ggplot2)
library (ggpubr)
library(corrplot)

##quick correlation
cp <- corrplot(cor.data)

PANAS_line <- ggline(D_results_randomized_only, x= 'Visit_Number', y= 'PANAS_Negative',
                    color = 'Dose_Time', palette = 'jco', add = 'mean') +
  labs (title = 'Negative PANAS Rattings Pre-Post Treatment Scores By Dose') 


##Print
print(PANAS_line)
#print(cp)