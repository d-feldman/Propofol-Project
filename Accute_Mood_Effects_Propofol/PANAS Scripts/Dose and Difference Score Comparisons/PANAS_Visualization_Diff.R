#PANAS
#visualization
library(ggplot2)
library (ggpubr)
library(corrplot)

##quick correlation
cp <- corrplot(cor.data)

PANAS_line <- ggline(D2, x= 'Visit_Number', y= 'PANAS_Serenity_Difference',
                    color = 'Dose_Time', palette = 'jco', add = 'mean_se') +
  labs (title = 'Fatigue PANAS Rattings Pre-Post Treatment Difference Scores') 


##Print
print(PANAS_line)
#print(cp)