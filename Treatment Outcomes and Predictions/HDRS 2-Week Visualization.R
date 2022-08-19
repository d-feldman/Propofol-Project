#HDRS Visualization
#visualization libraries
library(ggplot2)
library(ggpubr)


##box and whisker

HDRS_BW <- ggplot(data = D_cleaned, aes(x= Visit, y= HDRS_24, fill =Dose)) +
            geom_boxplot( alpha = .75) + ylab('HDRS 24 Score') + 
            scale_alpha_manual(values = c('#C0392B','#2471A3'))


###compressed line across dose
HDRS_line_no_Dose <- ggline(D_cleaned, x= 'Visit', y = 'HDRS_24', color = '#2980B9',
                             add = 'mean_se') + ylab('HDRS 24 Score')


###Line with Dose
HDRS_line_Dose <- ggline(D_cleaned, x= 'Visit', y = 'HDRS_24', color = 'Dose',
                            add = 'mean_se', palette = 'jco') + 
                            ylab('HDRS 24 Score')






###print results
print(HDRS_BW)
print(HDRS_line_no_Dose)
print(HDRS_line_Dose)