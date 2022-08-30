#PANAS Spaghetti 
#visualization
library(ggplot2)


#####High Vs Low Dose Randomized#####

PANAS_Spag <- ggplot(data=D_hl, aes(x=Visit_Number, y= PANAS_Sadness_Difference,
                                    group = subject_id)) + geom_line(aes(color=Dose_Time))

print(PANAS_Spag)