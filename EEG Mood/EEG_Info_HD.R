#PANAS
##Libraries##
#manipulating data
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)




###SETUP###
##Set working Directory and load CSV##
setwd('/Users/danielfeldman/Desktop/EEG PANAS//')

D_EEG <- read.csv('Summary_Table_EEG.csv', header=T)

D_EEG <- drop_na(D_EEG)

#D_EEG_PANAS_1 <- merge(D_high_dose_1, D_EEG, by = 'Patient')

#D_EEG_PANAS_16 <- merge(D_EEG,D_change_PANAS, by = 'Patient')

D_EEG_HDRS_PP <- merge(D_EEG,D_HDRS_PP_High, by = 'Patient')

###HDRSHigh and Open Label
D_EEG_HDRS_All_High <- bind_rows(D_HDRS_PP_High,D_HDRS_PP_Open)
D_EEG_HDRS_All_High<- select(D_EEG_HDRS_All_High, -Visit)

D_EEG_HDRS_All_High <- merge(D_EEG_HDRS_All_High, D_EEG,  by = 'Patient')
###Visualize
# 
# EEG_vis <- ggplot(data = D_EEG_HDRS_PP, aes(x =D_EEG_HDRS_PP$Mean.Spindle.Peak.to.Peak.Amplitude..uV. , y = HDRS_Diff))
# 
# EEG_vis_1 <- EEG_vis + geom_point() + geom_smooth(method = 'lm') + stat_cor()
# 
# 
# EEG_HDRS_lm <- lm(data = D_EEG_HDRS_PP, Mean.Spindle.Peak.to.Peak.Amplitude..uV.~ HDRS_Diff)
# 
# print(EEG_vis_1)



EEG_vis2 <- ggplot(data = D_EEG_HDRS_All_High, aes(x =Mean.Spindle.Peak.to.Peak.Amplitude..uV. , y = HDRS_Diff))

EEG_vis_3 <- EEG_vis2 + geom_point() + geom_smooth(method = 'lm') + stat_cor()


EEG_HDRS_lm_2 <- lm(data = D_EEG_HDRS_All_High, Mean.Spindle.Peak.to.Peak.Amplitude..uV.~ HDRS_Diff)

print(EEG_vis_3)
print(summary(EEG_HDRS_lm_2))