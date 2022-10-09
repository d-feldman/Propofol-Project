###HDRS
##Libraries##
#manipulating data
library(dplyr)
library(tidyr)



###SETUP###
##Set working Directory and load CSV##
setwd('/Users/danielfeldman/Desktop/EEG PANAS/')

D_raw <- read.csv('HDRS_Open.csv', header=T)

##expand randomization##
D_cleaned <- D_raw 

D_cleaned$Dose <- NA
D_cleaned$Dose <- 'Open_Label'

###change colnames
colnames(D_cleaned) <- c('Subject', 'Visit','HDRS_6','HDRS_17','HDRS_24', 'Dose')

###change dose names


###change visit names
D_cleaned <- D_cleaned %>% mutate(Visit = case_when(
                                                   Visit == 'w2_arm_1' ~ 'Week 2',
                                                   Visit == 'w4_arm_1' ~ 'Week 4'))

D_cleaned <- drop_na(D_cleaned)


D_2nd <- D_cleaned %>% filter(Visit == 'Week 2')
D_4th <- D_cleaned %>% filter(Visit == 'Week 4')


###pre-post-HDRS
D_HDRS_PP_Open <- D_2nd %>% select('Subject','Visit','Dose')

D_HDRS_PP_Open$HDRS_Diff <- D_4th$HDRS_24 - D_2nd$HDRS_24

D_HDRS_PP_Open$Patient <- c(28,38,40,47,48)





###HDRS data cleaned for open label patients weeks 2-4