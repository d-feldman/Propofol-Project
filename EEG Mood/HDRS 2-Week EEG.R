###HDRS
##Libraries##
#manipulating data
library(dplyr)
library(tidyr)



###SETUP###
##Set working Directory and load CSV##
setwd('/Users/danielfeldman/Desktop/EEG PANAS/')

D_raw <- read.csv('HDRS.csv', header=T)

##expand randomization##
D_cleaned <- D_raw %>% group_by(subject_id) %>% fill(randomization) %>% group_by(randomization)

###drop null
D_cleaned <- drop_na(D_cleaned)

###change colnames
colnames(D_cleaned) <- c('Subject', 'Visit', 'Dose','HDRS_6','HDRS_17','HDRS_24')

###change dose names
D_cleaned <- D_cleaned %>% mutate(Dose = replace(Dose, Dose == 1, 'High')) %>%
  mutate(Dose= replace(Dose, Dose == 0, 'Low'))

###change visit names
D_cleaned <- D_cleaned %>% mutate(Visit = case_when(Visit == 'baseline_arm_1' ~ 'Baseline',
                                                   Visit == 'w1_arm_1' ~ 'Week 1',
                                                   Visit == 'w2_arm_1' ~ 'Week 2'))

D_cleaned <- drop_na(D_cleaned)

D_B <- D_cleaned %>% filter(Visit == 'Baseline')
D_2nd <- D_cleaned %>% filter(Visit == 'Week 2')


###pre-post-HDRS
D_HDRS_PP <- D_B %>% select('Subject','Visit','Dose')

D_HDRS_PP$HDRS_Diff <- D_2nd$HDRS_24 - D_B$HDRS_24

D_HDRS_PP_High <- D_HDRS_PP %>% filter(Dose =='High')

D_HDRS_PP_High$Patient <- c(24,25,27,31,34,37,39,41,45,46,50)

##Set factors for analysis
D_cleaned$Dose <- as.factor(D_cleaned$Dose)
D_cleaned$Visit <- as.factor(D_cleaned$Visit)



###HDRS data cleaned for randomized patients, through 2 weeks.