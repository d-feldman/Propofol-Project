###HDRS
##Libraries##
#manipulating data
library(dplyr)
library(tidyr)



###SETUP###
##Set working Directory and load CSV##
setwd('/Users/danielfeldman/Desktop/PANAS_HDRS/')

D_raw <- read.csv('HDRS_23.csv', header=T)

##expand randomization##
D_cleaned <- D_raw %>% group_by(subject_id) %>% fill(randomization) %>% group_by(randomization)

###drop null
D_cleaned <- drop_na(D_cleaned)

###change colnames
colnames(D_cleaned) <- c('subject_id', 'Visit', 'Dose','HDRS_6','HDRS_17','HDRS_24')

###change dose names
D_cleaned <- D_cleaned %>% mutate(Dose = replace(Dose, Dose == 1, 'High')) %>%
  mutate(Dose= replace(Dose, Dose == 0, 'Low'))

###change visit names
D_cleaned <- D_cleaned %>% mutate(Visit = case_when(Visit == 'baseline_arm_1' ~ 'Baseline',
                                                   Visit == 'w1_arm_1' ~ 'Week 1',
                                                   Visit == 'w2_arm_1' ~ 'Week 2'))
###drop new NA
D_cleaned <- drop_na(D_cleaned)

##Set factors for analysis
D_cleaned$Dose <- as.factor(D_cleaned$Dose)
D_cleaned$Visit <- as.factor(D_cleaned$Visit)

###HDRS data cleaned for randomized patients, through 2 weeks.