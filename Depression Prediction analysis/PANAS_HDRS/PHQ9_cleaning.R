##libraries
library(dplyr)
library(tidyr)


###read in csv

D_PHQ9_raw <- read.csv('/Users/danielfeldman/Desktop/PANAS_PHQ9/PHQ9.csv', header = TRUE)

D_PHQ9 <- D_PHQ9_raw %>% select('subject_id', 'redcap_event_name', 'phq9_score')

D_PHQ9 <- drop_na(D_PHQ9)

D_PHQ9_base <- D_PHQ9 %>% filter( redcap_event_name == 'baseline_arm_1')
D_PHQ9_w2 <- D_PHQ9 %>% filter(redcap_event_name == 'w2_arm_1')
D_PHQ9_w1 <- D_PHQ9 %>% filter(redcap_event_name == 'w1_arm_1')
D_PHQ9_w1 <- D_PHQ9_w1 %>% subset(subject_id != 'PROP29')

D_PHQ9_PP <- merge(x= D_PHQ9_base, y = D_PHQ9_w2, 
                   by.x = 'subject_id', by.y = 'subject_id')

D_PHQ9_PP <- D_PHQ9_PP %>% select('subject_id', 'phq9_score.x', 'phq9_score.y')

colnames(D_PHQ9_PP) <- c('Subject', 'Baseline_PHQ9', 'Week2_PHQ9')

D_PHQ9_PP$Week1_PHQ9 <- D_PHQ9_w1$phq9_score

D_PHQ9_PP$PHQ9_Diff <- D_PHQ9_PP$Week2_PHQ9 - D_PHQ9_PP$Baseline_PHQ9