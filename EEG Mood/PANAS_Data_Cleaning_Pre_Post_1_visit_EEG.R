#PANAS
##Libraries##
#manipulating data
library(dplyr)
library(tidyr)


###SETUP###
##Set working Directory and load CSV##
setwd('/Users/danielfeldman/Desktop/EEG PANAS/')

D_raw <- read.csv('NAP_Example_Data.csv', header=T)


##Make Dataframe contain only treatment dates
D <- D_raw %>% filter( redcap_event_name == 'baseline_arm_1' | 
                         redcap_event_name == 't1_arm_1' )

#expand randomization to other visits
D <- D %>% group_by(subject_id) %>% fill(randomization) %>% group_by(randomization)


###SCORING###
# calculate the 8 available subscales, normalized to range 0-1
# pre-infusion (1), post-infusion (2), and change (d)
D1 <- data.frame(D$subject_id,D$redcap_event_name,D$randomization)

D1$panas_Negative_Pre <- ( D$panas_irritable_1 + D$panas_afraid_1 + D$panas_upset_1 + D$panas_guilty_1 + 
                          D$panas_nervous_1 + D$panas_hostile_1 + D$panas_jittery_1 + D$panas_ashamed_1 + 
                          D$panas_scared_1 + D$panas_distressed_1 - 10 ) / 40 *100
D1$panas_Negative_Post <- ( D$panas_irritable_2 + D$panas_afraid_2 + D$panas_upset_2 + D$panas_guilty_2 + 
                          D$panas_nervous_2 + D$panas_hostile_2 + D$panas_jittery_2 + D$panas_ashamed_2 + 
                          D$panas_scared_2 + D$panas_distressed_2 - 10 ) / 40 * 100


D1$panas_Positive_Pre <- ( D$panas_attentive_1 + D$panas_strong_1 + D$panas_inspired_1 + D$panas_alert_1 + 
                          D$panas_active_1 + D$panas_excited_1 + D$panas_proud_1 + D$panas_enthusiastic_1 + 
                          D$panas_determined_1 + D$panas_interested_1 - 10 ) / 40 *100
D1$panas_Positive_Post <- ( D$panas_attentive_2 + D$panas_strong_2 + D$panas_inspired_2 + D$panas_alert_2 + 
                          D$panas_active_2 + D$panas_excited_2 + D$panas_proud_2 + D$panas_enthusiastic_2 + 
                          D$panas_determined_2 + D$panas_interested_2 - 10 ) / 40 *100 


D1$panas_Fear_Pre <- ( D$panas_afraid_1 + D$panas_shaky_1 + D$panas_nervous_1 + D$panas_jittery_1 + 
                      D$panas_scared_1 + D$panas_frightened_1 - 6 ) / 24 * 100
D1$panas_Fear_Post <- ( D$panas_afraid_2 + D$panas_shaky_2 + D$panas_nervous_2 + D$panas_jittery_2 +
                      D$panas_scared_2 + D$panas_frightened_2 - 6 ) / 24 * 100


D1$panas_Sadness_Pre <- ( D$panas_sad_1 + D$panas_alone_1 + D$panas_blue_1 + D$panas_lonely_1 + 
                         D$panas_downhearted_1 - 5 ) / 20 * 100
D1$panas_Sadness_Post <- ( D$panas_sad_2 + D$panas_alone_2 + D$panas_blue_2 + D$panas_lonely_2 + 
                         D$panas_downhearted_2 - 5 ) / 20 * 100


D1$panas_Joviality_Pre <- ( D$panas_cheerful_1 + D$panas_delighted_1 + D$panas_happy_1 + D$panas_joyful_1 + 
                           D$panas_excited_1 + D$panas_lively_1 + D$panas_enthusiastic_1 + D$panas_energetic_1 - 8 ) / 32 * 100
D1$panas_Joviality_Post <- ( D$panas_cheerful_2 + D$panas_delighted_2 + D$panas_happy_2 + D$panas_joyful_2 + 
                           D$panas_excited_2 + D$panas_lively_2 + D$panas_enthusiastic_2 + D$panas_energetic_2 - 8 ) / 32 * 100


D1$panas_Attentiveness_Pre <- ( D$panas_attentive_1 + D$panas_alert_1 + D$panas_determined_1 + 
                               D$panas_concentrating_1 - 4 ) / 16 * 100
D1$panas_Attentiveness_Post <- ( D$panas_attentive_2 + D$panas_alert_2 + D$panas_determined_2 + 
                               D$panas_concentrating_2 - 4 ) / 16 * 100


D1$panas_Fatigue_Pre <- ( D$panas_sluggish_1 + D$panas_tired_1 + D$panas_sleepy_1 + D$panas_drowsy_1 - 4 ) / 16 * 100
D1$panas_Fatigue_Post <- ( D$panas_sluggish_2 + D$panas_tired_2 + D$panas_sleepy_2 + D$panas_drowsy_2 - 4 ) / 16 * 100


D1$panas_Serenity_Pre <- ( D$panas_relaxed_1 + D$panas_calm_1 + D$panas_at_ease_1 - 3 ) / 12 * 100
D1$panas_Serenity_Post <- ( D$panas_relaxed_2 + D$panas_calm_2 + D$panas_at_ease_2 - 3 ) / 12 * 100


###CLEAN_DATA###
#Drop null values
D1 <- drop_na(D1)

#change randomization name
D1 <- D1 %>% mutate(D.randomization = replace(D.randomization, D.randomization == 1, 'High')) %>%
  mutate(D.randomization= replace(D.randomization, D.randomization == 0, 'Low'))

###results table##
##Make multiple dataframe foir each cond.
D_pre <- D1 %>% select(D.subject_id, D.redcap_event_name, D.randomization, panas_Negative_Pre, panas_Positive_Pre,
                        panas_Fear_Pre, panas_Sadness_Pre, panas_Joviality_Pre, panas_Attentiveness_Pre,
                        panas_Fatigue_Pre, panas_Serenity_Pre)
D_post <- D1 %>% select(D.subject_id, D.redcap_event_name, D.randomization, panas_Negative_Post, panas_Positive_Post,
                        panas_Fear_Post, panas_Sadness_Post, panas_Joviality_Post, panas_Attentiveness_Post,
                        panas_Fatigue_Post, panas_Serenity_Post)


##Add in labels for pre, post, diff
D_pre$Time <- 'Pre'
D_pre <- D_pre %>% relocate(Time, .before= panas_Negative_Pre)

D_post$Time <- 'Post'
D_post <- D_post %>% relocate(Time, .before= panas_Negative_Post)

###Rename Columns to allow rbind#
colnames(D_pre)<- c('subject_id', 'Visit', 'Group', 'Time', 'PANAS_Negative', 'PANAS_Positive', 
                        'PANAS_Fear', 'PANAS_Sadness', 'PANAS_Joviality',
                        'PANAS_Attentiveness','PANAS_Fatigue','PANAS_Serenity')

colnames(D_post)<- c('subject_id', 'Visit', 'Group', 'Time', 'PANAS_Negative', 'PANAS_Positive', 
                        'PANAS_Fear', 'PANAS_Sadness', 'PANAS_Joviality',
                        'PANAS_Attentiveness','PANAS_Fatigue','PANAS_Serenity')

###results combined dataframe
D_results <- rbind(D_pre, D_post)


##Now Make into 3 groups of randomization##
D_results$Dose_Time <- NA
D_results <- D_results %>% relocate(Dose_Time, .before= PANAS_Negative)

### conditionally fill for initial high dose vs high after low
D_results <- D_results %>% mutate(Dose_Time = case_when(Group == 'High' & Time == 'Pre' & Visit == 't1_arm_1' ~ 'High_Dose_Pre', 
                                               Group == 'Low'  & Time == 'Pre' & Visit == 't1_arm_1' ~ 'Low_Dose_Pre', 
                                          Group == 'High' & Time == 'Post' & Visit == 't1_arm_1' ~ 'High_Dose_Post',
                                          Group == 'Low' & Time == 'Post' & Visit == 't1_arm_1' ~ 'Low_Dose_Post',
                                          Group == 'Low'  & Time == 'Pre' & Visit == 't7_arm_1' ~ 'Open_Label_High_Dose_Pre',
                                          Group == 'Low' & Time == 'Post' & Visit == 't7_arm_1' ~ 'Open_Label_High_Dose_Post',
                                          Group == 'High' & Visit == 't7_arm_1' ~ 'Open_Label_Low_Dose',
                                          Group == 'High'  & Visit == 't7_arm_1' ~ 'Open_Label_Low_Dose'))
#Expand Labels to other visits
D_results <- D_results %>% group_by(subject_id) %>% fill(Dose_Time) %>% group_by(Dose_Time)

#Drop Open Label Low Dose
D_results <- subset(D_results, Dose_Time != 'Open_Label_Low_Dose')



##Just Pre and Post first 6 visits-- NO OPEN LABEL
D_results_randomized <- subset(D_results, Dose_Time != 'Open_Label_High_Dose_Pre')
D_results_randomized<- subset(D_results_randomized, Dose_Time != 'Open_Label_High_Dose_Post')

##low dose and open label
D_results_open <- subset(D_results, Dose_Time != 'High_Dose_Pre')
D_results_open <- subset(D_results_open, Dose_Time != 'High_Dose_Post')

##open-label and high dose
D_results_high <- subset(D_results, Dose_Time != 'Low_Dose_Pre')
D_results_high <- subset(D_results_high, Dose_Time != 'Low_Dose_Post')

## As Factor
D_results_randomized$Dose_Time <- as.factor(D_results_randomized$Dose_Time)
D_results_open$Dose_Time <- as.factor(D_results_open$Dose_Time)
D_results_high$Dose_Time <- as.factor(D_results_high$Dose_Time)


##Cleaning Done
##Ready for analysis
