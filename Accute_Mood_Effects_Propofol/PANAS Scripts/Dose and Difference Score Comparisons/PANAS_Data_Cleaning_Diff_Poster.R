#PANAS
##Libraries##
#manipulating data
library(dplyr)
library(tidyr)



###SETUP###
##Set working Directory and load CSV##
setwd('/Users/danielfeldman/Desktop/Working//')

D <- read.csv('NAP_Example_Data.csv', header=T)

##Make Dataframe contain only treatment dates
D <- D %>% filter( redcap_event_name == 'baseline_arm_1' | redcap_event_name == 't1_arm_1' | 
                      redcap_event_name == 't2_arm_1' | redcap_event_name == 't3_arm_1'|
                      redcap_event_name == 't4_arm_1' | 
                      redcap_event_name == 't5_arm_1' | redcap_event_name == 't6_arm_1' |
                      redcap_event_name == 't7_arm_1'| redcap_event_name == 't8_arm_1'| 
                      redcap_event_name == 't9_arm_1'| redcap_event_name == 't10_arm_1'|
                      redcap_event_name == 't11_arm_1'| redcap_event_name == 't12_arm_1')

#expand randomization to other visits
D <- D %>% group_by(subject_id) %>% fill(randomization) %>% group_by(randomization)


###SCORING###
# calculate the 8 available subscales, normalized to range 0-1
# pre-infusion (1), post-infusion (2), and change (d)
D1 <- data.frame(D$subject_id,D$redcap_event_name,D$randomization)

D1$panas_Negative_Pre <- ( D$panas_irritable_1 + D$panas_afraid_1 + D$panas_upset_1 + D$panas_guilty_1 + 
                          D$panas_nervous_1 + D$panas_hostile_1 + D$panas_jittery_1 + D$panas_ashamed_1 + 
                          D$panas_scared_1 + D$panas_distressed_1 - 10 ) / 40
D1$panas_Negative_Post <- ( D$panas_irritable_2 + D$panas_afraid_2 + D$panas_upset_2 + D$panas_guilty_2 + 
                          D$panas_nervous_2 + D$panas_hostile_2 + D$panas_jittery_2 + D$panas_ashamed_2 + 
                          D$panas_scared_2 + D$panas_distressed_2 - 10 ) / 40
D1$panas_Negative_Difference <- (D1$panas_Negative_Post - D1$panas_Negative_Pre) * 100


D1$panas_Positive_Pre <- ( D$panas_attentive_1 + D$panas_strong_1 + D$panas_inspired_1 + D$panas_alert_1 + 
                          D$panas_active_1 + D$panas_excited_1 + D$panas_proud_1 + D$panas_enthusiastic_1 + 
                          D$panas_determined_1 + D$panas_interested_1 - 10 ) / 40
D1$panas_Positive_Post <- ( D$panas_attentive_2 + D$panas_strong_2 + D$panas_inspired_2 + D$panas_alert_2 + 
                          D$panas_active_2 + D$panas_excited_2 + D$panas_proud_2 + D$panas_enthusiastic_2 + 
                          D$panas_determined_2 + D$panas_interested_2 - 10 ) / 40
D1$panas_Positive_Difference <- (D1$panas_Positive_Post - D1$panas_Positive_Pre) * 100


D1$panas_Fear_Pre <- ( D$panas_afraid_1 + D$panas_shaky_1 + D$panas_nervous_1 + D$panas_jittery_1 + 
                      D$panas_scared_1 + D$panas_frightened_1 - 6 ) / 24
D1$panas_Fear_Post <- ( D$panas_afraid_2 + D$panas_shaky_2 + D$panas_nervous_2 + D$panas_jittery_2 +
                      D$panas_scared_2 + D$panas_frightened_2 - 6 ) / 24
D1$panas_Fear_Difference <- (D1$panas_Fear_Post - D1$panas_Fear_Pre) * 100


D1$panas_Sadness_Pre <- ( D$panas_sad_1 + D$panas_alone_1 + D$panas_blue_1 + D$panas_lonely_1 + 
                         D$panas_downhearted_1 - 5 ) / 20
D1$panas_Sadness_Post <- ( D$panas_sad_2 + D$panas_alone_2 + D$panas_blue_2 + D$panas_lonely_2 + 
                         D$panas_downhearted_2 - 5 ) / 20
D1$panas_Sadness_Difference <- (D1$panas_Sadness_Post - D1$panas_Sadness_Pre) * 100


D1$panas_Joviality_Pre <- ( D$panas_cheerful_1 + D$panas_delighted_1 + D$panas_happy_1 + D$panas_joyful_1 + 
                           D$panas_excited_1 + D$panas_lively_1 + D$panas_enthusiastic_1 + D$panas_energetic_1 - 8 ) / 32
D1$panas_Joviality_Post <- ( D$panas_cheerful_2 + D$panas_delighted_2 + D$panas_happy_2 + D$panas_joyful_2 + 
                           D$panas_excited_2 + D$panas_lively_2 + D$panas_enthusiastic_2 + D$panas_energetic_2 - 8 ) / 32
D1$panas_Joviality_Difference <- (D1$panas_Joviality_Post - D1$panas_Joviality_Pre) * 100


D1$panas_Attentiveness_Pre <- ( D$panas_attentive_1 + D$panas_alert_1 + D$panas_determined_1 + 
                               D$panas_concentrating_1 - 4 ) / 16
D1$panas_Attentiveness_Post <- ( D$panas_attentive_2 + D$panas_alert_2 + D$panas_determined_2 + 
                               D$panas_concentrating_2 - 4 ) / 16
D1$panas_Attentiveness_Difference <- (D1$panas_Attentiveness_Post - D1$panas_Attentiveness_Pre) * 100


D1$panas_Fatigue_Pre <- ( D$panas_sluggish_1 + D$panas_tired_1 + D$panas_sleepy_1 + D$panas_drowsy_1 - 4 ) / 16
D1$panas_Fatigue_Post <- ( D$panas_sluggish_2 + D$panas_tired_2 + D$panas_sleepy_2 + D$panas_drowsy_2 - 4 ) / 16
D1$panas_Fatigue_Difference <- (D1$panas_Fatigue_Post - D1$panas_Fatigue_Pre) * 100


D1$panas_Serenity_Pre <- ( D$panas_relaxed_1 + D$panas_calm_1 + D$panas_at_ease_1 - 3 ) / 12
D1$panas_Serenity_Post <- ( D$panas_relaxed_2 + D$panas_calm_2 + D$panas_at_ease_2 - 3 ) / 12
D1$panas_Serenity_Difference <- (D1$panas_Serenity_Post - D1$panas_Serenity_Pre) * 100


###CLEAN_DATA###
#Drop null values
D1 <- drop_na(D1)

#change randomization name
D1 <- D1 %>% mutate(D.randomization = replace(D.randomization, D.randomization == 1, 'High')) %>%
  mutate(D.randomization= replace(D.randomization, D.randomization == 0, 'Low'))

###results table##
##Make differences dataframe
D2 <- D1 %>% select(D.subject_id, D.redcap_event_name, D.randomization, panas_Negative_Difference, panas_Positive_Difference,
                    panas_Fear_Difference, panas_Sadness_Difference, panas_Joviality_Difference, panas_Attentiveness_Difference,
                    panas_Fatigue_Difference, panas_Serenity_Difference)

###Rename Columns to Make Pretty##
colnames(D2)<- c('subject_id', 'Visit', 'Group', 'PANAS_Negative_Difference', 'PANAS_Positive_Difference', 
                 'PANAS_Fear_Difference', 'PANAS_Sadness_Difference', 'PANAS_Joviality_Difference',
                 'PANAS_Attentiveness_Difference','PANAS_Fatigue_Difference','PANAS_Serenity_Difference')

##Now Make into 3 groups of randomization##
D2$Dose_Time <- NA
D2 <- D2 %>% relocate(Dose_Time, .before= PANAS_Negative_Difference)

### conditionally fill for initial high dose vs high after low
D2 <- D2 %>% mutate(Dose_Time = case_when(Group == 'High' & Visit == 't1_arm_1' ~ 'Initial_High_Dose', 
                                               Group == 'Low'  & Visit == 't1_arm_1' ~ 'Initial_Low_Dose', 
                                          Group == 'Low'  & Visit == 't7_arm_1' ~ 'Open_Label_High_Dose',
                                          Group == 'High' & Visit == 't7_arm_1' ~ 'Open_Label_Low_Dose'))
#Expand Labels to other visits
D2 <- D2 %>% group_by(subject_id) %>% fill(Dose_Time) %>% group_by(Dose_Time)

#Drop Open Label Low Dose
D2 <- subset(D2, Dose_Time != 'Open_Label_Low_Dose')

#Delete old Group Column
D2 <- select(D2, -Group)



###Create column for visit number in sequence###

#First, Make Visit a character string
D2$Visit <- as.character(D2$Visit)


#New Column
D2$Visit_Number <- NA
D2 <- D2 %>% relocate(Visit_Number, .after = Visit)

###for open label
D3 <- D2

##Add in numbers, making visits 7-12 same as 1-6 for time series alignment in D2
D2 <- D2 %>% mutate(Visit_Number = case_when(Visit == 't1_arm_1'~ 1,
  Visit == 't2_arm_1'~ 2, Visit == 't3_arm_1' ~ 3,
  Visit == 't4_arm_1'~ 4, Visit == 't5_arm_1' ~ 5, Visit == 't6_arm_1'~ 6, 
  Visit == 't7_arm_1'~ 1, Visit == 't8_arm_1'~ 2, Visit == 't9_arm_1' ~ 3,
  Visit == 't10_arm_1'~ 4, Visit == 't11_arm_1' ~ 5, Visit == 't12_arm_1'~ 6)) 

###just change names for D3
D3 <- D3 %>% mutate(Visit_Number = case_when(Visit == 't1_arm_1'~ 1,
                                             Visit == 't2_arm_1'~ 2, Visit == 't3_arm_1' ~ 3,
                                             Visit == 't4_arm_1'~ 4, Visit == 't5_arm_1' ~ 5, Visit == 't6_arm_1'~ 6, 
                                             Visit == 't7_arm_1'~ 7, Visit == 't8_arm_1'~ 8, Visit == 't9_arm_1' ~ 9,
                                             Visit == 't10_arm_1'~ 10, Visit == 't11_arm_1' ~ 11, Visit == 't12_arm_1'~ 12)) 


##high and low doses
D_hl <- subset(D2, Dose_Time != 'Open_Label_High_Dose')
D_hl <- D_hl %>% mutate(Dose_Time = case_when( Dose_Time == 'Initial_High_Dose' ~ 'High Dose', 
                                              Dose_Time == 'Initial_Low_Dose' ~ 'Low Dose'))
D_hl$Dose_Time <- as.factor(D_hl$Dose_Time)
D_hl$Visit_Number <- as.factor(D_hl$Visit_Number)

D_hl$Dose <- D_hl$Dose_Time

###Open label for stats
D_ol <- subset(D2, Dose_Time != 'Initial_High_Dose')
D_ol <- D_ol %>% mutate(Dose_Time = case_when( Dose_Time == 'Open_Label_High_Dose' ~ 'Open Label', 
                                               Dose_Time == 'Initial_Low_Dose' ~ 'Low Dose'))
D_ol$Dose_Time <- as.factor(D_ol$Dose_Time)
D_ol$Visit_Number <- as.factor(D_ol$Visit_Number)

D_ol$Dose <- D_ol$Dose_Time
###open label long for graphing
D_OL_long <- subset(D3, Dose_Time != 'Initial_High_Dose')
D_OL_long <- D_OL_long %>% mutate(Dose_Time = case_when( Dose_Time == 'Open_Label_High_Dose' ~ 'Open Label', 
                                               Dose_Time == 'Initial_Low_Dose' ~ 'Low Dose'))

D_OL_long$Dose_Time <- as.factor(D_OL_long$Dose_Time)
D_OL_long$Visit_Number <- as.factor(D_OL_long$Visit_Number)

D_OL_long$Dose <- D_OL_long$Dose_Time

                                        
##Cleaning Done
##Ready for analysis
