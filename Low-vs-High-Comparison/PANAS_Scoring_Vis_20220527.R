#PANAS
##Libraries##
#manipulating data
library(dplyr)
library(tidyr)
#visualization
library(ggplot2)
library (ggpubr)
#statistics
library(rstatix)
library(emmeans)
library(sjstats)
library(lme4)
library(lmerTest)


###SETUP###
##Set working Directory and load CSV##
setwd('/Users/danielfeldman/Desktop/NAP_Example_Data')

D <- read.csv('NAPStudyCCTS4510_PANAS.csv', header=T)

##Make Dataframe only go through week 2
D <- D %>% filter( redcap_event_name == 'baseline_arm_1' | redcap_event_name == 't1_arm_1' |
                     redcap_event_name == 't2_arm_1' | redcap_event_name == 't3_arm_1' |
                      redcap_event_name == 't4_arm_1' | 
                     redcap_event_name == 't5_arm_1' | redcap_event_name == 't6_arm_1')

#expand randomization to other visits
D <- D %>% group_by(subject_id) %>% fill(randomization) %>% group_by(randomization)



###SCORING###
# calculate the 8 available subscales, normalized to range 0-1
# pre-infusion (1), post-infusion (2), and change (d)
D1 <- data.frame(D$subject_id,D$redcap_event_name,D$randomization)

D1$panas_Negative_1 <- ( D$panas_irritable_1 + D$panas_afraid_1 + D$panas_upset_1 + D$panas_guilty_1 + 
                          D$panas_nervous_1 + D$panas_hostile_1 + D$panas_jittery_1 + D$panas_ashamed_1 + 
                          D$panas_scared_1 + D$panas_distressed_1 - 10 ) / 40
D1$panas_Negative_2 <- ( D$panas_irritable_2 + D$panas_afraid_2 + D$panas_upset_2 + D$panas_guilty_2 + 
                          D$panas_nervous_2 + D$panas_hostile_2 + D$panas_jittery_2 + D$panas_ashamed_2 + 
                          D$panas_scared_2 + D$panas_distressed_2 - 10 ) / 40
D1$panas_Negative_d <- D1$panas_Negative_2 - D1$panas_Negative_1

D1$panas_Positive_1 <- ( D$panas_attentive_1 + D$panas_strong_1 + D$panas_inspired_1 + D$panas_alert_1 + 
                          D$panas_active_1 + D$panas_excited_1 + D$panas_proud_1 + D$panas_enthusiastic_1 + 
                          D$panas_determined_1 + D$panas_interested_1 - 10 ) / 40
D1$panas_Positive_2 <- ( D$panas_attentive_2 + D$panas_strong_2 + D$panas_inspired_2 + D$panas_alert_2 + 
                          D$panas_active_2 + D$panas_excited_2 + D$panas_proud_2 + D$panas_enthusiastic_2 + 
                          D$panas_determined_2 + D$panas_interested_2 - 10 ) / 40
D1$panas_Positive_d <- D1$panas_Positive_2 - D1$panas_Positive_1

D1$panas_Fear_1 <- ( D$panas_afraid_1 + D$panas_shaky_1 + D$panas_nervous_1 + D$panas_jittery_1 + 
                      D$panas_scared_1 + D$panas_frightened_1 - 6 ) / 24
D1$panas_Fear_2 <- ( D$panas_afraid_2 + D$panas_shaky_2 + D$panas_nervous_2 + D$panas_jittery_2 +
                      D$panas_scared_2 + D$panas_frightened_2 - 6 ) / 24
D1$panas_Fear_d <- D1$panas_Fear_2 - D1$panas_Fear_1

D1$panas_Sadness_1 <- ( D$panas_sad_1 + D$panas_alone_1 + D$panas_blue_1 + D$panas_lonely_1 + 
                         D$panas_downhearted_1 - 5 ) / 20
D1$panas_Sadness_2 <- ( D$panas_sad_2 + D$panas_alone_2 + D$panas_blue_2 + D$panas_lonely_2 + 
                         D$panas_downhearted_2 - 5 ) / 20
D1$panas_Sadness_d <- D1$panas_Sadness_2 - D1$panas_Sadness_1

D1$panas_Joviality_1 <- ( D$panas_cheerful_1 + D$panas_delighted_1 + D$panas_happy_1 + D$panas_joyful_1 + 
                           D$panas_excited_1 + D$panas_lively_1 + D$panas_enthusiastic_1 + D$panas_energetic_1 - 8 ) / 32
D1$panas_Joviality_2 <- ( D$panas_cheerful_2 + D$panas_delighted_2 + D$panas_happy_2 + D$panas_joyful_2 + 
                           D$panas_excited_2 + D$panas_lively_2 + D$panas_enthusiastic_2 + D$panas_energetic_2 - 8 ) / 32
D1$panas_Joviality_d <- D1$panas_Joviality_2 - D1$panas_Joviality_1

D1$panas_Attentiveness_1 <- ( D$panas_attentive_1 + D$panas_alert_1 + D$panas_determined_1 + 
                               D$panas_concentrating_1 - 4 ) / 16
D1$panas_Attentiveness_2 <- ( D$panas_attentive_2 + D$panas_alert_2 + D$panas_determined_2 + 
                               D$panas_concentrating_2 - 4 ) / 16
D1$panas_Attentiveness_d <- D1$panas_Attentiveness_2 - D1$panas_Attentiveness_1

D1$panas_Fatigue_1 <- ( D$panas_sluggish_1 + D$panas_tired_1 + D$panas_sleepy_1 + D$panas_drowsy_1 - 4 ) / 16
D1$panas_Fatigue_2 <- ( D$panas_sluggish_2 + D$panas_tired_2 + D$panas_sleepy_2 + D$panas_drowsy_2 - 4 ) / 16
D1$panas_Fatigue_d <- D1$panas_Fatigue_2 - D1$panas_Fatigue_1

D1$panas_Serenity_1 <- ( D$panas_relaxed_1 + D$panas_calm_1 + D$panas_at_ease_1 - 3 ) / 12
D1$panas_Serenity_2 <- ( D$panas_relaxed_2 + D$panas_calm_2 + D$panas_at_ease_2 - 3 ) / 12
D1$panas_Serenity_d <- D1$panas_Serenity_2 - D1$panas_Serenity_1



###CLEAN_DATA###
#Drop null values
D1 <- drop_na(D1)

#change randomization name
D1 <- D1 %>% mutate(D.randomization = replace(D.randomization, D.randomization == 1, 'High')) %>%
  mutate(D.randomization= replace(D.randomization, D.randomization == 0, 'Low'))

#convert randomization into factor
D1$D.randomization <- as.factor(D1$D.randomization)

##Change treatment times into numeric for Linear regression analysis.
D1 <- D1 %>% mutate(D.redcap_event_name = replace(D.redcap_event_name, D.redcap_event_name == 't1_arm_1', 1 ))%>%
  mutate(D.redcap_event_name = replace(D.redcap_event_name, D.redcap_event_name == 't2_arm_1', 2 )) %>%
  mutate(D.redcap_event_name = replace(D.redcap_event_name, D.redcap_event_name == 't3_arm_1', 3 )) %>%
  mutate(D.redcap_event_name = replace(D.redcap_event_name, D.redcap_event_name == 't4_arm_1', 4 )) %>%
  mutate(D.redcap_event_name = replace(D.redcap_event_name, D.redcap_event_name == 't5_arm_1', 5 )) %>%
  mutate(D.redcap_event_name = replace(D.redcap_event_name, D.redcap_event_name == 't6_arm_1', 6 )) 

#Rename Columns to Make Pretty
colnames(D1)<- c('subject_id', 'Visit', 'randomization','Panas_Negative_Pre','Panas_Negative_Post',
                 'Panas_Negative_Diff','Panas_Positive_Pre', 'Panas_Positive_Post', 'Panas_Positive_Diff',
                 'Panas_Fear_Pre','Panas_Fear_Post','Panas_Fear_Diff',
                 'Panas_Sadness_Pre','Panas_Sadness_Post', 'Panas_Sadness_Diff',
                 'Panas_Joviality_Pre','Panas_Joviality_Post', 'Panas_Joviality_Diff',
                 'Panas_Attentiveness_Pre','Panas_Attentiveness_Post','Panas_Attentiveness_Diff',
                 'Panas_Fatigue_Pre','Panas_Fatigue_Post','Panas_Fatigue_Diff',
                 'Panas_Serenity_Pre','Panas_Serenity_Post','Panas_Serenity_Diff')


###ANALYSIS###
#Summary using rstatix
#for scale of interest, change first term after get_sum_stats
PANAS_Summary <- D1 %>% group_by(randomization, Visit) %>%
  get_summary_stats(Panas_Fatigue_Pre, type = 'mean_se')


#Mixed Linear Model
PANAS_lm <- lmer(Panas_Negative_Post ~ as.factor(randomization) *
                  as.numeric(Visit) + (1|subject_id), data = D1) 

#convert lm into Anova
PANAS_anova <- anova(PANAS_lm) 

# main effect
PANAS_eta_sq <- eta_sq(PANAS_lm) 

#Pairwise analysis
Panas_pairwise_visit <- emmeans(PANAS_lm, list(pairwise ~ randomization | Visit),
                               adjust = 'bonferroni') 



###VISUALIZE###
#All you have to do is change y column name

PANAS_line <- ggline(D1, x= 'Visit', y= 'Panas_Fatigue_Pre',
                    color = 'randomization', palette = 'jco', add = 'mean_se') +
                    labs( title = 'Fatigue Pre')




###RESULTS###
print(PANAS_Summary)
print(PANAS_line)
print(PANAS_anova)
print(Panas_pairwise_visit, which = 2)
print(PANAS_eta_sq)
