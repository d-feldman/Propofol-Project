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
#tables
library(flextable)


###SETUP###
##Set working Directory and load CSV##
setwd('/Users/danielfeldman/Desktop/NAP_Example_Data')

D <- read.csv('NAPStudyCCTS4510-PANAS_DATA_2022-05-04_1530.csv', header=T)

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
#Drop null values, change rand. name, convert into factors
D1 <- drop_na(D1)
D1 <- D1 %>% mutate(D.randomization = replace(D.randomization, D.randomization == 1, 'High')) %>%
  mutate(D.randomization= replace(D.randomization, D.randomization == 0, 'Low'))
D1$D.randomization <- as.factor(D1$D.randomization)
D1$D.redcap_event_name <- as.factor(D1$D.redcap_event_name)




###ANALYSIS###
#Summary using rstatix
#for scale of interest, change first term after get_sum_stats
PANAS_Summary <- D1 %>% group_by(D.randomization, D.redcap_event_name) %>%
  get_summary_stats(panas_Positive_1, type = 'mean_sd')


#Mixed Linear Model
PANAS_lm <- lmer(formula = panas_Positive_1 ~ as.factor(D.randomization) *
                  as.factor(D.redcap_event_name)+(1|D.subject_id), data = D1) 

#convert lm into Anova
PANAS_anova <- anova(PANAS_lm) 

# main effect
PANAS_eta_sq <- eta_squared(PANAS_lm) 

# #pairwise significance per visit using emmeans
PANAS_pairwise_visit <- emmeans(PANAS_lm, list(pairwise ~ D.randomization | 
                                D.redcap_event_name),  adjust = 'bonferroni') 


#make pairwise-t-test in vector form (using rstatix)
#reasoning is to aid with visualization later on. 
#This returns the effects of randomization group on value at each time point
#to change scale of interest, change first term in pwc-t-test to scale name 
#found in D1
PANAS_pwc <- D1 %>%
  group_by(D.redcap_event_name) %>%
  pairwise_t_test(panas_Positive_1 ~ D.randomization, 
                  p.adjust.method = 'bonferroni')

PANAS_pwc <-PANAS_pwc %>% add_xy_position(x = 'D.redcap_event_name')





###VISUALIZE###
#All you have to do is change y column name

PANAS_line <- ggline(D1, x= 'D.redcap_event_name', y= 'panas_Positive_1',
                    color = 'D.randomization', palette = 'jco', add = 'mean_sd') +
                    stat_pvalue_manual(PANAS_pwc,
                    label = "p = {p}, {p.signif}",
                    vjust = -1) +
                    scale_y_continuous(expand = expansion(mult = c(0.05, 0.15)))




###RESULTS###
print(PANAS_Summary)
print(PANAS_line)
print(PANAS_anova)
print(summary(PANAS_pairwise_visit, which = 2))
