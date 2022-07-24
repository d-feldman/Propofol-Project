# WHOQOL-BREF
###Libraries###
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
setwd('/Users/danielfeldman/Desktop/NAP_Example_Data/')

D <- read.csv('NAPStudyCCTS4510_WHOQOL.csv', header=T)

#Make Dataframe contain relevant weeks
D1 <- D %>% filter(redcap_event_name == 'baseline_arm_1' |
                    redcap_event_name == 'w2_arm_1' ) 

#expand randomization to other visits
D1 <- D1 %>% group_by(subject_id) %>% fill(randomization)

#drop null values
D1 <- drop_na(D1)



###SCORING###
#Calculate Domain Scores, standardize on 100 point scale#
D2 <- data.frame(D1$subject_id,D1$redcap_event_name,D1$randomization)

D2$Quality_of_life <- (D1$whoqol_1+D1$whoqol_2) * (100/20) 
  
D2$physical_health <- ((6-D1$whoqol_3) + (6-D1$whoqol_4) + D1$whoqol_10 + D1$whoqol_15 + 
                         D1$whoqol_16 + D1$whoqol_17 + D1$whoqol_18) * (100/35) 
  
D2$psychological <- (D1$whoqol_5 + D1$whoqol_6 + D1$whoqol_7 + D1$whoqol_11 + D1$whoqol_19 +
                       (6-D1$whoqol_26)) * (100/30)
  
D2$social_relationships <- (D1$whoqol_20 + D1$whoqol_21 + D1$whoqol_22) * (100/15) 
  
D2$environment <- (D1$whoqol_8 + D1$whoqol_9 + D1$whoqol_12 + D1$whoqol_12 + D1$whoqol_13
                   + D1$whoqol_14 + D1$whoqol_23 + D1$whoqol_24 + D1$whoqol_25) * (100/40) 



###FURTHER SETUP###
#Change Dose Names
D2 <- D2 %>% mutate(D1.randomization = replace(D1.randomization, D1.randomization == 1, 'High'))%>%
  mutate(D1.randomization= replace(D1.randomization, D1.randomization == 0, 'Low'))

##Rename Columns to Make Pretty##
colnames(D2)<- c('subject_id', 'Visit', 'randomization','Quality_of_life','Physical_health','Psychological',
                 'Social_relationships', 'Environment')

##Change names of visits to numeric for correct order  and data class in analysis##
D2 <- D2 %>% mutate(Visit = replace(Visit, 
                                    Visit == 'baseline_arm_1', 1 ))%>%
  mutate(Visit= replace(Visit, Visit == 'w2_arm_1', 2 )) 

#make randomization a factor
D2$randomization <- as.factor(D2$randomization)



###STATISTIC ANALYSIS###
#Summary using rstatix
#for scale of interest, change first term after get_sum_stats
WHOQOL_Summary <- D2 %>% group_by(randomization, Visit) %>%
  get_summary_stats(Physical_health, type = 'mean_se')


#Mixed Linear Model
WHOQOL_lm <- lmer(formula = Physical_health ~ as.factor(randomization) *
                as.numeric(Visit) + (1|subject_id), data = D2) ## linear model

#convert lm into Anova
WHOQOL_anova <- anova(WHOQOL_lm)

# main effect
WHOQOL_eta_sq <- eta_sq(WHOQOL_lm)

# #pairwise significance per visit using emmeans
WHOQOL_pairwise_visit <- emmeans(WHOQOL_lm, list(pairwise ~ randomization | Visit),
                             adjust = 'bonferroni')



##VISUALIZATION###
#ggpubr
#to change scale of interest, change y value to scale name found in D2
WHOQOL_line <- ggline(D2, x= 'Visit', y= 'Environment',
                    color = 'randomization', palette = 'jco', add = 'mean_se' ) +
                    scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) + 
                    labs(title = "Environment")



##Return Results##
print(WHOQOL_Summary)
print(WHOQOL_line)
print(WHOQOL_lm)
print(WHOQOL_anova)
print(WHOQOL_eta_sq)
print(summary(WHOQOL_pairwise_visit, which = 2))
