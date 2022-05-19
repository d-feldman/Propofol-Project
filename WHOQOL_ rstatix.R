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
#tables
library(flextable)

###SETUP###
##Set working Directory and load CSV##
setwd('/Users/danielfeldman/Desktop/NAP_Example_Data/')

D <- read.csv('NAPStudyCCTS4510-WHOQOL_DATA_2022-05-04_1459.csv', header=T)

#Make Dataframe contain relevant weeks
D1 <- D %>% filter(redcap_event_name == 'baseline_arm_1' |
                    redcap_event_name == 'w2_arm_1' | 
                     redcap_event_name == 'f7_arm_1')

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

##Change names of visits for correct order in analysis##
D2 <- D2 %>% mutate(Visit = replace(Visit, 
                                    Visit == 'baseline_arm_1', '1_baseline'))%>%
  mutate(Visit= replace(Visit, Visit == 'w2_arm_1', '2_Week2')) %>%
  mutate(Visit= replace(Visit, Visit == 'f7_arm_1', '3_7Week'))

#make randomization and subject visits factors
D2$randomization <- as.factor(D2$randomization)
D2$Visit <- as.factor(D2$Visit)



###STATISTIC ANALYSIS###
#Summary using rstatix
#for scale of interest, change first term after get_sum_stats
WHOQOL_Summary <- D2 %>% group_by(randomization, Visit) %>%
  get_summary_stats(Physical_health, type = 'mean_sd')


#make pairwise-t-test in vector form (using rstatix)
#reasoning is to aid with visualization later on. 
#This returns the effects of randomization group on value at each time point
#to change scale of interest, change first term in pw-t-test to scale name 
#found in D2
WHOQOL_pwc <- D2 %>%
  group_by(Visit) %>%
  pairwise_t_test(Physical_health ~ randomization, p.adjust.method = 'bonferroni')
WHOQOL_pwc <- WHOQOL_pwc %>% add_xy_position(x = 'Visit')

# ANOVA (rstatix)
WHOQOL.aov <- anova_test(data= D2, dv = Physical_health, wid = subject_id,
                       between = randomization, within = Visit)




##VISUALIZATION###
#ggpubr
#to change scale of interest, change y value to scale name found in D1
WHOQOL_line <- ggboxplot(D2, x= 'Visit', y= 'Physical_health',
                    color = 'randomization', palette = 'jco', add = 'mean_sd') +
                    stat_pvalue_manual(WHOQOL_pwc,
                    label = "p = {p}, {p.signif}",
                    vjust = -1, bracket.nudge.y = 2 ) +
                    scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
                    labs(subtitle = get_test_label(WHOQOL.aov, detailed = T ))
                      





##Return Results##
print(WHOQOL_Summary)
print(WHOQOL_line)
print(WHOQOL.aov)
