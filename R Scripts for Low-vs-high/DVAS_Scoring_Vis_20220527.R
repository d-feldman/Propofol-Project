# DVAS
#make sure you install all packages into RStudio before running the script#
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





##Set working Directory and load CSV##
setwd('/Users/danielfeldman/Desktop/NAP_Example_Data')

D <- read.csv('NAPStudyCCTS4510_DVAS.csv', header=T)



##Make Dataframe only go through week 2##
D1 <- D %>% filter( redcap_event_name == 'baseline_arm_1' | redcap_event_name == 't1_arm_1' | 
                    redcap_event_name == 't2_arm_1' | redcap_event_name == 't3_arm_1'|
                     redcap_event_name == 't4_arm_1' | 
                    redcap_event_name == 't5_arm_1' | redcap_event_name == 't6_arm_1')



#expand randomization to other visits##
D1 <- D1 %>% group_by(subject_id) %>% fill(randomization) %>% group_by(randomization)



#add id, delete dates, delete null values, mutate, make factors##
#the goal here is to transform the dataframe to make it friendly to analysis#
D1 <- select(D1, -dvas_date_s)
D1 <- D1 %>% mutate(redcap_event_name = replace( redcap_event_name, 
              redcap_event_name == 'baseline_arm_1', NA))
D1 <- drop_na(D1)

D1 <- D1 %>% mutate(randomization = replace(randomization, randomization == 1, 'High'))%>%
       mutate(randomization= replace(randomization, randomization == 0, 'Low'))

###Rename Columns to Make Pretty##
colnames(D1)<- c('subject_id', 'Visit', 'randomization','Date_Diff','Depressed','Mood_Low','Lack_Interest',
                 'Not_Enjoy','Motivation_Low','Low_Energy','Pessimistic','Negative_Self_View',
                 'Physical_Pain','Survey_Complete')

#make randomization and subject visits factors
D1$randomization <- as.factor(D1$randomization)

#Change Visit to numeric for linear regression
D1 <- D1 %>% mutate(Visit = replace(Visit, Visit == 't1_arm_1', 1 ))%>%
  mutate(Visit= replace(Visit, Visit == 't2_arm_1', 2 )) %>%
  mutate(Visit= replace(Visit, Visit == 't3_arm_1', 3 )) %>%
  mutate(Visit= replace(Visit, Visit == 't4_arm_1', 4 )) %>%
  mutate(Visit= replace(Visit, Visit == 't5_arm_1', 5 )) %>%
  mutate(Visit= replace(Visit, Visit == 't6_arm_1', 6 )) 



###STATISTIC ANALYSIS###
#Summary using rstatix
#for scale of interest, change first term after get_sum_stats
DVAS_Summary <- D1 %>% group_by(randomization, Visit) %>%
   get_summary_stats(Depressed, type = 'mean_se')


#Mixed Linear Model
DVAS_lm <- lmer(formula = Depressed ~ as.factor(randomization) *
                    as.numeric(Visit) + (1|subject_id), data = D1) 

#convert lm into Anova
DVAS_anova <- anova(DVAS_lm) 

# main effect
DVAS_eta_sq <- eta_sq(DVAS_lm, partial= T) 

#Pairwise analysis
DVAS_pairwise_visit <- emmeans(DVAS_lm, list(pairwise ~ randomization | Visit),
                                 adjust = 'bonferroni') 





##VISUALIZATION###
#ggpubr
#to change scale of interest, change y value to scale name found in D1
DVAS_line <- ggline(D1, x= 'Visit', y= 'Depressed',
                    color = 'randomization', palette = 'jco', add = 'mean_se') +
      labs (title = 'Low Energy') + 
    annotate("text", x =3, y= 88, label = "*" , size = 6) + 
  annotate("text", x =4, y= 86, label = "*" , size = 6) +
  annotate("text", x =5, y= 84, label = "**" , size = 6) +
  annotate("text", x =6, y= 84, label = "**" , size = 6) 
                    



##Return Results##
print(DVAS_Summary)
print(DVAS_line)
print(DVAS_anova)
print(DVAS_pairwise_visit, which = 2)
print(DVAS_eta_sq)


