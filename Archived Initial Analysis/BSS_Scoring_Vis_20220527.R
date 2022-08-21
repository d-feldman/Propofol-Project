#BSS
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



#####SETUP#####
##Set working Directory and load CSV##
setwd('/Users/danielfeldman/Desktop/NAP_Example_Data')

D <- read.csv('NAPStudyCCTS4510_BSS.csv', header=T)
#Make Dataframe  go through week 2 (using dplyr)
D <- D %>% filter(redcap_event_name == 'baseline_arm_1' |
                     redcap_event_name == 'w1_arm_1' | 
                     redcap_event_name == 'w2_arm_1')
#expand randomization to other visits(using tidyr)
D <- D %>% group_by(subject_id) %>% fill(randomization)



#####SCORING#####
#begin dataframe with identify data
D1 <- data.frame(D$subject_id,D$redcap_event_name, D$randomization)

#simply sum of BSS 1-19
D1$BSS_total_score <- rowSums(D[6:24], na.rm=T)

#Low Risk Means Over Time#
DL <- data.frame(D1 %>% filter(D.redcap_event_name == 'baseline_arm_1', D.randomization=='0')) 

#Append other visits#
DL <- cbind(DL, (D1 %>% filter(D.redcap_event_name == 'w1_arm_1', D.randomization=='0')
                 %>% select(BSS_total_score)))
DL <- cbind(DL, (D1 %>% filter(D.redcap_event_name == 'w2_arm_1', D.randomization=='0')
                 %>% select(BSS_total_score)))

#repeat for high risk -- does the same as above
DH <- data.frame(D1 %>% filter(D.redcap_event_name == 'baseline_arm_1', D.randomization=='1')) 

#Append other visits#
DH <- cbind(DH, BSS_Week1 = (D1 %>% filter(D.redcap_event_name == 'w1_arm_1', D.randomization=='1')
                 %>% select(BSS_total_score)))
DH <- cbind(DH, BSS_Week2 = (D1 %>% filter(D.redcap_event_name == 'w2_arm_1', D.randomization=='1')
                 %>% select(BSS_total_score)))
 


#####DATA MANIPULATION#####
#Restructure and combine results#
colnames(DH) <- c('Subject','DELETE','Randomization','Baseline_BSS','Week1_BSS','Week2_BSS')
colnames(DL) <- c('Subject','DELETE','Randomization','Baseline_BSS','Week1_BSS','Week2_BSS')
DH <- (select (DH,-c (DELETE)))
DL <- (select (DL,-c (DELETE)))
BSS_full <- rbind(DH,DL)

#Now, Restructure once again for analysis form.
BSS_res <- BSS_full %>% 
  gather(key = 'Visit', value = 'Score',
         Baseline_BSS, Week1_BSS, Week2_BSS)
BSS_res['Randomization'][BSS_res['Randomization'] == 1] <- 'High'
BSS_res['Randomization'][BSS_res['Randomization'] == 0 ] <- 'Low'

BSS_res <- BSS_res %>% convert_as_factor(Subject, Randomization)

#Make randomization Factor Variable
BSS_res$Randomization <- as.factor(BSS_res$Randomization)

#Make visit into numeric for Linear regression
BSS_res <- BSS_res %>% mutate(Visit = replace(Visit, Visit == 'Baseline_BSS', 1 ))%>%
  mutate(Visit= replace(Visit, Visit == 'Week1_BSS', 2 )) %>%
  mutate(Visit= replace(Visit, Visit == 'Week2_BSS', 3 ))



######ANALYSIS#####
#Summary using rstatix
#for scale of interest, change first term after get_sum_stats
BSS_Summary <- BSS_res %>% group_by(Randomization, Visit) %>%
  get_summary_stats(Score, type = 'mean_se')

#Mixed Linear Model
BSS_lm <- lmer(formula = Score ~ as.factor(Randomization) *
                as.numeric(Visit) + (1|Subject), data = BSS_res)

#convert lm into Anova
BSS_anova <- anova(BSS_lm) 

# main effect
BSS_eta_sq <- eta_sq(BSS_lm, partial= T)

#Pairwise analysis
BSS_pairwise_visit <- emmeans(BSS_lm, list(pairwise ~ Randomization | Visit),
                               adjust = 'bonferroni')




# #####VISUALIZATION#####
BSS_line <- ggline(BSS_res, x= 'Visit', y= 'Score',
                    color = 'Randomization', palette = 'jco', add = 'mean_se'
                   ) 


#####RESULTS#####
print(BSS_Summary)
print(BSS_line)
print(BSS_anova)
print(summary(BSS_pairwise_visit, which = 2))


