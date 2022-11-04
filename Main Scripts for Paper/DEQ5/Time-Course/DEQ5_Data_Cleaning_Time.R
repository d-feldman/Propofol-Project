#DEQ5
##Libraries##
#manipulating data
library(dplyr)
library(tidyr)



###SETUP###
##Set working Directory and load CSV##
setwd('/Users/danielfeldman/Desktop/Prop Paper/DEQ5/')

D <- read.csv('NAP_DEQ5.csv', header=T)

##Make Dataframe contain only treatment dates
D <- D %>% filter( redcap_event_name == 'baseline_arm_1' | redcap_event_name == 't1_arm_1' | 
                      redcap_event_name == 't2_arm_1' | redcap_event_name == 't3_arm_1'|
                      redcap_event_name == 't4_arm_1' | 
                      redcap_event_name == 't5_arm_1' | redcap_event_name == 't6_arm_1')

#expand randomization to other visits
D <- D %>% group_by(subject_id) %>% fill(randomization) %>% group_by(randomization)


###CLEAN_DATA###
#Drop null values
D1 <- drop_na(D)

##rename columns
colnames(D1) <- c('Subject','Visit','Group','Time_Post_TX', 'Drug_Effect', 
                  'High','Dislike','Like','Want_More')
  
#change randomization name
D1 <- D1 %>% mutate(Group = replace(Group, Group == 1, 'High Dose')) %>%
  mutate(Group= replace(Group, Group == 0, 'Low Dose'))

##results table
D2 <-D1




##make numeric column for regression

#First, Make Visit a character string
D2$Visit <- as.character(D2$Visit)

#New Column
D2$TX_Number <- NA
D2 <- D2 %>% relocate(TX_Number, .after = Visit)

##Add in numbers, making visits 7-12 same as 1-6 for time series alignment
D2 <- D2 %>% mutate(TX_Number = case_when(Visit == 't1_arm_1'~ 1,
      Visit == 't2_arm_1'~ 2, Visit == 't3_arm_1' ~ 3,
      Visit == 't4_arm_1'~ 4, Visit == 't5_arm_1' ~ 5, Visit == 't6_arm_1'~ 6)) 

##set data types
D2$TX_Number <- as.numeric(D2$TX_Number)
D2$Group <- factor(D2$Group, levels = c('Low Dose', 'High Dose'))
###Data has been cleaned!
