#DEQ5
##Libraries##
#manipulating data
library(dplyr)
library(tidyr)



###SETUP###
##Set working Directory and load CSV##
setwd('/Users/danielfeldman/Desktop/NAP_Example_Data')

D <- read.csv('NAPStudyCCTS4510_DEQ5.csv', header=T)

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


###CLEAN_DATA###
#Drop null values
D1 <- drop_na(D)

##rename columns
colnames(D1) <- c('Subject','Visit','Group','Time_Post_TX', 'Drug_Effect', 
                  'High','Dislike','Like','Want_More')
  
#change randomization name
D1 <- D1 %>% mutate(Group = replace(Group, Group == 1, 'High')) %>%
  mutate(Group= replace(Group, Group == 0, 'Low'))

##results table
D2 <-D1

###make Dose column
##Now Make into 3 groups of randomization##
D2$Dose_Time <- NA
D2 <- D2 %>% relocate(Dose_Time, .before= Time_Post_TX)

### conditionally fill for initial high dose vs high after low
D2 <- D2 %>% mutate(Dose_Time = case_when(Group == 'High' & Visit == 't1_arm_1' ~ 'Initial_High_Dose', 
                                          Group == 'Low'  & Visit == 't1_arm_1' ~ 'Initial_Low_Dose', 
                                          Group == 'Low'  & Visit == 't7_arm_1' ~ 'Open_Label_High_Dose',
                                          Group == 'High' & Visit == 't7_arm_1' ~ 'Open_Label_Low_Dose'))
#Expand Labels to other visits
D2 <- D2 %>% group_by(Group) %>% fill(Dose_Time) %>% group_by(Dose_Time)

#Drop Open Label Low Dose
D2 <- subset(D2, Dose_Time != 'Open_Label_Low_Dose')

#Delete old Group Column
D2 <- select(D2, -Group)

##make numeric column for regression

#First, Make Visit a character string
D2$Visit <- as.character(D2$Visit)

#New Column
D2$TX_Number <- NA
D2 <- D2 %>% relocate(TX_Number, .after = Visit)

##Add in numbers, making visits 7-12 same as 1-6 for time series alignment
D2 <- D2 %>% mutate(TX_Number = case_when(Visit == 't1_arm_1'~ 1,
      Visit == 't2_arm_1'~ 2, Visit == 't3_arm_1' ~ 3,
      Visit == 't4_arm_1'~ 4, Visit == 't5_arm_1' ~ 5, Visit == 't6_arm_1'~ 6, 
      Visit == 't7_arm_1'~ 1, Visit == 't8_arm_1'~ 2, Visit == 't9_arm_1' ~ 3,
      Visit == 't10_arm_1'~ 4, Visit == 't11_arm_1' ~ 5, Visit == 't12_arm_1'~ 6)) 


###Data has been cleaned!
