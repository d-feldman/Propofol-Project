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
D <- D %>% filter( redcap_event_name == 'baseline_arm_1' | redcap_event_name == 't1_arm_1')

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

D1$Visit <- 'Visit 1'

D1$Group <- factor(D1$Group, levels = c('Low', 'High'))


names(D1)[names(D1) == 'Group'] <- 'Dose'

###Data has been cleaned!
