###PANAS Change over tx course to predict HDRS change over treatment course
###pulls from both panas cleaning and hdrs cleaning scripts

##Libraries##
#manipulating data
library(dplyr)
library(tidyr)
library(lme4)

PANAS_Prepost_Sadness_Diff<- D_hl %>% 
  group_by(subject_id) %>%
  summarise(diff = PANAS_Sadness_Difference[Visit_Number == c('6')] -
              PANAS_Sadness_Difference[Visit_Number == c('1')])
Initial_Sadness_Diff <- D_hl %>% group_by(subject_id) %>%
  summarise(PANAS_Sadness_Difference[Visit_Number == c('1')])
Mean_Sadness_Diff <- D_hl %>% group_by(subject_id) %>%
  summarise(mean(PANAS_Sadness_Difference))

HDRS_prepost <- D_cleaned %>%
  group_by(subject_id) %>%
  summarise(diff = HDRS_24[Visit == c('Week 2')] - HDRS_24[Visit == c('Baseline')])

PANAS_HDRS_Merge <- merge(PANAS_Prepost_Sadness_Diff, HDRS_prepost, 
                          by.x = 'subject_id', by.y = 'subject_id')
PANAS_HDRS_Merge <- merge(PANAS_HDRS_Merge, Initial_Sadness_Diff, 
                          by.x = 'subject_id', by.y = 'subject_id')
PANAS_HDRS_Merge <- merge(PANAS_HDRS_Merge, Mean_Sadness_Diff, 
                          by.x = 'subject_id', by.y = 'subject_id')


colnames(PANAS_HDRS_Merge) <- c('Subject', 'PANAS_Post_Pre_Sadness_Diff',
                                'HDRS_Post_Pre', 'PANAS_Initial_Sadness_Diff',
                                'PANAS_Mean_Sadness_Diff')

##liner model

PANAS_HDRS_model <-  lm(formula = HDRS_Post_Pre ~ PANAS_Post_Pre_Sadness_Diff+
                          PANAS_Initial_Sadness_Diff,
                         data = PANAS_HDRS_Merge)

##anova
PANAS_HDRS_anova <- anova(PANAS_HDRS_model)

print(PANAS_HDRS_model)
print(summary(PANAS_HDRS_model))
print(PANAS_HDRS_anova)