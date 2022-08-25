###PANAS Change over tx course to predict HDRS change over treatment course
###pulls from both panas cleaning and hdrs cleaning scripts

##Libraries##
#manipulating data
library(dplyr)
library(tidyr)
library(lme4)

PANAS_prepost <- D_hl %>% 
  group_by(subject_id) %>%
  summarise(diff = PANAS_Sadness_Difference[Visit_Number == c('6')] -
              PANAS_Sadness_Difference[Visit_Number == c('1')])

HDRS_prepost <- D_cleaned %>%
  group_by(Subject) %>%
  summarise(diff = HDRS_24[Visit == c('Week 2')] - HDRS_24[Visit == c('Baseline')])

PANAS_HDRS_Merge <- merge(PANAS_prepost, HDRS_prepost, 
                          by.x = 'subject_id', by.y = 'Subject')

colnames(PANAS_HDRS_Merge) <- c('Subject', 'PANAS_Post_Pre', 'HDRS_Post_Pre')

##liner model

PANAS_HDRS_model <-  lm(formula = HDRS_Post_Pre ~ PANAS_Post_Pre,
                         data = PANAS_HDRS_Merge)

##anova
PANAS_HDRS_anova <- anova(PANAS_HDRS_model)

print(PANAS_HDRS_model)
print(summary(PANAS_HDRS_model))
print(PANAS_HDRS_anova)