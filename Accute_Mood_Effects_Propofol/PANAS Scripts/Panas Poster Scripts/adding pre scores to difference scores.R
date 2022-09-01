### add in Panas sadness pre to DF
library(lme4)
library(lmerTest)
#library(car)
library(emmeans)
library(sjstats)

panas_sadness_pre_only <- D_results_randomized_only %>% filter(Dose_Time == 'Low_Dose_Pre' | Dose_Time == 'High_Dose_Pre') %>%
                          select(PANAS_Sadness)

D_compare_diff_pre <- D_hl

D_compare_diff_pre$PANAS_Sadness_Pre <- panas_sadness_pre_only$PANAS_Sadness


PANAS_lm_pre_diff <- lmer(formula = PANAS_Sadness_Difference ~ Dose *
                   Visit_Number * PANAS_Sadness_Pre + (1|subject_id), data = D_compare_diff_pre) 

#convert lm into Anova
PANAS_anova_pre_diff <- anova(PANAS_lm_pre_diff) 

# main effect
#PANAS_eta_sq <- eta_sq(PANAS_lm, partial= T) 

#Pairwise analysis
#PANAS_pairwise_visit <- emmeans(PANAS_lm, list(pairwise ~ Dose | Visit_Number),
#                                adjust = 'bonferroni')

print(PANAS_lm_pre_diff)
print(PANAS_anova_pre_diff)
print(summary(PANAS_lm_pre_diff))