###PANAS Time Stats
library(lme4)
library(lmerTest)
library(emmeans)
library(sjstats)


#Mixed Linear Model
PANAS_lm <- lmer(formula = PANAS_Fatigue ~ Time * Group * Visit_Number + (1|subject_id), data = D_results) 

#convert lm into Anova
PANAS_anova <- anova(PANAS_lm) 

# main effect
PANAS_eta_sq <- eta_sq(PANAS_lm, partial= T) 


write.csv(PANAS_anova, file = 'PANAS_time.csv')





#print results 
print(PANAS_lm)
print(summary(PANAS_lm))
print(PANAS_anova)
print(PANAS_eta_sq)
