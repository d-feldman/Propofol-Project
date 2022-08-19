#HDRS Statistic Analysis
###packeges
#statistics
library(lme4)
library(lmerTest)
library(emmeans)
library(sjstats)


#Mixed Linear Model
HDRS_lm <- lmer(formula = HDRS_24 ~ Dose *
                   Visit + (1|Subject), data = D_cleaned) 

#convert lm into Anova
HDRS_anova <- anova(HDRS_lm) 

# main effect
HDRS_eta_sq <- eta_sq(HDRS_lm, partial= T) 

#Pairwise analysis
HDRS_pairwise_visit <- emmeans(HDRS_lm, list(pairwise ~ Dose | Visit),
                                adjust = 'bonferroni')

###print results
print(HDRS_lm)
print(HDRS_anova)
print(HDRS_pairwise_visit, which = 2)
print(summary(HDRS_lm))