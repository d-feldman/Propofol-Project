#PANAS Statistic Analysis
#statistics
library(lme4)
library(lmerTest)
library(emmeans)
library(sjstats)
library(corrplot)


#Mixed Linear Model
DEQ5_lm <- lmer(formula = Want_More ~ Group *
                  TX_Number + (1|Subject), data = D2) 

#convert lm into Anova
DEQ5_anova <- anova(DEQ5_lm) 

# main effect
DEQ5_eta_sq <- eta_sq(DEQ5_lm, partial= T) 

write.csv(DEQ5_anova, file = 'DEQ5_Results.csv', col.names = T)


#optionally print results in console
print(DEQ5_lm)
print(summary(DEQ5_lm))
print(DEQ5_anova)
print(DEQ5_eta_sq)
