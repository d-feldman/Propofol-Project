#PANAS Statistic Analysis
#statistics
library(lme4)
library(lmerTest)
#library(car)
library(emmeans)
library(sjstats)



#Mixed Linear Model
PANAS_lm <- lmer(formula = PANAS_Sadness_Difference ~ Dose *
                  Visit_Number + (1|subject_id), data = D_hl) 

#convert lm into Anova
PANAS_anova <- anova(PANAS_lm) 

# main effect
PANAS_eta_sq <- eta_sq(PANAS_lm, partial= T) 

#Pairwise analysis
PANAS_pairwise_visit <- emmeans(PANAS_lm, list(pairwise ~ Dose | Visit_Number),
                               adjust = 'bonferroni')

# 
# ##Correlations 
# D3 <- D2
# 
# D3 <- select(D3, -Visit)
# D3 <- select(D3, -Dose_Time)
# 
# D3$Visit_Number <- as.numeric(D3$Visit_Number)
# 
# 
# num.cols <- sapply(D3, is.numeric)
# 
# cor.data <- cor(D3[,num.cols])

#print(PANAS_lm)
print(PANAS_anova)
#print(PANAS_pairwise_visit, which = 2)
print(summary(PANAS_lm))