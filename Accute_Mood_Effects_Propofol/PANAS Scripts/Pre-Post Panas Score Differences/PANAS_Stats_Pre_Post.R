#PANAS Statistic Analysis
#statistics
library(lme4)
library(lmerTest)
library(emmeans)
library(sjstats)
library(corrplot)


#Mixed Linear Model
PANAS_lm <- lmer(formula = PANAS_Negative ~ as.factor(Dose_Time) *
                  as.factor(Visit_Number) + (1|subject_id), data = D_results_randomized_only) 

#convert lm into Anova
PANAS_anova <- anova(PANAS_lm) 

# main effect
PANAS_eta_sq <- eta_sq(PANAS_lm, partial= T) 

#Pairwise analysis
PANAS_pairwise_visit <- emmeans(PANAS_lm, list(pairwise ~ Dose_Time | Visit_Number), 
                                              adjust = 'bonferroni')


##Correlations 
D3 <- D_results_randomized_only

D3 <- select(D3, -Visit)
D3 <- select(D3, -Dose_Time)

D3$Visit_Number <- as.numeric(D3$Visit_Number)


num.cols <- sapply(D3, is.numeric)

cor.data <- cor(D3[,num.cols])


#optionally print results in console
print(PANAS_lm)
print(PANAS_anova)
print(PANAS_pairwise_visit, which = 2)
print(PANAS_eta_sq)
print(cor.data)
