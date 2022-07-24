#PANAS Statistic Analysis
#statistics
library(lme4)
library(lmerTest)
library(emmeans)
library(sjstats)
library(corrplot)


#Mixed Linear Model
PANAS_lm <- lmer(formula = PANAS_Negative_Difference ~ as.factor(Dose_Time) *
                  as.factor(Visit_Number) + (1|subject_id), data = D2) 

#convert lm into Anova
PANAS_anova <- anova(PANAS_lm) 

# main effect
PANAS_eta_sq <- eta_sq(PANAS_lm, partial= T) 

#Pairwise analysis
PANAS_pairwise_visit <- emmeans(PANAS_lm, list(pairwise ~ Dose_Time | Visit_Number),
                               adjust = 'bonferroni')


##Correlations 
D3 <- D2

D3 <- select(D3, -Visit)
D3 <- select(D3, -Dose_Time)

D3$Visit_Number <- as.numeric(D3$Visit_Number)


num.cols <- sapply(D3, is.numeric)

cor.data <- cor(D3[,num.cols])
