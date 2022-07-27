#PANAS Statistic Analysis
#statistics
library(lme4)
library(lmerTest)
library(emmeans)
library(sjstats)
library(corrplot)


#Mixed Linear Model
DEQ5_lm <- lmer(formula = High ~ Dose_Time *
                  TX_Number + (1|Subject), data = D2) 

#convert lm into Anova
DEQ5_anova <- anova(DEQ5_lm) 

# main effect
DEQ5_eta_sq <- eta_sq(DEQ5_lm, partial= T) 

#Pairwise analysis
DEQ5_pairwise_visit <- emmeans(DEQ5_lm, list(pairwise ~ Dose_Time | TX_Number), 
                               adjust = 'bonferroni')
                                              


##Correlations 
D3 <- D2

D3 <- select(D3, -Visit)
D3 <- select(D3, -Dose_Time)

D3$TX_Number <- as.numeric(D3$TX_Number)


num.cols <- sapply(D3, is.numeric)

cor.data <- cor(D3[,num.cols])


#optionally print results in console
print(DEQ5_lm)
print(DEQ5_anova)
print(DEQ5_pairwise_visit, which = 2)
print(DEQ5_eta_sq)
corrplot(cor.data)
