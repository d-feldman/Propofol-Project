#PANAS 1st visit Statistic Analysis

#libraries
library(dplyr)
library(tidyr)
library(sjstats)
library(car)
library(lsr)



###For Pre-Post Collapsed### Paired T-Test
###Summary Stats###
D_Sum <- group_by(D_results_randomized, Time) %>%
  summarise(count=n(), mean = mean(PANAS_Sadness,na.rm=T),
            sd = sd(PANAS_Sadness, na.rm=T))

###check normality###
D_normality <-with(D_results_randomized, 
                   PANAS_Sadness[Time == 'Pre'] - PANAS_Sadness[Time == 'Post'])

###paired T-Test###
D_ptt<- t.test(PANAS_Sadness ~ Time, data=D_results_randomized, paired = T)
D_ptt1<- t.test(PANAS_Fear ~ Time, data=D_results_randomized, paired = T)
D_ptt2<- t.test(PANAS_Joviality ~ Time, data=D_results_randomized, paired = T)
D_ptt3<- t.test(PANAS_Attentiveness ~ Time, data=D_results_randomized, paired = T)
D_ptt4<- t.test(PANAS_Fatigue ~ Time, data=D_results_randomized, paired = T)
D_ptt5<- t.test(PANAS_Serenity ~ Time, data=D_results_randomized, paired = T)


###Cohen's D###
D_cd<- cohensD(PANAS_Sadness ~ Time, data=D_results_randomized)
D_cd1<- cohensD(PANAS_Fear ~ Time, data=D_results_randomized)
D_cd2<- cohensD(PANAS_Joviality ~ Time, data=D_results_randomized)
D_cd3<- cohensD(PANAS_Attentiveness ~ Time, data=D_results_randomized)
D_cd4<- cohensD(PANAS_Fatigue ~ Time, data=D_results_randomized)
D_cd5<- cohensD(PANAS_Serenity ~ Time, data=D_results_randomized)

##print##
print(D_Sum)
print(shapiro.test(D_normality))
print(D_ptt)




# ###for pre-post dose-- two-way repeated measures anova-- within subject. between dose.
# D_results_randomized$subject_id <- as.factor(D_results_randomized$subject_id)
# D_results_randomized$Group <- factor(D_results_randomized$Group, levels = c('Low', 'High'))
# D_results_randomized$Time <- factor(D_results_randomized$Time, levels = c('Pre', 'Post'))
# 
# 
# D_lm <- lmer(data=D_results_randomized, PANAS_Serenity~ Group * Time + (1|(subject_id)))
# 
# d_anova <- Anova(D_lm)
# 
# write.csv(d_anova, file = 'MM1st.csv')
# 
# 
# PANAS_eta_sq <- eta_sq(D_lm, partial= T) 
# 
# print(d_anova)
# print(PANAS_eta_sq)


