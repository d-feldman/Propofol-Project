#PANAS 1st visit Statistic Analysis

#libraries
library(dplyr)
library(tidyr)
library(sjstats)
library(car)



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


##print##
print(D_Sum)
print(shapiro.test(D_normality))
print(D_ptt)




###for pre-post dose-- two-way repeated measures anova-- within subject. between dose.
D_results_randomized$subject_id <- as.factor(D_results_randomized$subject_id)
D_results_randomized$Group <- factor(D_results_randomized$Group, levels = c('Low', 'High'))
D_results_randomized$Time <- factor(D_results_randomized$Time, levels = c('Pre', 'Post'))


D_lm <- lmer(data=D_results_randomized, PANAS_Serenity~ Group * Time + (1|(subject_id)))

d_anova <- Anova(D_lm)

write.csv(d_anova, file = 'MM1st.csv')


PANAS_eta_sq <- eta_sq(D_lm, partial= T) 

print(d_anova)
print(PANAS_eta_sq)


