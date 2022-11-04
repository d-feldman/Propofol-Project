##PANAS_HDRS_corrplot

library(dplyr)
library(tidyr)
library(corrplot)
library(lme4)
library(sjstats)
library(car)
library(ggplot2)


##PANAS visit Difference
PANAS_1st_Diff <- D_PANAS_Diff %>% filter(Visit_Number == 1)
PANAS_1st_Diff <- drop_na(PANAS_1st_Diff)
PANAS_1st_Diff<- PANAS_1st_Diff %>% subset(Subject != 'PROP29')

PANAS_2nd_Diff <- D_PANAS_Diff %>% filter(Visit_Number == 2)
PANAS_2nd_Diff <- drop_na(PANAS_2nd_Diff)
PANAS_3rd_Diff <- D_PANAS_Diff %>% filter(Visit_Number == 3)
PANAS_3rd_Diff <- drop_na(PANAS_3rd_Diff)
PANAS_4th_Diff <- D_PANAS_Diff %>% filter(Visit_Number == 4)
PANAS_4th_Diff <- drop_na(PANAS_4th_Diff)



##PANAS Pre score 2nd, 3rd, 4th vis - 1st vis
PANAS2nd_1st <- D_PANAS_PP %>% select(subject_id, Visit_Number, Group, Time, PANAS_Sadness, PANAS_Fear, PANAS_Joviality, PANAS_Attentiveness,
                                      PANAS_Fatigue, PANAS_Serenity)
PANAS2nd_1st <- PANAS2nd_1st %>% filter(Time == 'Pre Treatment')

PANAS2nd_1st<- PANAS2nd_1st %>% subset(subject_id != 'PROP31')
PANAS2nd_1st<- PANAS2nd_1st %>% subset(subject_id != 'PROP29')



PANAS4th <- PANAS2nd_1st %>% filter (Visit_Number == 4)
PANAS3rd <- PANAS2nd_1st %>% filter (Visit_Number == 3)
PANAS2nd <- PANAS2nd_1st %>% filter (Visit_Number == 2)
PANAS1st <- PANAS2nd_1st %>% filter (Visit_Number == 1 )


PANAS_12_diff <- PANAS2nd[,5:10]-PANAS1st[,5:10]
PANAS_13_diff <- PANAS3rd[,5:10]-PANAS1st[,5:10]
PANAS_14_diff <- PANAS4th[,5:10]-PANAS1st[,5:10]


PANAS_12_diff$Subject <- PANAS1st$subject_id
PANAS_12_diff$Dose <- PANAS1st$Group
PANAS_13_diff$Subject <- PANAS1st$subject_id
PANAS_13_diff$Dose <- PANAS1st$Group
PANAS_14_diff$Subject <- PANAS1st$subject_id
PANAS_14_diff$Dose <- PANAS1st$Group


###HDRS merge for PANAS 1st Vis vs HDRS change corr plot
PANAS_PHQ9_Merge <- merge(PANAS_1st_Diff, D_PHQ9_PP, 
                          by.x = 'Subject', by.y = 'Subject')

PANAS_PHQ9_Corr <- PANAS_PHQ9_Merge %>% select(PHQ9_Diff, PANAS_Sadness_Difference, PANAS_Fear_Difference,
                                               PANAS_Joviality_Difference, PANAS_Attentiveness_Difference, PANAS_Fatigue_Difference,
                                               PANAS_Serenity_Difference)

colnames(PANAS_PHQ9_Corr) <- c('PHQ9 Change', 'Sadness', 'Fear', 'Joviality', 'Attentiveness', 'Fatigue', 'Serenity')

cor.data <- cor(PANAS_PHQ9_Corr)

PANAS_PHQ9 <- corrplot(cor.data, method = 'square',
         type = 'lower', diag = F,
         addCoef.col = 'black', tl.col = 'black', tl.srt = 40) 


###HDRS merge for PANAS pre-change 2-1 vs HDRS change
PANAS21_PHQ9_Merge <- merge(PANAS_14_diff, D_PHQ9_PP,
                                                by.x = 'Subject', by.y = 'Subject')

PANAS21_PHQ9_Corr <- PANAS21_PHQ9_Merge %>% select(PHQ9_Diff, PANAS_Sadness, PANAS_Fear,
                                               PANAS_Joviality, PANAS_Attentiveness, PANAS_Fatigue,
                                               PANAS_Serenity)

cor.data2 <- cor(PANAS21_PHQ9_Corr)

PANAS21_PHQ9 <- corrplot(cor.data2, method = 'square',
                       type = 'lower', diag = F,
                       addCoef.col = 'black', tl.col = 'black', tl.srt = 40)


### Create data-frame for actual analysis
df_analysis <- PANAS_3rd_Diff %>% select(Subject, Group)
df_analysis <- merge(df_analysis, D_PHQ9_PP, 
                          by.x = 'Subject', by.y = 'Subject')
df_analysis<- df_analysis %>% subset(Subject != 'PROP31')

df_analysis$PANAS_Sadness_1_diff <- PANAS_1st_Diff$PANAS_Sadness_Difference
df_analysis$PANAS_Sadness_12 <- PANAS_12_diff$PANAS_Sadness
df_analysis$PANAS_Sadness_13 <- PANAS_13_diff$PANAS_Sadness
df_analysis$PANAS_Sadness_14 <- PANAS_14_diff$PANAS_Sadness



# ### look at mid point as predictor
# PANAS_HDRS_mid_check <- merge(HDRS_mid, df_analysis, 
#                               by.x = 'subject_id', by.y = 'subject_id')




##analysis
#Difference on 1st visit

PANAS_PHQ9_lm <- lm(data = df_analysis, Week2_PHQ9 ~ Baseline_PHQ9 + PANAS_Sadness_1_diff*Group)

PANAS_1st_anova <- Anova(PANAS_PHQ9_lm)

PANAS_etasq <- eta_sq(PANAS_PHQ9_lm, partial = T)


print(PANAS_PHQ9_lm)
print(summary(PANAS_PHQ9_lm))
print(PANAS_1st_anova)
print(PANAS_etasq)

# 
# ### make sure 1st visit doesn't predict HDRS
# 
# PANAS_HDRS_midpoint <- lm (data= PANAS_HDRS_mid_check, HDRS_POST ~ HDRS_PRE + HDRS_diff_mid*Dose)
# 
# anova_check <- Anova(PANAS_HDRS_midpoint)
# 
# print(PANAS_HDRS_midpoint)
# print(summary(PANAS_HDRS_midpoint))
# print(anova_check)