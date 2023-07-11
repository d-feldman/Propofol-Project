##DEQ5 1st Visit Analysis

#libraries
library(dplyr)
library(tidyr)
library(lsr)


###Summary Stats###
# D_Sum <- group_by(D1, Group) %>%
#   summarise(count=n(), mean = mean(Like,na.rm=T),
#             sd = sd(Like, na.rm=T))

###check normality###
# D_normality <-with(D1, 
#                    D1[D1$Group == 'High_Dose'] - D1[D1$Group == 'Low_Dose'])


###group T-Test###
Gres_Drug<- t.test(Drug_Effect ~ Dose, data=D1, paired = F)
Gres_High<- t.test(High ~ Dose, data=D1, paired = F)
Gres_Like<- t.test(Like ~ Dose, data=D1, paired = F)
Gres_Dislike<- t.test(Dislike ~ Dose, data=D1, paired = F)
Gres_Want<- t.test(Want_More ~ Dose, data=D1, paired = F)

#Cohen's D
Gres_Drug<- cohensD(Drug_Effect ~ Dose, data=D1)
Gres_High<- cohensD(High ~ Dose, data=D1)
Gres_Like<- cohensD(Like ~ Dose, data=D1)
Gres_Dislike<- cohensD(Dislike ~ Dose, data=D1)
Gres_Want<- cohensD(Want_More ~ Dose, data=D1)


##print##
#print(D_Sum)
#print(shapiro.test(D_normality))


print(Gres_Drug)
print(Gres_High)
print(Gres_Like)
print(Gres_Dislike)
print(Gres_Want)