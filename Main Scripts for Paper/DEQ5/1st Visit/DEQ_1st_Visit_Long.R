#DEQ5 1st Visit Long Form

library(dplyr)
library(tidyr)
library(ggplot2)

##Make into long form

D_Drug <- D1 %>% select('Subject', 'Dose', 'Drug_Effect')
D_Drug$Scale <- 'Drug Effect'
colnames(D_Drug) <- c('Subject', 'Dose', 'Rating', 'Scale')
  
D_High <- D1 %>% select('Subject', 'Dose', 'High')
D_High$Scale <- 'High'
colnames(D_High) <- c('Subject', 'Dose', 'Rating', 'Scale')

D_Like <- D1 %>% select('Subject', 'Dose', 'Like')
D_Like$Scale <- 'Like'
colnames(D_Like) <- c('Subject', 'Dose', 'Rating', 'Scale')

D_Dislike <- D1 %>% select('Subject', 'Dose', 'Dislike')
D_Dislike$Scale <- 'Dislike'
colnames(D_Dislike) <- c('Subject', 'Dose', 'Rating', 'Scale')

D_Want <- D1 %>% select('Subject', 'Dose', 'Want_More')
D_Want$Scale <- 'Want More'
colnames(D_Want) <- c('Subject', 'Dose', 'Rating', 'Scale')

D_long <- rbind(D_Drug,D_High,D_Like,D_Dislike,D_Want)

D_long$Scale <- factor(D_long$Scale, levels = c('Drug Effect', 'Like', 'Dislike', 'High', 'Want More'))




###Vis Dose
DEQ5_long_box_Dose <- ggplot(data = D_long, aes(x = Scale, y = Rating, fill = Dose)) + 
  geom_boxplot(alpha=.6) + scale_fill_manual(values = c('#3263a8', '#a8323e')) + 
  xlab('DEQ5 Scale') + ylab('DEQ5 Rating') + 
  ggtitle('                                First Propofol Anesthesia DEQ5 Post-Treatment Ratings by Dosing Group') + 
  theme( plot.title = element_text(face='bold')) + theme_bw()  


print(DEQ5_long_box_Dose)
