#PANAS
#visualization
library(ggplot2)
library (ggpubr)

# D_high <- D_results %>% filter(Group == 'High')
# 
# D_low <- D_results %>% filter(Group == 'Low')
# 
# D_vis_high_sad <- ggplot(data = D_results, aes(x=Visit_Number, y=PANAS_Sadness, linetype = Time, color = Dose_Time)) + geom_smooth(se=F, method = 'lm') + expand_limits(x=c(1,6), y=c(0, 60)) + scale_color_manual(values = c('#a8323e', '#a8323e','#3263a8', '#3263a8'))
# #D_vis_low_sad <- ggplot(data = D_low, aes(x=Visit_Number, y=PANAS_Sadness, linetype = Time)) + geom_smooth(se=F, method = 'loess') + expand_limits(x=c(1,6), y=c(0, 60))
# 
# print(D_vis_high_sad)
# #print(D_vis_low_sad)
# 
#  D_vis_high_jov <- ggplot(data = D_results, aes(x=Visit_Number, y=PANAS_Joviality, linetype = Time, color = Dose_Time)) + geom_smooth(se=F, method = 'lm') + expand_limits(x=c(1,6), y=c(0, 25)) + scale_color_manual(values = c('#a8323e', '#a8323e','#3263a8', '#3263a8'))
# # D_vis_low_jov <- ggplot(data = D_low, aes(x=Visit_Number, y=PANAS_Joviality, linetype = Time)) + geom_smooth(se=F, method = 'loess') + expand_limits(x=c(1,6), y=c(0, 25))
# # 
# print(D_vis_high_jov)
# # print(D_vis_low_jov)

D_results$Group <- factor(D_results$Group, levels = c('Low', 'High'))

D_results$Dose <- D_results$Group


PANAS_Sadness <- ggplot(data=D_results, aes(x=Visit_Number, y= PANAS_Sadness, color = Dose)) + geom_smooth(data = D_results, method=lm, se=T, aes(linetype = Time), size = 1.25) + 
  scale_linetype_manual(values = c("solid","dotted"),
                        name="Time", breaks=c("Pre Infusion", "Post Infusion"), 
                        labels = c("Pre Infusion", "Post Infusion")) +
   scale_color_manual(values = c('#3263a8', '#a8323e'), name="Dose", breaks=c("Low", "High"), 
 labels = c("Low Dose", "High Dose")) +  ylab(label= 'PANAS Sadness Rating') + xlab(label = "Infusion Number") + theme_bw() + 
  guides(linetype = guide_legend(override.aes = list(color = "black"), order = 2),
         color = guide_legend(order=1)) 
print(PANAS_Sadness)


PANAS_Sadness_2 <- ggline(D_results, x= 'Visit_Number', y= 'PANAS_Sadness',
                     color = 'Dose_Time', palette = 'jco', add = 'mean', size =1, point.size = 2,  shape = c ('circle', 'circle', 'triangle', 'triangle', 'circle', 'circle', 'triangle', 'triangle', 'circle', 'circle', 'triangle', 'triangle', 'circle', 'circle', 'triangle', 'triangle', 'circle', 'circle', 'triangle', 'triangle', 'circle', 'circle', 'triangle', 'triangle' ),
                     linetype = c('dashed', 'dashed', 'dashed', 'dashed', 'dashed', 'dashed', 'solid', 'solid', 'solid', 'solid', 'solid', 'solid', 'dashed', 'dashed', 'dashed', 'dashed', 'dashed', 'dashed', 'solid', 'solid', 'solid', 'solid', 'solid', 'solid')) +
  scale_color_manual(values = c('#a8323e', '#a8323e','#3263a8', '#3263a8')) +
  ylab(label= 'PANAS Sadness Rating') + xlab(label = 'Infusion Number') + theme(legend.position = 'none', text = element_text(size=18)) 

print(PANAS_Sadness_2)

#############

PANAS_line <- ggline(D_results, x= 'Visit_Number', y= 'PANAS_Fear',
                    color = 'Dose_Time', palette = 'jco', add = 'mean', size =1, point.size = 2,  shape = c ('circle', 'circle', 'triangle', 'triangle', 'circle', 'circle', 'triangle', 'triangle', 'circle', 'circle', 'triangle', 'triangle', 'circle', 'circle', 'triangle', 'triangle', 'circle', 'circle', 'triangle', 'triangle', 'circle', 'circle', 'triangle', 'triangle' ),
                    linetype = c('dashed', 'dashed', 'dashed', 'dashed', 'dashed', 'dashed', 'solid', 'solid', 'solid', 'solid', 'solid', 'solid', 'dashed', 'dashed', 'dashed', 'dashed', 'dashed', 'dashed', 'solid', 'solid', 'solid', 'solid', 'solid', 'solid')) +
  scale_color_manual(values = c('#a8323e', '#a8323e','#3263a8', '#3263a8')) +
  ylab(label= 'PANAS Fear Rating') + xlab(label = 'Infusion Number') + theme(legend.position = 'none', text = element_text(size=14)) 

  
  
#   
# PANAS_line <- ggline(D_results, x= 'Visit_Number', y= 'PANAS_Fear',
#                        color = 'Dose_Time', linetype = 'Dose_Time', add = 'mean', size =1, point.size = 2) + 
#   scale_color_manual(values = c('#a8323e', '#a8323e','#3263a8', '#3263a8'), name="Dose", breaks=c("Low_Dose_Pre", "Low_Dose_Post", "High_Dose_Pre", "High_Dose_Post"), 
#                      labels = c("High Dose", "High Dose", "Low Dose","Low Dose"))+
#   scale_linetype_manual(values = c('solid','dotted', 'solid','dotted'), 
#                         name="Time", breaks=c("Low_Dose_Pre", "Low_Dose_Post", "High_Dose_Pre", "High_Dose_Post"), 
#                         labels = c("Pre Infusion", "Post Infusion", "Pre Infusion","Post Infusion")) +
#       ylab(label= 'PANAS Joviality Rating') + xlab(label = 'Infusion Number') + theme(legend.position = 'none', text = element_text(size=14))  
#   
  
PANAS_line <- annotate_figure(PANAS_line,
                                top = text_grob("PANAS Fear Ratings Pre-Post Infusion By Dose",
                                                color = 'black', face = 'bold', size = 16))


PANAS_line_2 <- ggline(D_results, x= 'Visit_Number', y= 'PANAS_Joviality',
                     color = 'Dose_Time', palette = 'jco', add = 'mean', size =1, point.size = 2,  shape = c ('circle', 'circle', 'triangle', 'triangle', 'circle', 'circle', 'triangle', 'triangle', 'circle', 'circle', 'triangle', 'triangle', 'circle', 'circle', 'triangle', 'triangle', 'circle', 'circle', 'triangle', 'triangle', 'circle', 'circle', 'triangle', 'triangle' ),
                     linetype = c('dashed', 'dashed', 'dashed', 'dashed', 'dashed', 'dashed', 'solid', 'solid', 'solid', 'solid', 'solid', 'solid', 'dashed', 'dashed', 'dashed', 'dashed', 'dashed', 'dashed', 'solid', 'solid', 'solid', 'solid', 'solid', 'solid')) +
  scale_color_manual(values = c('#a8323e', '#a8323e','#3263a8', '#3263a8')) +
  ylab(label= 'PANAS Joviality Rating') + xlab(label = 'Infusion Number') + theme(legend.position = 'none', text = element_text(size=14))

PANAS_line_2 <- annotate_figure(PANAS_line_2,
                top = text_grob("PANAS Joviality Ratings Pre-Post Infusion By Dose",
                                color = 'black', face = 'bold', size = 16))

PANAS_line_3 <- ggline(D_results, x= 'Visit_Number', y= 'PANAS_Attentiveness',
                       color = 'Dose_Time', palette = 'jco', add = 'mean', size =1, point.size = 2,  shape = c ('circle', 'circle', 'triangle', 'triangle', 'circle', 'circle', 'triangle', 'triangle', 'circle', 'circle', 'triangle', 'triangle', 'circle', 'circle', 'triangle', 'triangle', 'circle', 'circle', 'triangle', 'triangle', 'circle', 'circle', 'triangle', 'triangle' ),
                       linetype = c('dashed', 'dashed', 'dashed', 'dashed', 'dashed', 'dashed', 'solid', 'solid', 'solid', 'solid', 'solid', 'solid', 'dashed', 'dashed', 'dashed', 'dashed', 'dashed', 'dashed', 'solid', 'solid', 'solid', 'solid', 'solid', 'solid')) +
  scale_color_manual(values = c('#a8323e', '#a8323e','#3263a8', '#3263a8')) +
  ylab(label= 'PANAS Attentiveness Rating') + xlab(label = 'Infusion Number') + theme(legend.position = 'none', text = element_text(size=14))

PANAS_line_3 <- annotate_figure(PANAS_line_3,
                                top = text_grob("PANAS Attentiveness Ratings Pre-Post Infusion By Dose",
                                                color = 'black', face = 'bold', size = 16))

PANAS_line_4 <- ggline(D_results, x= 'Visit_Number', y= 'PANAS_Fatigue',
                       color = 'Dose_Time', palette = 'jco', add = 'mean', size =1, point.size = 2,  shape = c ('circle', 'circle', 'triangle', 'triangle', 'circle', 'circle', 'triangle', 'triangle', 'circle', 'circle', 'triangle', 'triangle', 'circle', 'circle', 'triangle', 'triangle', 'circle', 'circle', 'triangle', 'triangle', 'circle', 'circle', 'triangle', 'triangle' ),
                       linetype = c('dashed', 'dashed', 'dashed', 'dashed', 'dashed', 'dashed', 'solid', 'solid', 'solid', 'solid', 'solid', 'solid', 'dashed', 'dashed', 'dashed', 'dashed', 'dashed', 'dashed', 'solid', 'solid', 'solid', 'solid', 'solid', 'solid')) +
  scale_color_manual(values = c('#a8323e', '#a8323e','#3263a8', '#3263a8')) +
  ylab(label= 'PANAS Fatigue Rating') + xlab(label = 'Infusion Number') + theme(legend.position = 'none', text = element_text(size=14))

PANAS_line_4 <- annotate_figure(PANAS_line_4,
                                top = text_grob("PANAS Fatigue Ratings Pre-Post Infusion By Dose",
                                                color = 'black', face = 'bold', size = 16))

PANAS_line_5 <- ggline(D_results, x= 'Visit_Number', y= 'PANAS_Serenity',
                       color = 'Dose_Time', palette = 'jco', add = 'mean', size =1, point.size = 2,  shape = c ('circle', 'circle', 'triangle', 'triangle', 'circle', 'circle', 'triangle', 'triangle', 'circle', 'circle', 'triangle', 'triangle', 'circle', 'circle', 'triangle', 'triangle', 'circle', 'circle', 'triangle', 'triangle', 'circle', 'circle', 'triangle', 'triangle' ),
                       linetype = c('dashed', 'dashed', 'dashed', 'dashed', 'dashed', 'dashed', 'solid', 'solid', 'solid', 'solid', 'solid', 'solid', 'dashed', 'dashed', 'dashed', 'dashed', 'dashed', 'dashed', 'solid', 'solid', 'solid', 'solid', 'solid', 'solid')) +
  scale_color_manual(values = c('#a8323e', '#a8323e','#3263a8', '#3263a8')) +
  ylab(label= 'PANAS Serenity Rating') + xlab(label = 'Infusion Number') + theme(legend.position = 'none', text = element_text(size=14))

PANAS_line_5 <- annotate_figure(PANAS_line_5,
                                top = text_grob("PANAS Serenity Ratings Pre-Post Infusion By Dose",
                                                color = 'black', face = 'bold', size = 16))


#figure
PANAS_line_comb <- ggarrange(PANAS_line, PANAS_line_2, PANAS_line_3, PANAS_line_4, PANAS_line_5,
                             ncol=2, nrow=3,
                             labels = c('a','b','c','d','e'))
# 
# PANAS_line_comb_1 <- annotate_figure(PANAS_line_comb,
#                                    top = text_grob("Dose Moderation of Propofol Infusion Mood Induction over Treatment Course",
#                                                    color = 'black', face = c('bold'), size = 18)) + theme_bw()

print(PANAS_line_comb)



#print(cp)
# 
# PANAS_line3 <- ggline(D_results, x= 'Visit_Number', y= 'PANAS_Sadness',
#                       add = 'mean', size =1, point.size = 2,
#                      linetype = 'Time') +
#   labs (title = '               Sadness PANAS Rattings Pre-Post Treatment Scores By Dose')  +
#   ylab(label= 'PANAS Sadness Rating') + xlab(label = 'Treatment Number')
# 
# print(PANAS_line3)