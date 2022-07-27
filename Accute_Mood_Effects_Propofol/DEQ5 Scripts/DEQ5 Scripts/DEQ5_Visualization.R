#PANAS
#visualization
library(ggplot2)
library (ggpubr)

##graphing

DEQ5_line <- ggline(D2, x= 'TX_Number', y= 'High',
                    color = 'Dose_Time', palette = 'jco', add = 'mean_se') +
  labs (title = "DEQ-5 'High' Rating") + 
  annotate("text", x =2, y= 93, label = "." , size = 8, color= 'blue') +
  annotate("text", x =3, y= 90, label = "**" , size = 6, color= 'blue') + 
  annotate("text", x =3, y= 85, label = "***" , size = 6, color= 'grey') +
  annotate("text", x =4, y= 90, label = "**" , size = 6, color= 'blue') + 
  annotate("text", x =4, y= 85, label = "***" , size = 6, color= 'grey') +
  annotate("text", x =5, y= 95, label = "." , size = 8, color= 'blue') + 
  annotate("text", x =5, y= 92, label = "." , size = 8, color= 'grey') +
  annotate("text", x =6, y= 90, label = "**" , size = 6, color= 'blue') + 
  annotate("text", x =6, y= 85, label = "**" , size = 6, color= 'grey')


##table for anova
##make table accessible
result_table <- DEQ5_anova %>% 
  mutate(across(where(is.numeric), ~ round(., digits = 6)))
result_table_anova <- ggtexttable(result_table, rows = c('Group', 'Visit', 'Interaction'),
                                  theme = ttheme('mBlue'))
result_table_anova <- result_table_anova %>%
  tab_add_title(text = 'ANOVA', face = 'bold', hjust = -4.9)

# #make pairwise visits a dataframe
# visit_pw_df <- as.data.frame(summary(DEQ5_pairwise_visit, which = 2))
# visit_pw_df <- visit_pw_df%>% 
#   mutate(across(where(is.numeric), ~ round(., digits = 4)))
# visit_pw_df <- select(visit_pw_df, -1:-2)
# names(visit_pw_df) <- c('Estimate', 'SE', 'df', 't.ratio', 'p.value')
# 
# result_table_pair <- ggtexttable(visit_pw_df, rows= c('Visit 1', 'Visit 1', 'Visit 1',
#                                                       'Visit 2', 'Visit 2', 'Visit 2',
#                                                       'Visit 3', 'Visit 3', 'Visit 3',
#                                                       'Visit 4', 'Visit 4', 'Visit 4',
#                                                       'Visit 5', 'Visit 5', 'Visit 5',
#                                                       'Visit 6', 'Visit 6', 'Visit 6'),
#                                  theme = ttheme('mBlue'))
# result_table_pair <- result_table_pair %>%
#   tab_add_title(text = 'Pairwise', face = 'bold', hjust = -3)



##arrange plots


figure <- ggarrange(DEQ5_line, result_table_anova, ncol=1, nrow=2,
                              labels = c('A','B'))


annotated_figure <- annotate_figure(figure,
    top = text_grob("DEQ5 High Post-Treatment Scores By Dose and Treatment Number",
                    color = 'black', face = 'bold', size = 14)) +
   annotate("text", x =.91, y=.22, label = "***" , size = 8, color= 'black')





print(annotated_figure)
