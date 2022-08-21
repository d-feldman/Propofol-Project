#customizable Script to build figures and tables
#Note, all wanted components have to already exist in your environment
#to utilize this tool

library(ggplot2)
library (ggpubr)


##table for anova
##make table accessible
result_table <- PANAS_anova %>% 
  mutate(across(where(is.numeric), ~ round(., digits = 6)))
result_table_anova <- ggtexttable(result_table, rows = c('Randomization', 'Visit', 'Interaction'),
                              theme = ttheme('mBlue'))
result_table_anova <- result_table_anova %>%
        tab_add_title(text = 'Anova', face = 'bold', hjust = -6)

#make pairwise visits a dataframe
visit_pw_df <- as.data.frame(summary(Panas_pairwise_visit, which = 2))
visit_pw_df <- visit_pw_df%>% 
  mutate(across(where(is.numeric), ~ round(., digits = 4)))
visit_pw_df <- select(visit_pw_df, -1:-2)
names(visit_pw_df) <- c('Estimate', 'SE', 'df', 't.ratio', 'p.value')

result_table_pair <- ggtexttable(visit_pw_df, rows= c('Visit 1', 'Visit 2', 'Visit 3',
                                                      'Visit 4', 'Visit 5', 'Visit 6'),
                                 theme = ttheme('mBlue'))
result_table_pair <- result_table_pair %>%
  tab_add_title(text = 'Pairwise', face = 'bold', hjust = -3)





##arrange plots

figure <- ggarrange(PANAS_line_1, PANAS_line_2, PANAS_line_3,
                    ncol = 1, nrow = 3,
                    labels = c('A', 'B', 'C')) + border()
                  


annotated_figure <- annotate_figure(figure,
                                    top = text_grob("PANAS Serenity Rating over Initial Propofol Treatment",
                                                    color = 'black', face = 'bold', size = 14)) + 
  border() 
 

print(annotated_figure)



