#customizable Script to build figures and tables
#Note, all wanted components have to already exist in your environment
#to utilize this tool

library(ggplot2)
library (ggpubr)


##table for anova
##make table accessible
result_table <- BSS_anova %>% 
  mutate(across(where(is.numeric), ~ round(., digits = 4)))
result_table_anova <- ggtexttable(result_table, rows = c('Randomization', 'Visit', 'Interaction'),
                              theme = ttheme('mBlue'))
result_table_anova <- result_table_anova %>%
        tab_add_title(text = 'BSS ANOVA Results', face = 'bold', hjust = -1.5)

#make pairwise visits a dataframe
visit_pw_df <- as.data.frame(summary(BSS_pairwise_visit, which = 2))
visit_pw_df <- visit_pw_df%>% 
  mutate(across(where(is.numeric), ~ round(., digits = 4)))
visit_pw_df <- select(visit_pw_df, -1:-2)
names(visit_pw_df) <- c('Estimate', 'SE', 'df', 't.ratio', 'p.value')

result_table_pair <- ggtexttable(visit_pw_df, rows= c('Visit 1', 'Visit 2', 'Visit 3'),
                                 theme = ttheme('mBlue'))
result_table_pair <- result_table_pair %>%
  tab_add_title(text = 'BSS Pairwise Visit Results', face = 'bold', hjust = -.5)




#annotated figure
annotated_plot <- annotate_figure(BSS_line,
                top = text_grob("Beck Suicidality Scale Over 2-week Propofol Treatment",
                                color = 'black', face = 'bold', size = 14))

##arange plots

figure <- ggarrange(annotated_plot, 
                    ggarrange( result_table_anova, result_table_pair,
                   labels = c('B', 'C'),
                   ncol=2, nrow =1),
                   nrow = 2, labels = 'A') +border()

print(figure)



