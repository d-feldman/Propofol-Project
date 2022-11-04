###DEQ5 Visualization Between Doses##

library(ggplot2)

DEQ5_Dose_Vis <- ggplot(data = D1, aes (x = Group, y = Like, fill = Group)) + 
  geom_boxplot()

print(DEQ5_Dose_Vis)
