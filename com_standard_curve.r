#Standard curve test

library(ggpubr)
library(tidyverse)


standard_curve <- 
  read_delim("C:/Users/jjohn/OneDrive/MScthesis/data/qPCR/com_standard_curve.csv",",",)

ggscatter(standard_curve, x = "qty", y = "ct", add = "reg.line") +
  stat_cor(label.x = 3, label.y = 34) +
  stat_regline_equation( output.type = "text")+
  scale_x_log10()
#> `geom_smooth()` using formula 'y ~ x'

# Gettin an exacter model equation
model <- lm(ct ~ qty_log10, data = standard_curve)
model