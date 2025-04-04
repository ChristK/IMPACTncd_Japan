library(tidyverse)

#### Analysis set ####
# diseases
dis_cat <- c("stroke", "chd")

# Sex categories
sex_cat <- c("men","women")

# years needed
years <- c(2001, 2013, 2019)

# age end
age_end <- 109

# set
set <- expand_grid(dis_cat, sex_cat, years)
set <- set %>% 
  mutate(dir_incd = paste0("work/input/", dis_cat, "_incd_for_disbayes_20over.fst"),
         dir_pop = "work/input/combined_population_japan_0_to_155_yo_afte_grouping_90over.fst",
         dir_ftlt = paste0("work/input/", dis_cat, "_ftlt_.fst"))

