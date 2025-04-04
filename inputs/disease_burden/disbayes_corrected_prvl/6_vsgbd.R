#### Comparison of disbayes prevalence with GBD prevalence ####
for(i in 1:nrow(set)){
  db <- read_fst(paste0("work/output/prev_fst/", set$dis_cat[i], "_", set$sex_cat[i], "_", set$years[i], ".fst"))
  db <- db %>% mutate(type = "disbayes") %>% dplyr::filter(age >=30)
  
  # d_pop <- read_fst(set$dir_pop[i])
  # d_pop <- d_pop %>% 
  #   dplyr::filter(year %in% set$years[i], 
  #                 sex %in% set$sex_cat[i]) %>% 
  #   dplyr::filter(age >=30, age <100)
  
  gbd <- read_fst(paste0("work/input/",set$dis_cat[i],"_prvl_for_disbayes_20over.fst"))
  gbd <- gbd %>% 
    dplyr::filter(sex == set$sex_cat[i], year == set$years[i]) %>% 
    filter(age>=30, age<100) %>% 
    mutate(type = "GBD")
  
  d <- rbind(db, gbd)
  
  d %>% 
    ggplot() + 
    geom_point(aes(x=age, y=mu, color=type)) + 
    labs(title = paste0(set$dis_cat[i], "_", set$sex_cat[i], "_", set$years[i]), 
         y = "prevalence")
  
  ggsave(paste0("work/output/vsgbd/", set$dis_cat[i], "_", set$sex_cat[i], "_", set$years[i], ".jpg"),
         width = 5, height = 4)
}
