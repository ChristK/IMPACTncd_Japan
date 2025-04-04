#### Plots for median prevalence rates and checking Rhat ####
for(i in 1:nrow(set)){
  res <- read_rds(paste0("work/output/disbayes_rds/", set$dis_cat[i], "_", set$sex_cat[i], "_", set$years[i], ".rds"))
  
  res_prev <- res %>% broom::tidy() %>% filter(var %in% "prev_prob")
  
  rhat1.1 <- any(res_prev$Rhat >=1.1, na.rm = T)
  
  p <- res_prev %>% 
    ggplot() + 
    geom_point(aes(x=age+20,y=`50%`)) + 
    annotate("text", x=50, y=0.2, label=paste0("any Rhat >=1.1: ", rhat1.1)) + 
    labs(title = paste0(set$dis_cat[i], "_", set$sex_cat[i], "_", set$years[i]), 
         x = "age",
         y = "median prevalence")
  
  ggsave(paste0("work/output/plot_prev/", set$dis_cat[i], "_", set$sex_cat[i], "_", set$years[i], ".jpg"),
         width = 4, height = 4)
}