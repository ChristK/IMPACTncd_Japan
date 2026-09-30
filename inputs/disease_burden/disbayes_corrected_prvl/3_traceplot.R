library(tidyverse)

#### checking traceplots ####
for(i in 1:nrow(set)){
  res <- read_rds(paste0("work/output/disbayes_rds/", set$dis_cat[i], "_", set$sex_cat[i], "_", set$years[i], ".rds"))
  
  p <- rstan::traceplot(res$fit, 
                        pars=paste0("prev_prob[",10*1:9,",1]"), 
                        inc_warmup=T) + theme_minimal()
  
  ggsave(paste0("work/output/traceplot/", set$dis_cat[i], "_", set$sex_cat[i], "_", set$years[i], ".jpg"),
         plot = p,
         width = 8, height = 8)
}
