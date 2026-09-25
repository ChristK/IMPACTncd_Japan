library(fst)

#### final fst files ####
for(i in 1:nrow(set)){
  res <- read_rds(paste0("work/output/disbayes_rds/", set$dis_cat[i], "_", set$sex_cat[i], "_", set$years[i], ".rds"))
  res_prev <- res %>% broom::tidy() %>% filter(var %in% "prev_prob")
  res_prev <- res_prev %>% 
    mutate(mu = `50%`, 
           mu_lower = `2.5%`, 
           mu_upper = `97.5%`) %>% 
    dplyr::select(age, mu, mu_lower, mu_upper)
  
  d_pop <- read_fst(set$dir_pop[i]) %>% 
    dplyr::filter(year %in% set$years[i], 
                  sex %in% set$sex_cat[i]) %>% 
    dplyr::select(age, year, sex, pops)
  
  d <- left_join(d_pop, res_prev, by="age")
  d <- d %>% 
    mutate(age = age + 20, 
           N = round(pops*mu) %>% as.integer(), 
           N_lower = round(pops*mu_lower) %>% as.integer(), 
           N_upper = round(pops*mu_upper) %>% as.integer(), 
           cause_name = set$dis_cat[i], 
           measure_name = "Prevalence") %>% 
    filter(age <100) %>% 
    dplyr::select(age, year, sex, cause_name, measure_name, N, N_lower, N_upper, mu, mu_lower, mu_upper)
  
  write_fst(d, paste0("work/output/prev_fst/", set$dis_cat[i], "_", set$sex_cat[i], "_", set$years[i], ".fst"))
  
  write_csv(d, paste0("work/output/prev_csv/", set$dis_cat[i], "_", set$sex_cat[i], "_", set$years[i], ".csv"))
}