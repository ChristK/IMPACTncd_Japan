


##########################################
###### Read Me 
######
###### Run program named "simulate.r" before conducting this file,  
##########################################


source("./global.R")




##########################################
######### check output
##########################################

##############################
### inputs/synthpop
##############################
# synthetic population was contained.




##############################
### /outputs/lifecourse
##############################

hoge1 <- c()

for(iii in 1:100){
	tryCatch({
		# procedure when warning or error does not happen
		temp <- fread(paste0("./outputs/lifecourse/", iii, "_lifecourse.csv.gz"))
		temp[, FROM := iii, ]
		hoge1 <- rbind(hoge1, temp)
		print(paste0("readable_", iii))

	
	}, warning = function(w) {
		# procedures when warning happens
		print(paste0("warning_", iii))
	
	}, error = function(e) {
		# procedures when error happens
		print(paste0("error_", iii))
		
	}, finally = {
		# procedure regardless of correctness
		NULL		
	})
}


hoge1



setdiff(1:100, hoge1[, unique(mc), ])




#pid = individual id
#pid_mrk = start point for each individual
#prvl = years from true prevalence 
#dgns = years from diagnosed or not 
	# difference between prvl and dgns exist. but there were no difference in the current IMPACT NCD-Japan on July 20th in 2023
#wt wt for japanese projection population
#wt_esp for Japanese standard population
#mc for Nth of MC simulation.
#scenario = scenario

#all_cause_mrtl: coding rule is described "mortality: code" in the yaml file.
# - name: chd
#   friendly_name: CHD
#   meta:
#     incidence:
#       type: 2 # it depends on exposure and relative risk
#       can_recur: no
#     diagnosis:
#       type: 1
#       probability: 1
#       duration_distr: NBI
#       mm_wt: 0.49
#     mortality:
#       type: 2
#       code: 2
#   notes: .na
# - name: stroke
#   friendly_name: Stroke
#   meta:
#     incidence:
#       type: 2
#       can_recur: no
#     diagnosis:
#       type: 1
#       probability: 1
#       duration_distr: PIG
#       mm_wt: 0.8
#     mortality:
#       type: 2
#       code: 3
#   notes: .na









###########################
###outputs/summaries
###########################

# grouped results by mc, scenario, year, age-group, sex 
# esp = Japanese standard population
# scaled = Japanese projection population
# incd = incidence
# prvl = prevalence
# mrtl = mortality 
# popsize = N
# prvl = N but not (%)



HEIGHT <- 5
WIDTH <- 10

DATA <- c(
	"prvl_scaled_up.csv.gz"
	, "incd_scaled_up.csv.gz"
	#, "incd_esp.csv.gz"
	#, "prvl_esp.csv.gz"
)


DIS <- c( 
	"chd"
	, "stroke"
	# , "cvd_prvl"
	# , "obesity_prvl"
	# , "htn_prvl"
	# , "t2dm_prvl"
	# , "cms1st_cont_prvl"
	# , "cmsmm0_prvl"
	# , "cmsmm1_prvl"
	# , "cmsmm1.5_prvl"
	# , "cmsmm2_prvl"
)

LOOP <- CJ(DATA = DATA, DIS = DIS)

LOOP[, DIS := paste0(DIS, "_", substr(DATA, 1, 4)), ]

LOOP[, DATA_GBD := 
	ifelse(DIS == "chd_incd", "chd_incd.fst",
	ifelse(DIS == "chd_prvl", "chd_prvl.fst",
	ifelse(DIS == "stroke_incd", "stroke_incd.fst",
	ifelse(DIS == "stroke_prvl", "stroke_prvl.fst",
	NA)))), ]





for(iii in 1:nrow(LOOP)){
	tryCatch({
		
		### impact ncd per 5 yo
		data_sum <- fread(paste0("./outputs/summaries/", LOOP[iii, DATA]))
		data_sum$temp <- data_sum[, LOOP[iii, DIS], with = FALSE]
			
			data_sum[, unique(mc) %>% length(.), ]  
			data_sum[, unique(scenario) %>% length(.), ]
			data_sum[, max(year) - min(year) + 1, ] 
			data_sum[, unique(agegrp) %>% length(.), ]
			data_sum[, unique(sex) %>% length(.), ] 
			nrow(data_sum)
			
			
		
		data_sum_agg <- data_sum[, .(
			rate_impact_ncd_med = quantile(temp/popsize, p = 0.500),
			rate_impact_ncd_low = quantile(temp/popsize, p = 0.025),
			rate_impact_ncd_upp = quantile(temp/popsize, p = 0.975),
			
			n_impact_ncd_med = quantile(temp, p = 0.500),	
			n_impact_ncd_low = quantile(temp, p = 0.025),	
			n_impact_ncd_upp = quantile(temp, p = 0.975)			
			), 
			by = c("scenario", "year", "agegrp", "sex")]
					
				data_sum_agg[, unique(scenario) %>% length(.), ] 
				data_sum_agg[, max(year) - min(year) + 1, ] 
				data_sum_agg[, unique(agegrp) %>% length(.), ]
				data_sum_agg[, unique(sex) %>% length(.), ] 
				nrow(data_sum_agg)
				
		
		
		### gbd per 1 yo
		data_gbd <- read_fst(paste0("./inputs/disease_burden/", LOOP[iii, DATA_GBD]), as.data.table = TRUE)
				data_gbd[, max(year) - min(year) + 1, ] 
				data_gbd[, unique(age) %>% length(.), ]
				data_gbd[, unique(sex) %>% length(.), ] 
				nrow(data_gbd)
		
		
		### pop per 1 yo
		data_pop <- read_fst("./inputs/pop_projections/combined_population_japan.fst", as.data.table = TRUE)
			
			
		### gbd + pop
		data_gbd_pop <- merge(
			x = data_gbd,
			y = data_pop[, list(age, year, sex, pops), ],
			by = c("age", "year", "sex"),
			all.x = TRUE
			)
			
			nrow(data_gbd)
			nrow(data_pop)
			nrow(data_gbd_pop)			
			

		data_gbd_pop[, agegrp :=
			ifelse(between(age, 5* 6, 5* 6+4), "30-34", 
			ifelse(between(age, 5* 7, 5* 7+4), "35-39",
			ifelse(between(age, 5* 8, 5* 8+4), "40-44",
			ifelse(between(age, 5* 9, 5* 9+4), "45-49",
			ifelse(between(age, 5*10, 5*10+4), "50-54",
			ifelse(between(age, 5*11, 5*11+4), "55-59",
			ifelse(between(age, 5*12, 5*12+4), "60-64",
			ifelse(between(age, 5*13, 5*13+4), "65-69",
			ifelse(between(age, 5*14, 5*14+4), "70-74",
			ifelse(between(age, 5*15, 5*15+4), "75-79",
			ifelse(between(age, 5*16, 5*16+4), "80-84",
			ifelse(between(age, 5*17, 5*17+4), "85-89",
			ifelse(between(age, 5*18, 5*18+4), "90-94",
			ifelse(between(age, 5*19, 5*19+4), "95-99",
			NA)))))))))))))), ]
			
			a <- data_gbd_pop[, table(age, agegrp, useNA = "always"), ]
			a[1:50, ]
			a[51:nrow(a), ]			


		data_gbd_pop[, ":="(
			N       = mu * pops,
			N_lower = mu_lower * pops,
			N_upper = mu_upper * pops
			), ]

#		data_gbd_pop[, ":="(
#			N2       = mu * pops,
#			N_lower2 = mu_lower * pops,
#			N_upper2 = mu_upper * pops
#			), ]
#			
#		data_gbd_pop[, table(round(N) == round(N2)), ]
#		data_gbd_pop[, table(round(N_lower) == round(N_lower2)), ]
#		data_gbd_pop[, table(round(N_upper) == round(N_upper2)), ]

			
		### gbd + pop per 5 yo			
		data_gbd_agg <- data_gbd_pop[, .(
			n_gbd_med = sum(N),
			n_gbd_low = sum(N_lower),
			n_gbd_upp = sum(N_upper),
			pops = sum(pops)			
			), by = c("year", "agegrp", "sex")]


		data_gbd_agg[, ":="(
			rate_gbd_med = n_gbd_med/pops,
			rate_gbd_low = n_gbd_low/pops,
			rate_gbd_upp = n_gbd_upp/pops
			), ]
			
			data_gbd_agg[, max(year) - min(year) + 1, ] 
			data_gbd_agg[, unique(agegrp) %>% length(.), ]
			data_gbd_agg[, unique(sex) %>% length(.), ] 
			nrow(data_gbd_agg)
	
	
	
		### plot men
		data_sum_agg_sel <- data_sum_agg[sex == "men", , ]
		data_gbd_agg_sel <- data_gbd_agg[sex == "men", , ]
		
		#### rate
		ggplot() + 
			geom_line(data = data_gbd_agg_sel, aes(x = year, y = rate_gbd_med), color = "black") + 
			geom_line(data = data_gbd_agg_sel, aes(x = year, y = rate_gbd_low), color = "black", linetype = "dashed") + 
			geom_line(data = data_gbd_agg_sel, aes(x = year, y = rate_gbd_upp), color = "black", linetype = "dashed") + 
			
			geom_line(data = data_sum_agg_sel, aes(x = year, y = rate_impact_ncd_med), color = "red") + 
			geom_line(data = data_sum_agg_sel, aes(x = year, y = rate_impact_ncd_low), color = "red", linetype = "dashed") + 
			geom_line(data = data_sum_agg_sel, aes(x = year, y = rate_impact_ncd_upp), color = "red", linetype = "dashed") + 
			
			facet_wrap(. ~ factor(agegrp), scales="free_y") +
			theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) +
			ylab(LOOP[iii, DIS]) + xlab("Year") + ggtitle("Rate for men")
			
		ggsave(paste0(
			"./outputs/plots/plot_rate_impactNcd_vs_GBD_",
			gsub(".csv.gz", "", LOOP[iii, DATA]),
			"_",
			LOOP[iii, DIS],
			"_men.png"
			), height = HEIGHT, width = WIDTH)
		
		
		#### N		
		ggplot() + 
			geom_line(data = data_gbd_agg_sel, aes(x = year, y = n_gbd_med), color = "black") + 
			geom_line(data = data_gbd_agg_sel, aes(x = year, y = n_gbd_low), color = "black", linetype = "dashed") + 
			geom_line(data = data_gbd_agg_sel, aes(x = year, y = n_gbd_upp), color = "black", linetype = "dashed") + 
			
			geom_line(data = data_sum_agg_sel, aes(x = year, y = n_impact_ncd_med), color = "red") + 
			geom_line(data = data_sum_agg_sel, aes(x = year, y = n_impact_ncd_low), color = "red", linetype = "dashed") + 
			geom_line(data = data_sum_agg_sel, aes(x = year, y = n_impact_ncd_upp), color = "red", linetype = "dashed") + 
			
			facet_wrap(. ~ factor(agegrp), scales="free_y") +
			theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) +
			ylab(LOOP[iii, DIS]) + xlab("Year") + ggtitle("N for men")
			
		ggsave(paste0(
			"./outputs/plots/plot_N_impactNcd_vs_GBD_",
			gsub(".csv.gz", "", LOOP[iii, DATA]),
			"_",
			LOOP[iii, DIS],
			"_men.png"
			), height = HEIGHT, width = WIDTH)	
			
			



		### plot women
		data_sum_agg_sel <- data_sum_agg[sex == "women", , ]
		data_gbd_agg_sel <- data_gbd_agg[sex == "women", , ]
		
		#### rate
		ggplot() + 
			geom_line(data = data_gbd_agg_sel, aes(x = year, y = rate_gbd_med), color = "black") + 
			geom_line(data = data_gbd_agg_sel, aes(x = year, y = rate_gbd_low), color = "black", linetype = "dashed") + 
			geom_line(data = data_gbd_agg_sel, aes(x = year, y = rate_gbd_upp), color = "black", linetype = "dashed") + 
			
			geom_line(data = data_sum_agg_sel, aes(x = year, y = rate_impact_ncd_med), color = "red") + 
			geom_line(data = data_sum_agg_sel, aes(x = year, y = rate_impact_ncd_low), color = "red", linetype = "dashed") + 
			geom_line(data = data_sum_agg_sel, aes(x = year, y = rate_impact_ncd_upp), color = "red", linetype = "dashed") + 
			
			facet_wrap(. ~ factor(agegrp), scales="free_y") +
			theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) +
			ylab(LOOP[iii, DIS]) + xlab("Year") + ggtitle("Rate for women")
			
		ggsave(paste0(
			"./outputs/plots/plot_rate_impactNcd_vs_GBD_",
			gsub(".csv.gz", "", LOOP[iii, DATA]),
			"_",
			LOOP[iii, DIS],
			"_women.png"
			), height = HEIGHT, width = WIDTH)
		
		
		#### N		
		ggplot() + 
			geom_line(data = data_gbd_agg_sel, aes(x = year, y = n_gbd_med), color = "black") + 
			geom_line(data = data_gbd_agg_sel, aes(x = year, y = n_gbd_low), color = "black", linetype = "dashed") + 
			geom_line(data = data_gbd_agg_sel, aes(x = year, y = n_gbd_upp), color = "black", linetype = "dashed") + 
			
			geom_line(data = data_sum_agg_sel, aes(x = year, y = n_impact_ncd_med), color = "red") + 
			geom_line(data = data_sum_agg_sel, aes(x = year, y = n_impact_ncd_low), color = "red", linetype = "dashed") + 
			geom_line(data = data_sum_agg_sel, aes(x = year, y = n_impact_ncd_upp), color = "red", linetype = "dashed") + 
			
			facet_wrap(. ~ factor(agegrp), scales="free_y") +
			theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) +
			ylab(LOOP[iii, DIS]) + xlab("Year") + ggtitle("N for women")
			
		ggsave(paste0(
			"./outputs/plots/plot_N_impactNcd_vs_GBD_",
			gsub(".csv.gz", "", LOOP[iii, DATA]),
			"_",
			LOOP[iii, DIS],
			"_women.png"
			), height = HEIGHT, width = WIDTH)				
			

	}, warning = function(w) {
		# procedures when warning happens
		print(paste0("warning_", iii, "_", gsub(".csv.gz", "", LOOP[iii, DATA]), "_", LOOP[iii, DIS]))
	
	}, error = function(e) {
		# procedures when error happens
		print(paste0("error_", iii, "_", gsub(".csv.gz", "", LOOP[iii, DATA]), "_", LOOP[iii, DIS]))
		
	}, finally = {
		# procedure regardless of correctness
		NULL		
	})


}

















HEIGHT <- 5
WIDTH <- 10

DATA <- c(
	"dis_mrtl_scaled_up.csv.gz"
	#, "dis_mrtl_esp.csv.gz"
)


DIS <- c( 
	"nonmodelled_deaths"
)

LOOP <- CJ(DATA = DATA, DIS = DIS)

#LOOP[, DIS := paste0(DIS, "_", substr(DATA, 1, 4)), ]

LOOP[, DATA_GBD := "nonmodelled_ftlt.fst", ]

LOOP




for(iii in 1:nrow(LOOP)){
	tryCatch({
		
		
		### impact ncd per 5 yo
		data_sum <- fread(paste0("./outputs/summaries/", LOOP[iii, DATA]))
		data_sum$temp <- data_sum[, LOOP[iii, DIS], with = FALSE]
			data_sum[, unique(mc) %>% length(.), ] 
			data_sum[, unique(scenario) %>% length(.), ]
			data_sum[, max(year) - min(year) + 1, ] 
			data_sum[, unique(agegrp) %>% length(.), ]
			data_sum[, unique(sex) %>% length(.), ] 
			nrow(data_sum)
			
		
		data_sum_agg <- data_sum[, .(
			rate_impact_ncd_med = quantile(temp/popsize, p = 0.500),
			rate_impact_ncd_low = quantile(temp/popsize, p = 0.025),
			rate_impact_ncd_upp = quantile(temp/popsize, p = 0.975),
			
			n_impact_ncd_med = quantile(temp, p = 0.500),	
			n_impact_ncd_low = quantile(temp, p = 0.025),	
			n_impact_ncd_upp = quantile(temp, p = 0.975)	
			), 
			by = c("scenario", "year", "agegrp", "sex")]
			
				data_sum_agg[, unique(scenario) %>% length(.), ]
				data_sum_agg[, max(year) - min(year) + 1, ] 
				data_sum_agg[, unique(agegrp) %>% length(.), ] 
				data_sum_agg[, unique(sex) %>% length(.), ] 
				nrow(data_sum_agg)
				
		
		
		### gbd per 1 yo
		data_gbd <- read_fst(paste0("./inputs/disease_burden/", LOOP[iii, DATA_GBD]), as.data.table = TRUE)
				data_gbd[, max(year) - min(year) + 1, ] 
				data_gbd[, unique(age) %>% length(.), ]
				data_gbd[, unique(sex) %>% length(.), ] 
				nrow(data_gbd)
	

		### pop per 1 yo
		data_pop <- read_fst("./inputs/pop_projections/combined_population_japan.fst", as.data.table = TRUE)
			
			
		### gbd + pop per 1 yo
		data_gbd_pop <- merge(
			x = data_gbd,
			y = data_pop[, list(age, year, sex, pops), ],
			by = c("age", "year", "sex"),
			all.x = TRUE
			)
			
			nrow(data_gbd)
			nrow(data_pop)
			nrow(data_gbd_pop)
			
			
		data_gbd_pop[, agegrp :=
			ifelse(between(age, 5* 6, 5* 6+4), "30-34", 
			ifelse(between(age, 5* 7, 5* 7+4), "35-39",
			ifelse(between(age, 5* 8, 5* 8+4), "40-44",
			ifelse(between(age, 5* 9, 5* 9+4), "45-49",
			ifelse(between(age, 5*10, 5*10+4), "50-54",
			ifelse(between(age, 5*11, 5*11+4), "55-59",
			ifelse(between(age, 5*12, 5*12+4), "60-64",
			ifelse(between(age, 5*13, 5*13+4), "65-69",
			ifelse(between(age, 5*14, 5*14+4), "70-74",
			ifelse(between(age, 5*15, 5*15+4), "75-79",
			ifelse(between(age, 5*16, 5*16+4), "80-84",
			ifelse(between(age, 5*17, 5*17+4), "85-89",
			ifelse(between(age, 5*18, 5*18+4), "90-94",
			ifelse(between(age, 5*19, 5*19+4), "95-99",
			NA)))))))))))))), ]
			
			a <- data_gbd_pop[, table(age, agegrp, useNA = "always"), ]
			a[1:50, ]
			a[51:nrow(a), ]	
			
			
		data_gbd_pop[, ":="(
			N       = mu2 * pops,
			N_lower = mu_lower * pops,
			N_upper = mu_upper * pops
			), ]
		
		
		
		### gbd + pop per 5 yo
		data_gbd_agg <- data_gbd_pop[, .(
			n_gbd_med = sum(N),
			n_gbd_low = sum(N_lower),
			n_gbd_upp = sum(N_upper),
			pops = sum(pops)			
			), by = c("year", "agegrp", "sex")]
			
			

		data_gbd_agg[, ":="(
			rate_gbd_med = n_gbd_med/pops,
			rate_gbd_low = n_gbd_low/pops,
			rate_gbd_upp = n_gbd_upp/pops
			), ]
			
			data_gbd_agg[, max(year) - min(year) + 1, ] 
			data_gbd_agg[, unique(agegrp) %>% length(.), ]
			data_gbd_agg[, unique(sex) %>% length(.), ] 
			nrow(data_gbd_agg)
		
		
		
		
		
		### plot men
		data_sum_agg_sel <- data_sum_agg[sex == "men", , ]
		data_gbd_agg_sel <- data_gbd_agg[sex == "men", , ]
		
		#### rate
		ggplot() + 
			geom_line(data = data_gbd_agg_sel, aes(x = year, y = rate_gbd_med), color = "black") + 
			geom_line(data = data_gbd_agg_sel, aes(x = year, y = rate_gbd_low), color = "black", linetype = "dashed") + 
			geom_line(data = data_gbd_agg_sel, aes(x = year, y = rate_gbd_upp), color = "black", linetype = "dashed") + 
			
			geom_line(data = data_sum_agg_sel, aes(x = year, y = rate_impact_ncd_med), color = "red") + 
			geom_line(data = data_sum_agg_sel, aes(x = year, y = rate_impact_ncd_low), color = "red", linetype = "dashed") + 
			geom_line(data = data_sum_agg_sel, aes(x = year, y = rate_impact_ncd_upp), color = "red", linetype = "dashed") + 
			
			facet_wrap(. ~ factor(agegrp), scales="free_y") +
			theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) +
			ylab(LOOP[iii, DIS]) + xlab("Year") + ggtitle("Rate for men")
			
		ggsave(paste0(
			"./outputs/plots/plot_rate_impactNcd_vs_GBD_",
			gsub(".csv.gz", "", LOOP[iii, DATA]),
			"_",
			LOOP[iii, DIS],
			"_men.png"
			), height = HEIGHT, width = WIDTH)
		
		
		#### N		
		ggplot() + 
			geom_line(data = data_gbd_agg_sel, aes(x = year, y = n_gbd_med), color = "black") + 
			geom_line(data = data_gbd_agg_sel, aes(x = year, y = n_gbd_low), color = "black", linetype = "dashed") + 
			geom_line(data = data_gbd_agg_sel, aes(x = year, y = n_gbd_upp), color = "black", linetype = "dashed") + 
			
			geom_line(data = data_sum_agg_sel, aes(x = year, y = n_impact_ncd_med), color = "red") + 
			geom_line(data = data_sum_agg_sel, aes(x = year, y = n_impact_ncd_low), color = "red", linetype = "dashed") + 
			geom_line(data = data_sum_agg_sel, aes(x = year, y = n_impact_ncd_upp), color = "red", linetype = "dashed") + 
			
			facet_wrap(. ~ factor(agegrp), scales="free_y") +
			theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) +
			ylab(LOOP[iii, DIS]) + xlab("Year") + ggtitle("N for men")
			
		ggsave(paste0(
			"./outputs/plots/plot_N_impactNcd_vs_GBD_",
			gsub(".csv.gz", "", LOOP[iii, DATA]),
			"_",
			LOOP[iii, DIS],
			"_men.png"
			), height = HEIGHT, width = WIDTH)	
			
			



		### plot women
		data_sum_agg_sel <- data_sum_agg[sex == "women", , ]
		data_gbd_agg_sel <- data_gbd_agg[sex == "women", , ]
		
		#### rate
		ggplot() + 
			geom_line(data = data_gbd_agg_sel, aes(x = year, y = rate_gbd_med), color = "black") + 
			geom_line(data = data_gbd_agg_sel, aes(x = year, y = rate_gbd_low), color = "black", linetype = "dashed") + 
			geom_line(data = data_gbd_agg_sel, aes(x = year, y = rate_gbd_upp), color = "black", linetype = "dashed") + 
			
			geom_line(data = data_sum_agg_sel, aes(x = year, y = rate_impact_ncd_med), color = "red") + 
			geom_line(data = data_sum_agg_sel, aes(x = year, y = rate_impact_ncd_low), color = "red", linetype = "dashed") + 
			geom_line(data = data_sum_agg_sel, aes(x = year, y = rate_impact_ncd_upp), color = "red", linetype = "dashed") + 
			
			facet_wrap(. ~ factor(agegrp), scales="free_y") +
			theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) +
			ylab(LOOP[iii, DIS]) + xlab("Year") + ggtitle("Rate for women")
			
		ggsave(paste0(
			"./outputs/plots/plot_rate_impactNcd_vs_GBD_",
			gsub(".csv.gz", "", LOOP[iii, DATA]),
			"_",
			LOOP[iii, DIS],
			"_women.png"
			), height = HEIGHT, width = WIDTH)
		
		
		#### N		
		ggplot() + 
			geom_line(data = data_gbd_agg_sel, aes(x = year, y = n_gbd_med), color = "black") + 
			geom_line(data = data_gbd_agg_sel, aes(x = year, y = n_gbd_low), color = "black", linetype = "dashed") + 
			geom_line(data = data_gbd_agg_sel, aes(x = year, y = n_gbd_upp), color = "black", linetype = "dashed") + 
			
			geom_line(data = data_sum_agg_sel, aes(x = year, y = n_impact_ncd_med), color = "red") + 
			geom_line(data = data_sum_agg_sel, aes(x = year, y = n_impact_ncd_low), color = "red", linetype = "dashed") + 
			geom_line(data = data_sum_agg_sel, aes(x = year, y = n_impact_ncd_upp), color = "red", linetype = "dashed") + 
			
			facet_wrap(. ~ factor(agegrp), scales="free_y") +
			theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) +
			ylab(LOOP[iii, DIS]) + xlab("Year") + ggtitle("N for women")
			
		ggsave(paste0(
			"./outputs/plots/plot_N_impactNcd_vs_GBD_",
			gsub(".csv.gz", "", LOOP[iii, DATA]),
			"_",
			LOOP[iii, DIS],
			"_women.png"
			), height = HEIGHT, width = WIDTH)				
			

	}, warning = function(w) {
		# procedures when warning happens
		print(paste0("warning_", iii, "_", gsub(".csv.gz", "", LOOP[iii, DATA]), "_", LOOP[iii, DIS]))
	
	}, error = function(e) {
		# procedures when error happens
		print(paste0("error_", iii, "_", gsub(".csv.gz", "", LOOP[iii, DATA]), "_", LOOP[iii, DIS]))
		
	}, finally = {
		# procedure regardless of correctness
		NULL		
	})


}










HEIGHT <- 5
WIDTH <- 10

DATA <- c(
	"all_cause_mrtl_by_dis_scaled_up.csv.gz"
	#, "all_cause_mrtl_by_dis_esp.csv.gz"
)


DIS <- c( 
	"deaths_chd"
	, "deaths_stroke"
	# , "cvd_prvl"
	# , "obesity_prvl"
	# , "htn_prvl"
	# , "t2dm_prvl"
	# , "cms1st_cont_prvl"
	# , "cmsmm0_prvl"
	# , "cmsmm1_prvl"
	# , "cmsmm1.5_prvl"
	# , "cmsmm2_prvl"
)

LOOP <- CJ(DATA = DATA, DIS = DIS)

#LOOP[, DIS := paste0(DIS, "_", substr(DATA, 1, 4)), ]

LOOP[, DATA_GBD := 
	ifelse(DIS == "deaths_chd", "chd_ftlt.fst",
	ifelse(DIS == "deaths_stroke", "stroke_ftlt.fst",
	NA)), ]

LOOP




for(iii in 1:nrow(LOOP)){
	tryCatch({
		
		
		### pop per 1 yo
		data_pop <- read_fst("./inputs/pop_projections/combined_population_japan.fst", as.data.table = TRUE)
		
		
		### pop per 5 yo
		data_pop[, agegrp :=
			ifelse(between(age, 5* 6, 5* 6+4), "30-34", 
			ifelse(between(age, 5* 7, 5* 7+4), "35-39",
			ifelse(between(age, 5* 8, 5* 8+4), "40-44",
			ifelse(between(age, 5* 9, 5* 9+4), "45-49",
			ifelse(between(age, 5*10, 5*10+4), "50-54",
			ifelse(between(age, 5*11, 5*11+4), "55-59",
			ifelse(between(age, 5*12, 5*12+4), "60-64",
			ifelse(between(age, 5*13, 5*13+4), "65-69",
			ifelse(between(age, 5*14, 5*14+4), "70-74",
			ifelse(between(age, 5*15, 5*15+4), "75-79",
			ifelse(between(age, 5*16, 5*16+4), "80-84",
			ifelse(between(age, 5*17, 5*17+4), "85-89",
			ifelse(between(age, 5*18, 5*18+4), "90-94",
			ifelse(between(age, 5*19, 5*19+4), "95-99",
			NA)))))))))))))), ]
				
				a <- data_pop[, table(age, agegrp, useNA = "always"), ]
				a[1:50, ]
				a[51:nrow(a), ]			
			
			
		data_pop_agg <- data_pop[, .(pops = sum(pops)), by = c("year", "agegrp", "sex")]
		data_pop_agg <- na.omit(data_pop_agg)
			data_pop_agg[, max(year) - min(year) + 1, ] 
			data_pop_agg[, unique(agegrp) %>% length(.), ]
			data_pop_agg[, unique(sex) %>% length(.), ] 
			nrow(data_pop_agg)
		
		
		
		
		
		### impact ncd per 5 yo
		data_sum <- fread(paste0("./outputs/summaries/", LOOP[iii, DATA]))
		data_sum$temp <- data_sum[, LOOP[iii, DIS], with = FALSE]
			
			data_sum[, unique(mc) %>% length(.), ]  
			data_sum[, unique(scenario) %>% length(.), ]
			data_sum[, max(year) - min(year) + 1, ] 
			data_sum[, unique(agegrp) %>% length(.), ]
			data_sum[, unique(sex) %>% length(.), ] 
			nrow(data_sum)
			
			
		data_sum <- merge(
			x = data_sum, 
			y = data_pop_agg[ , list(agegrp, year, sex, pops), ],
			by = c("year", "agegrp", "sex"), 
			all.x = TRUE
			)
			nrow(data_sum)		
		setnames(data_sum, "pops", "popsize")
	
		
		
		
		data_sum_agg <- data_sum[, .(
			rate_impact_ncd_med = quantile(temp/popsize, p = 0.500),
			rate_impact_ncd_low = quantile(temp/popsize, p = 0.025),
			rate_impact_ncd_upp = quantile(temp/popsize, p = 0.975),
			
			n_impact_ncd_med = quantile(temp, p = 0.500),	
			n_impact_ncd_low = quantile(temp, p = 0.025),	
			n_impact_ncd_upp = quantile(temp, p = 0.975)			
			), 
			by = c("scenario", "year", "agegrp", "sex")]
					
				data_sum_agg[, unique(scenario) %>% length(.), ] 
				data_sum_agg[, max(year) - min(year) + 1, ] 
				data_sum_agg[, unique(agegrp) %>% length(.), ]
				data_sum_agg[, unique(sex) %>% length(.), ] 
				nrow(data_sum_agg)
				


		
		
		
		### gbd per 1 yo
		data_gbd <- read_fst(paste0("./inputs/disease_burden/", LOOP[iii, DATA_GBD]), as.data.table = TRUE)
				data_gbd[, max(year) - min(year) + 1, ] 
				data_gbd[, unique(age) %>% length(.), ]
				data_gbd[, unique(sex) %>% length(.), ] 
				nrow(data_gbd)	
		


		### gbd + pop
		data_gbd_pop <- merge(
			x = data_gbd,
			y = data_pop[, list(age, year, sex, pops), ],
			by = c("age", "year", "sex"),
			all.x = TRUE
			)
			
			nrow(data_gbd)
			nrow(data_pop)
			nrow(data_gbd_pop)			
			

		data_gbd_pop[, agegrp :=
			ifelse(between(age, 5* 6, 5* 6+4), "30-34", 
			ifelse(between(age, 5* 7, 5* 7+4), "35-39",
			ifelse(between(age, 5* 8, 5* 8+4), "40-44",
			ifelse(between(age, 5* 9, 5* 9+4), "45-49",
			ifelse(between(age, 5*10, 5*10+4), "50-54",
			ifelse(between(age, 5*11, 5*11+4), "55-59",
			ifelse(between(age, 5*12, 5*12+4), "60-64",
			ifelse(between(age, 5*13, 5*13+4), "65-69",
			ifelse(between(age, 5*14, 5*14+4), "70-74",
			ifelse(between(age, 5*15, 5*15+4), "75-79",
			ifelse(between(age, 5*16, 5*16+4), "80-84",
			ifelse(between(age, 5*17, 5*17+4), "85-89",
			ifelse(between(age, 5*18, 5*18+4), "90-94",
			ifelse(between(age, 5*19, 5*19+4), "95-99",
			NA)))))))))))))), ]
			
			a <- data_gbd_pop[, table(age, agegrp, useNA = "always"), ]
			a[1:50, ]
			a[51:nrow(a), ]			
			
			
		data_gbd_pop[, ":="(
			N       = mu2 * pops,
			N_lower = mu_lower * pops,
			N_upper = mu_upper * pops
			), ]
			
		
		### gbd + pop per 5 yo			
		data_gbd_agg <- data_gbd_pop[, .(
			n_gbd_med = sum(N),
			n_gbd_low = sum(N_lower),
			n_gbd_upp = sum(N_upper),
			pops = sum(pops)			
			), by = c("year", "agegrp", "sex")]
					
	
	
		data_gbd_agg[, ":="(
			rate_gbd_med = n_gbd_med/pops,
			rate_gbd_low = n_gbd_low/pops,
			rate_gbd_upp = n_gbd_upp/pops
			), ]
			
			data_gbd_agg[, max(year) - min(year) + 1, ] 
			data_gbd_agg[, unique(agegrp) %>% length(.), ]
			data_gbd_agg[, unique(sex) %>% length(.), ] 
			nrow(data_gbd_agg)
				
		

		
		
		
		### plot men
		data_sum_agg_sel <- data_sum_agg[sex == "men", , ]
		data_gbd_agg_sel <- data_gbd_agg[sex == "men", , ]
		
		#### rate
		ggplot() + 
			geom_line(data = data_gbd_agg_sel, aes(x = year, y = rate_gbd_med), color = "black") + 
			geom_line(data = data_gbd_agg_sel, aes(x = year, y = rate_gbd_low), color = "black", linetype = "dashed") + 
			geom_line(data = data_gbd_agg_sel, aes(x = year, y = rate_gbd_upp), color = "black", linetype = "dashed") + 
			
			geom_line(data = data_sum_agg_sel, aes(x = year, y = rate_impact_ncd_med), color = "red") + 
			geom_line(data = data_sum_agg_sel, aes(x = year, y = rate_impact_ncd_low), color = "red", linetype = "dashed") + 
			geom_line(data = data_sum_agg_sel, aes(x = year, y = rate_impact_ncd_upp), color = "red", linetype = "dashed") + 
			
			facet_wrap(. ~ factor(agegrp), scales="free_y") +
			theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) +
			ylab(LOOP[iii, DIS]) + xlab("Year") + ggtitle("Rate for men")
			
		ggsave(paste0(
			"./outputs/plots/plot_rate_impactNcd_vs_GBD_",
			gsub(".csv.gz", "", LOOP[iii, DATA]),
			"_",
			LOOP[iii, DIS],
			"_men.png"
			), height = HEIGHT, width = WIDTH)
		
		
		#### N		
		ggplot() + 
			geom_line(data = data_gbd_agg_sel, aes(x = year, y = n_gbd_med), color = "black") + 
			geom_line(data = data_gbd_agg_sel, aes(x = year, y = n_gbd_low), color = "black", linetype = "dashed") + 
			geom_line(data = data_gbd_agg_sel, aes(x = year, y = n_gbd_upp), color = "black", linetype = "dashed") + 
			
			geom_line(data = data_sum_agg_sel, aes(x = year, y = n_impact_ncd_med), color = "red") + 
			geom_line(data = data_sum_agg_sel, aes(x = year, y = n_impact_ncd_low), color = "red", linetype = "dashed") + 
			geom_line(data = data_sum_agg_sel, aes(x = year, y = n_impact_ncd_upp), color = "red", linetype = "dashed") + 
			
			facet_wrap(. ~ factor(agegrp), scales="free_y") +
			theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) +
			ylab(LOOP[iii, DIS]) + xlab("Year") + ggtitle("N for men")

			
		ggsave(paste0(
			"./outputs/plots/plot_N_impactNcd_vs_GBD_",
			gsub(".csv.gz", "", LOOP[iii, DATA]),
			"_",
			LOOP[iii, DIS],
			"_men.png"
			), height = HEIGHT, width = WIDTH)	
			
			



		### plot women
		data_sum_agg_sel <- data_sum_agg[sex == "women", , ]
		data_gbd_agg_sel <- data_gbd_agg[sex == "women", , ]
		
		#### rate
		ggplot() + 
			geom_line(data = data_gbd_agg_sel, aes(x = year, y = rate_gbd_med), color = "black") + 
			geom_line(data = data_gbd_agg_sel, aes(x = year, y = rate_gbd_low), color = "black", linetype = "dashed") + 
			geom_line(data = data_gbd_agg_sel, aes(x = year, y = rate_gbd_upp), color = "black", linetype = "dashed") + 
			
			geom_line(data = data_sum_agg_sel, aes(x = year, y = rate_impact_ncd_med), color = "red") + 
			geom_line(data = data_sum_agg_sel, aes(x = year, y = rate_impact_ncd_low), color = "red", linetype = "dashed") + 
			geom_line(data = data_sum_agg_sel, aes(x = year, y = rate_impact_ncd_upp), color = "red", linetype = "dashed") + 
			
			facet_wrap(. ~ factor(agegrp), scales="free_y") +
			theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) +
			ylab(LOOP[iii, DIS]) + xlab("Year") + ggtitle("Rate for women")

			
		ggsave(paste0(
			"./outputs/plots/plot_rate_impactNcd_vs_GBD_",
			gsub(".csv.gz", "", LOOP[iii, DATA]),
			"_",
			LOOP[iii, DIS],
			"_women.png"
			), height = HEIGHT, width = WIDTH)
		
		
		#### N		
		ggplot() + 
			geom_line(data = data_gbd_agg_sel, aes(x = year, y = n_gbd_med), color = "black") + 
			geom_line(data = data_gbd_agg_sel, aes(x = year, y = n_gbd_low), color = "black", linetype = "dashed") + 
			geom_line(data = data_gbd_agg_sel, aes(x = year, y = n_gbd_upp), color = "black", linetype = "dashed") + 
			
			geom_line(data = data_sum_agg_sel, aes(x = year, y = n_impact_ncd_med), color = "red") + 
			geom_line(data = data_sum_agg_sel, aes(x = year, y = n_impact_ncd_low), color = "red", linetype = "dashed") + 
			geom_line(data = data_sum_agg_sel, aes(x = year, y = n_impact_ncd_upp), color = "red", linetype = "dashed") + 
			
			facet_wrap(. ~ factor(agegrp), scales="free_y") +
			theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) +
			ylab(LOOP[iii, DIS]) + xlab("Year") + ggtitle("N for women")

			
		ggsave(paste0(
			"./outputs/plots/plot_N_impactNcd_vs_GBD_",
			gsub(".csv.gz", "", LOOP[iii, DATA]),
			"_",
			LOOP[iii, DIS],
			"_women.png"
			), height = HEIGHT, width = WIDTH)				
			

	}, warning = function(w) {
		# procedures when warning happens
		print(paste0("warning_", iii, "_", gsub(".csv.gz", "", LOOP[iii, DATA]), "_", LOOP[iii, DIS]))
	
	}, error = function(e) {
		# procedures when error happens
		print(paste0("error_", iii, "_", gsub(".csv.gz", "", LOOP[iii, DATA]), "_", LOOP[iii, DIS]))
		
	}, finally = {
		# procedure regardless of correctness
		NULL		
	})


}



















