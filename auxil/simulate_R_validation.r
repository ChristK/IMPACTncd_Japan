

####################################
###### validation plots of incidence, prevalence, and mortality
####################################

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




# rm(list=ls(all=TRUE))


# source("./global.R")

HEIGHT <- 5
WIDTH <- 10




######################################################
######################################################
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
			
			facet_wrap(. ~ factor(agegrp), scales="free") +
			theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) +
			ylab(LOOP[iii, DIS]) + xlab("Year") + ggtitle("Rate for men")
			
		ggsave(paste0(
			"./outputs/plots/plot_DIS_rate_impactNcd_vs_GBD_",
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
			
			facet_wrap(. ~ factor(agegrp), scales="free") +
			theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) +
			ylab(LOOP[iii, DIS]) + xlab("Year") + ggtitle("N for men")
			
		ggsave(paste0(
			"./outputs/plots/plot_DIS_N_impactNcd_vs_GBD_",
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
			
			facet_wrap(. ~ factor(agegrp), scales="free") +
			theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) +
			ylab(LOOP[iii, DIS]) + xlab("Year") + ggtitle("Rate for women")
			
		ggsave(paste0(
			"./outputs/plots/plot_DIS_rate_impactNcd_vs_GBD_",
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
			
			facet_wrap(. ~ factor(agegrp), scales="free") +
			theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) +
			ylab(LOOP[iii, DIS]) + xlab("Year") + ggtitle("N for women")
			
		ggsave(paste0(
			"./outputs/plots/plot_DIS_N_impactNcd_vs_GBD_",
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
















######################################################
######################################################
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
			
			facet_wrap(. ~ factor(agegrp), scales="free") +
			theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) +
			ylab(LOOP[iii, DIS]) + xlab("Year") + ggtitle("Rate for men")
			
		ggsave(paste0(
			"./outputs/plots/plot_DIS_rate_impactNcd_vs_GBD_",
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
			
			facet_wrap(. ~ factor(agegrp), scales="free") +
			theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) +
			ylab(LOOP[iii, DIS]) + xlab("Year") + ggtitle("N for men")
			
		ggsave(paste0(
			"./outputs/plots/plot_DIS_N_impactNcd_vs_GBD_",
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
			
			facet_wrap(. ~ factor(agegrp), scales="free") +
			theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) +
			ylab(LOOP[iii, DIS]) + xlab("Year") + ggtitle("Rate for women")
			
		ggsave(paste0(
			"./outputs/plots/plot_DIS_rate_impactNcd_vs_GBD_",
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
			
			facet_wrap(. ~ factor(agegrp), scales="free") +
			theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) +
			ylab(LOOP[iii, DIS]) + xlab("Year") + ggtitle("N for women")
			
		ggsave(paste0(
			"./outputs/plots/plot_DIS_N_impactNcd_vs_GBD_",
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













######################################################
######################################################
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
			
			facet_wrap(. ~ factor(agegrp), scales="free") +
			theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) +
			ylab(LOOP[iii, DIS]) + xlab("Year") + ggtitle("Rate for men")
			
		ggsave(paste0(
			"./outputs/plots/plot_DIS_rate_impactNcd_vs_GBD_",
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
			
			facet_wrap(. ~ factor(agegrp), scales="free") +
			theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) +
			ylab(LOOP[iii, DIS]) + xlab("Year") + ggtitle("N for men")

			
		ggsave(paste0(
			"./outputs/plots/plot_DIS_N_impactNcd_vs_GBD_",
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
			
			facet_wrap(. ~ factor(agegrp), scales="free") +
			theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) +
			ylab(LOOP[iii, DIS]) + xlab("Year") + ggtitle("Rate for women")

			
		ggsave(paste0(
			"./outputs/plots/plot_DIS_rate_impactNcd_vs_GBD_",
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
			
			facet_wrap(. ~ factor(agegrp), scales="free") +
			theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) +
			ylab(LOOP[iii, DIS]) + xlab("Year") + ggtitle("N for women")

			
		ggsave(paste0(
			"./outputs/plots/plot_DIS_N_impactNcd_vs_GBD_",
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




















##########################################
# validation plot of exposure
##########################################



#############################################
### simulated dataset
#############################################
dataSum <- fread("./outputs/xps/xps20.csv.gz")   # scaled up to the population.
# dataSum <- fread("./outputs/xps/xps_esp.csv.gz") # age-standardised




### see its contents
dataSum[, key := paste(year, sex, agegrp20, sep = "_")]
	str(dataSum)
	summary(dataSum)
	nrow(dataSum[, , ])
	nrow(dataSum[duplicated(key) == FALSE, , ])
	nrow(dataSum[, , ]) / nrow(dataSum[duplicated(key) == FALSE, , ])
	
	
	dataSum[, unique(year) %>% sort(), ]
	dataSum[, unique(sex) %>% sort(), ]
	dataSum[, unique(agegrp20) %>% sort(), ]
	dataSum[, unique(mc) %>% sort(), ]
	
	
	nrow(dataSum)
	dataSum[, unique(year) %>% sort() %>% length(), ] *
	dataSum[, unique(sex) %>% sort() %>% length(), ] *
	dataSum[, unique(agegrp20) %>% sort() %>% length(), ] *
	dataSum[, unique(mc) %>% sort() %>% length(), ]

	dataSum[year == 2001 & sex == "men" & agegrp20 == "All", , ]



### organize colnames
colnames(dataSum) <- colnames(dataSum) %>% gsub("_curr_xps", "", .)


### select data
dim(dataSum)
dataSum <- dataSum[(sex %in% "All") == FALSE, , ]
dim(dataSum)

dataSum <- dataSum[(agegrp20 %in% "All") == FALSE, , ]
dim(dataSum)
(2050 - 2001 + 1) *2 * 4 * 200




dataSumValEst <- dataSum[, .(
	year = first(year),
	sex = first(sex),
	agegrp20 = first(agegrp20),
	
	Fruit_vege = quantile(p = 0.5, Fruit_vege, na.rm = TRUE),
	Med_HT = quantile(p = 0.5, Med_HT, na.rm = TRUE),
	Med_HL = quantile(p = 0.5, Med_HL, na.rm = TRUE),
	Med_DM = quantile(p = 0.5, Med_DM, na.rm = TRUE),
	BMI = quantile(p = 0.5, BMI, na.rm = TRUE),
	HbA1c = quantile(p = 0.5, HbA1c, na.rm = TRUE),
	LDLc = quantile(p = 0.5, LDLc, na.rm = TRUE),
	SBP = quantile(p = 0.5, SBP, na.rm = TRUE),
	smok_never = quantile(p = 0.5, smok_never, na.rm = TRUE),
	smok_active = quantile(p = 0.5, smok_active, na.rm = TRUE),
	Smoking_number = quantile(p = 0.5, Smoking_number, na.rm = TRUE),
	pa567 = quantile(p = 0.5, pa567, na.rm = TRUE),
	
	
	Fruit_vege_low = quantile(p = 0.025, Fruit_vege, na.rm = TRUE),
	Med_HT_low = quantile(p = 0.025, Med_HT, na.rm = TRUE),
	Med_HL_low = quantile(p = 0.025, Med_HL, na.rm = TRUE),
	Med_DM_low = quantile(p = 0.025, Med_DM, na.rm = TRUE),
	BMI_low = quantile(p = 0.025, BMI, na.rm = TRUE),
	HbA1c_low = quantile(p = 0.025, HbA1c, na.rm = TRUE),
	LDLc_low = quantile(p = 0.025, LDLc, na.rm = TRUE),
	SBP_low = quantile(p = 0.025, SBP, na.rm = TRUE),
	smok_never_low = quantile(p = 0.025, smok_never, na.rm = TRUE),
	smok_active_low = quantile(p = 0.025, smok_active, na.rm = TRUE),
	Smoking_number_low = quantile(p = 0.025, Smoking_number, na.rm = TRUE),
	pa567_low = quantile(p = 0.025, pa567, na.rm = TRUE),
	
	Fruit_vege_up = quantile(p = 0.975, Fruit_vege, na.rm = TRUE),
	Med_HT_up = quantile(p = 0.975, Med_HT, na.rm = TRUE),
	Med_HL_up = quantile(p = 0.975, Med_HL, na.rm = TRUE),
	Med_DM_up = quantile(p = 0.975, Med_DM, na.rm = TRUE),
	BMI_up = quantile(p = 0.975, BMI, na.rm = TRUE),
	HbA1c_up = quantile(p = 0.975, HbA1c, na.rm = TRUE),
	LDLc_up = quantile(p = 0.975, LDLc, na.rm = TRUE),
	SBP_up = quantile(p = 0.975, SBP, na.rm = TRUE),
	smok_never_up = quantile(p = 0.975, smok_never, na.rm = TRUE),
	smok_active_up = quantile(p = 0.975, smok_active, na.rm = TRUE),
	Smoking_number_up = quantile(p = 0.975, Smoking_number, na.rm = TRUE),
	pa567_up = quantile(p = 0.975, pa567, na.rm = TRUE)
	), by = key]
	
	
dim(dataSumValEst)
(2050 - 2001 + 1) *2 * 4 









#############################################
### input observed dataset
#############################################
listDataObserved <- list.files("/home/rstudio/IMPACT_NCD_data/NHNS_data/Output_data_organized", pattern = "modeling")


tempDataHoge <- NULL
tempDataHoge <- fread(paste0("/home/rstudio/IMPACT_NCD_data/NHNS_data/Output_data_organized/", listDataObserved[1]))


for(iii in 2:length(listDataObserved)){

	tempData <- fread(paste0("/home/rstudio/IMPACT_NCD_data/NHNS_data/Output_data_organized/", listDataObserved[iii]))
	
	if(listDataObserved[iii] == "Data_modeling_Smoking_never_vs_ex.csv"){
	setnames(tempData, "Smoking", "Smoking_never_past")
	}

	if(listDataObserved[iii] == "Data_modeling_Smoking_NevEx_vs_current.csv"){
	setnames(tempData, "Smoking", "Smoking_others_current")
	}
	
	tempDifCols <- setdiff(colnames(tempData), colnames(tempDataHoge))
	tempSameCols <- intersect(colnames(tempData), colnames(tempDataHoge))
	
	tempDataHoge <- merge(x = tempDataHoge, y = tempData, by = c(tempSameCols), all = TRUE)
}





### organize variables
tempDataHoge[, sex := ifelse(Sex == 1, "women", "men"), ]
	tempDataHoge[, table(Sex, sex), ]


tempDataHoge[, agegrp20 := 
	ifelse(between(Age, 30, 49), "30-49",
	ifelse(between(Age, 50, 69), "50-69",
	ifelse(between(Age, 70, 89), "70-89",
	ifelse(Age >= 90, "90+",
	NA)))), ]

	tempDataHoge[, table(Age, agegrp20, useNA = "always"), ]


tempDataHoge[, pa567 := ifelse(PA_days >= 5, 1, 0), ]
	tempDataHoge[, table(pa567, PA_days), ]
	



	tempDataHoge[, table(Smoking_number), ]
setnames(tempDataHoge, "Smoking_number", "Smoking_number_grp")
tempDataHoge[Smoking_number_grp == 0L, Smoking_number := 5L ]
tempDataHoge[Smoking_number_grp == 1L, Smoking_number := 10L]
tempDataHoge[Smoking_number_grp == 2L, Smoking_number := 15L]
tempDataHoge[Smoking_number_grp == 3L, Smoking_number := 20L]
tempDataHoge[Smoking_number_grp == 4L, Smoking_number := 25L]
tempDataHoge[Smoking_number_grp == 5L, Smoking_number := 30L]
tempDataHoge[Smoking_number_grp == 6L, Smoking_number := 35L]
tempDataHoge[Smoking_number_grp == 7L, Smoking_number := 40L]
tempDataHoge[Smoking_number_grp == 8L, Smoking_number := sample(c(50L, 60L, 80L), .N, TRUE, prob = c(0.4, 0.45, 0.15))] # I do not explicitly set.seed because I do so at the beginning of gen_synthpop()
	tempDataHoge[, table(Smoking_number), ]



summary(tempDataHoge)









### select data
dim(tempDataHoge)
tempDataHoge <- tempDataHoge[Age >= 30, , ]
dim(tempDataHoge)




tempDataHoge[, key := paste(Year, sex, agegrp20, sep = "_")]





temp1 <- tempDataHoge[, .(
	year = first(Year),
	sex = first(sex),
	agegrp20 = first(agegrp20),
	Fruit_vege = mean(Fruit_vege, na.rm = TRUE),
	Med_HT = mean(Med_HT, na.rm = TRUE),
	Med_HL = mean(Med_HL, na.rm = TRUE),
	Med_DM = mean(Med_DM, na.rm = TRUE),
	BMI = mean(BMI, na.rm = TRUE),
	HbA1c = mean(HbA1c, na.rm = TRUE),
	LDLc = mean(LDLc, na.rm = TRUE),
	SBP = mean(SBP, na.rm = TRUE),
	smok_active = mean(Smoking_others_current, na.rm = TRUE),
	pa567 = mean(pa567, na.rm = TRUE)
	), by = key]
	



	tempDataHoge[, table(Smoking_others_current), ]


temp2 <- tempDataHoge[Smoking_others_current == 0, .(
	year = first(Year),
	sex = first(sex),
	agegrp20 = first(agegrp20),
	smok_never = 1 - mean(Smoking_never_past, na.rm = TRUE)
	), by = key]	




temp3 <- tempDataHoge[Smoking_others_current == 1, .(
	year = first(Year),
	sex = first(sex),
	agegrp20 = first(agegrp20),
	Smoking_number = mean(Smoking_number, na.rm = TRUE)
	), by = key]	





dataSumValObs <- merge(temp1, temp2, by = "key")
dataSumValObs <- merge(dataSumValObs, temp3, by = "key")
dim(dataSumValObs)
(2019 - 1995 + 1) * 2 * 4 
	















##################################
## Make validation plot
##################################
VAR <- c(
	"Fruit_vege"    
	, "Smoking_number"
	, "Med_HT"        
	, "Med_HL"        
	, "Med_DM"        
	, "BMI"           
	, "HbA1c"         
	, "LDLc"          
	, "SBP"           
	, "smok_never"    
	, "smok_active"   
	, "pa567" 
	)
	
VAR_LOW <- paste0(VAR, "_low")
VAR_UPP <- paste0(VAR, "_up")

	


LOOP <- data.table(VAR = VAR, VAR_LOW = VAR_LOW, VAR_UPP = VAR_UPP)



#### validation plot
HEIGHT <- 5
WIDTH <- 10


for(iii in 1:nrow(LOOP)){


tempObs <- dataSumValObs[, c("year", "sex", "agegrp20", LOOP[iii, VAR]), with = FALSE]
tempObs[, SexAgegrp := paste0(sex, agegrp20), ]
setnames(tempObs, LOOP[iii, VAR], "VAR")


tempEst <- dataSumValEst[, c("year", "sex", "agegrp20", LOOP[iii, VAR], LOOP[iii, VAR_LOW], LOOP[iii, VAR_UPP]), with = FALSE]
tempEst[, SexAgegrp := paste0(sex, agegrp20), ]
setnames(tempEst, LOOP[iii, VAR], "VAR")
setnames(tempEst, LOOP[iii, VAR_LOW], "VAR_LOW")
setnames(tempEst, LOOP[iii, VAR_UPP], "VAR_UPP")





ggplot() + 
	geom_line(data = tempObs, aes(x = year, y = VAR), color = "black") + 
	geom_line(data = tempEst, aes(x = year, y = VAR), color = "red") + 
	geom_line(data = tempEst, aes(x = year, y = VAR_LOW), color = "red", linetype = "dashed") + 
	geom_line(data = tempEst, aes(x = year, y = VAR_UPP), color = "red", linetype = "dashed") + 
	facet_wrap(. ~ factor(SexAgegrp), scales="free") +
	theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) +
	ylab(LOOP[iii, VAR]) + xlab("Year") + ggtitle(paste0(LOOP[iii, VAR], " in men"))
	
	
ggsave(paste0(
	"./outputs/plots/plot_RF_impactNcd_vs_NHNS_",
	LOOP[iii, VAR],
	".png"
	), height = HEIGHT, width = WIDTH)


}














