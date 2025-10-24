#-----------------------------
# run
#-----------------------------
mcnum <- 100L # Number of iterations
source("./global.R")
IMPACTncd <- Simulation$new("scenarios/sim_design_JPN21.yaml")
# IMPACTncd$del_parfs()
# IMPACTncd$del_synthpops()
# g <- IMPACTncd$get_causal_structure(print_plot = TRUE)
# g <- IMPACTncd$get_causal_structure(processed = FALSE, print_plot = TRUE, focus = "chd")
# plot(igraph::make_ego_graph(g, order = 1, c("chd"), "in")[[1]])


#-----------------------------
# base-case scenario
#-----------------------------
IMPACTncd$
    del_logs()$
    del_outputs()$
    run(1:mcnum, multicore = TRUE, "sc0")




#-----------------------------
# Scenario for SBP
#-----------------------------
# Scenario Goal: Reduce the age-standardized average SBP (for individuals 40 years and older, including those on medical treatment) by 5 mmHg from 2024 to 2032.
# 	1) assuming that the annual decrease in SBP is constant from 2024 to 2032 .
# 	2) assuming that people with higher SBP will experience a larger decrease in SBP.
#	3) assuming that the decrease in SBP depnds on SBP only (SBP depends on age, sex, .......)
#	4) assuming the difference between the base-case and intervention scenarios remains after the intervention period.
#	5) assuming that apply the decrease in SBP only to individuals aged 40 years and older.




# apply the scenario to the synthpop datasets
IMPACTncd$update_primary_prevention_scn(function(synthpop){
	#-----------------------------
	# setting for Healthy Japan 21
	#-----------------------------
	#year
	year_base <- 2024 # R6
	year_goal <- 2032 # R14
	year_last <- 2040 # R26
	
	
	
	# SONOTE added lower_clip to proportional_reduction() to limit the lowest value after intervention
	proportional_reduction_bounded <- function(
	exposure,     # exposure: eg. SBP_curr_xps,
	change,       # average change （minus change = - X）
	weights,      # weight = age-standardised
	penalty = 1,  # penalty: determines how sharply the penalty increases with x. penalty
	lower_clip = -Inf  # the lowest value after the intervention (not delta, but value)
	) {
	# 
	wm <- weighted.mean(exposure, weights)
	# 
	c <- (wm - (wm + change)) / weighted.mean(exposure^penalty, weights)
	message("c: ", c)
	# 
	new_exposure <- exposure - c * exposure^penalty
	# 
	new_exposure <- pmax(new_exposure, lower_clip)
	# 
	return(invisible(new_exposure))
	}
		


    # goal of SBP decrease
    # CKNOTE you need to define these in the function
    total_delta_SBPsc <- -5
    age_target_SBPsc <- 40 #40 and over yo
	penalty <- 3
	lower_clip <- synthpop$pop[, quantile(SBP_curr_xps, p = 0.0001)]
	
	#-- preparation
    # CKNOTE should be setkeyv(), better than setorder()
	setkeyv(synthpop$pop, c("pid", "year"))
	synthpop$pop[, SBP_curr_xps_original := copy(SBP_curr_xps), ]
	synthpop$pop[, wt_SBPSc := copy(wt_esp), ] # age-standardised prevalece NEED CHANGE

	
	#-- setting intervention (when where who what how)
    ## CKNOTE the above assumes change in year_base. Sometimes we assume no change for year_base
	effect_size <- synthpop$pop[
		year %in% (year_base:year_last) & age >= age_target_SBPsc, 
		.(sbp_mean_base = weighted.mean(SBP_curr_xps_original, wt_SBPSc)), 
		by = year
		]
	setorder(effect_size, year)
	
	## Annually "equally decreasing"
	effect_size[, sbp_mean_target_tmp := c(	
		seq(
			from = .SD[year == year_base, sbp_mean_base, ], 
			to = .SD[year == year_base, sbp_mean_base, ] + total_delta_SBPsc, 
			length.out = (year_goal - year_base + 1)
			),
		rep(
			x = .SD[year == year_base, sbp_mean_base, ] + total_delta_SBPsc, 
			time = year_last - year_goal
			)
		)
	, ]
	
	#assuming the difference between the base-case and intervention scenarios remains after the intervention period.
	effect_size[between(year, year_goal, year_last), sbp_mean_target_tmp := sbp_mean_base +	.SD[year == year_goal, pmin(sbp_mean_target_tmp, sbp_mean_base) - sbp_mean_base, ], ]
	effect_size[, sbp_mean_target := pmin(sbp_mean_target_tmp, sbp_mean_base), ]
	effect_size[, delta := sbp_mean_target - sbp_mean_base, ]
	effect_size

	effect_size <- effect_size[, list(year, delta), ]
	effect_size
	
				   
	# zero padding for years before the initial year of the intevention
    synthpop$pop[, delta := NA_real_]
    synthpop$pop[effect_size, on = 'year', delta := i.delta]
	
	#-- Perform intervention (who, when)
    synthpop$pop[age >= age_target_SBPsc & year %in% (year_base:year_last),
                 SBP_curr_xps := proportional_reduction_bounded(
					exposure = SBP_curr_xps_original, 
					change = first(delta), 
					weights = wt_SBPSc, 
					penalty = penalty, 
					lower_clip = lower_clip
					),
                 by = year
				 ]
				 # SONOTE 20250711  := with keyby is only possible when i is not supplied since you can't setkey on a subset of rows. Either change keyby to by or remove i
	
		#synthpop$pop[, summary(SBP_curr_xps), ]
	synthpop$pop[
		age >= age_target_SBPsc & year %in% (year_base:year_last),
		SBP_curr_xps := pmin(SBP_curr_xps, SBP_curr_xps_original),  # the policy does not have bad effect.
		]				 
		#synthpop$pop[, summary(SBP_curr_xps), ]



	# delete objects
	# delete unnecessary colums and objects
	 synthpop$pop[, ":="(
	 	SBP_curr_xps_original = NULL,
	 	wt_SBPSc = NULL,
	 	delta = NULL
	 	), ]
	 
	 rm(
	 	age_target_SBPsc
	 	, effect_size
	 	, tmp
	 	, total_delta_SBPsc
	 	, penalty
		)



})


IMPACTncd$
  run(1:mcnum, multicore = TRUE, "SBP")







#-----------------------------
# Scenario for high ldl-c 
#-----------------------------
# Scenario Goal: Achieve a 25% reduction from baseline in the age-adjusted proportion of adults aged 40 and older who have LDL cholesterol levels ≥160 mg/dL (including those receiving oral treatment). The denominator is defined as adults aged 40 and older.
#	1) assuming decreases in LDL-C can achieve the target reduction in the proportion of LDL-C ≥160 mg/dL among adults aged 40 and older.
# 	2) assuming that people with higher LDLc will experience a larger decrease in LDLc.

# 	1) assuming that the annual decrease in LDL-c is constant from 2024 to 2032 
# 	4) assuming that the decrease in LDLc depends on LDLc only (LDLc depends on age, sex, .......) 
#	5) assuming the difference between the base-case and intervention scenarios remains after the intervention period.
# 	6) assuming that apply the decrease in LDLc only to individuals aged 40 years and older, and LDLc >= 160 mg/dl (high risk approach)


 
# apply the scenario to the synthpop datasets
IMPACTncd$update_primary_prevention_scn(function(synthpop){
	#-----------------------------
	# setting for Healthy Japan 21
	#-----------------------------
	#year
	year_base <- 2024 # R6
	year_goal <- 2032 # R14
	year_last <- 2040 # R26
	
	

	
	# SONOTE added lower_clip to proportional_reduction() to limit the lowest value after intervention
	proportional_reduction_bounded <- function(
	exposure,     # exposure: eg. SBP_curr_xps,
	change,       # average change （minus change = - X）
	weights,      # weight = age-standardised
	penalty = 1,  # penalty: determines how sharply the penalty increases with x. penalty
	lower_clip = -Inf  # the lowest value after the intervention (not delta, but value)
	) {
	# 
	wm <- weighted.mean(exposure, weights)
	# 
	c <- (wm - (wm + change)) / weighted.mean(exposure^penalty, weights)
	message("c: ", c)
	# 
	new_exposure <- exposure - c * exposure^penalty
	# 
	new_exposure <- pmax(new_exposure, lower_clip)
	# 
	return(invisible(new_exposure))
	}



	
	# high-risk approach interventions
	proportional_reduction_threshold_highrisk <-
	function(exposure, threshold_impact, threshold_target, target_prop, weights, penalty = 1, lower_clip) {
		# it only applies changes above the threshold_impacts and optimises the
		# target proportion above the threshold_target
		exposure_bounded <- exposure[exposure > threshold_impact]
		weights_bounded <- weights[exposure > threshold_impact]
		
		# Function to compute proportion above threshold given a specific change
		calc_prop_above <- function(change) {
		adj_exposure <- exposure
		adj_exposure[exposure > threshold_impact] <-
			proportional_reduction_bounded(exposure_bounded, change, weights_bounded, penalty)
		sum(weights[adj_exposure > threshold_target]) / sum(weights)
		}
	
		# Find the change required to achieve the target proportion above threshold
		optimal_change <- uniroot(
		f = function(change) calc_prop_above(change) - target_prop,
		interval = c(-max(exposure), max(exposure)),
		extendInt = "yes", tol = .Machine$double.eps, maxiter = 1000, trace = 0
		)$root
	
		message("optimal_change: ", optimal_change)
		# Apply the optimal change
		exposure[exposure > threshold_impact] <-
		proportional_reduction_bounded(exposure_bounded, optimal_change, weights_bounded, penalty)
	
		exposure <- pmax(exposure, lower_clip)
		return(exposure)    
	}
	

	# goal of high ldl-c proportion decrease
	# Find current proportion of people with LDLc_curr_xps_original >= 160 taking into account weights
	# setting #1 for baseline value
	age_target_LDLCsc <- 40
	ldlc_threshold_LDLCsc <- 160
	threshold_impact_LDLCsc <- 160

	# setting #2 for baseline and target values
	target_reduction_rate <- (1 - 0.25)
	
	# setting #3 for the function
	penalty <- 2
	lower_clip <- synthpop$pop[, quantile(LDLc_curr_xps, p = 0.0001)]
	
	#-- preparation
    # CKNOTE should be setkeyv(), better than setorder()
	setkeyv(synthpop$pop, c("pid", "year"))
	synthpop$pop[, LDLc_curr_xps_original := copy(LDLc_curr_xps), ]
	synthpop$pop[, wt_LDLcSc := copy(wt_esp), ] # age-standardised prevalece  NEED CHANGE


	# baseline value
	sc0_prp <- synthpop$pop[
			year == year_base & age >= age_target_LDLCsc,
			sum(wt_LDLcSc[LDLc_curr_xps_original >= ldlc_threshold_LDLCsc]) / sum(wt_LDLcSc)
		]

	#-- setting intervention (when where who what how)
    ## CKNOTE the above assumes change in year_base. Sometimes we assume no change for year_base
	# target values; Let's assume we aim a 25% reduction (relative) of people with LDLc_curr_xps_original >= 160. The scenario target should then be
	sc1_trgt <- sc0_prp * target_reduction_rate
		print(sc0_prp)
		print(sc1_trgt)

    effect_size_cfscenario <- data.table(
						year = year_base:year_last,
						delta_cfscenario = c(seq(sc0_prp, sc1_trgt, length.out = (year_goal - year_base + 1)), rep(sc1_trgt, year_last - year_goal))
                   )
	
	# effect size in the base-case scenario
	effect_size_baseSc <- synthpop$pop[
		age >= age_target_LDLCsc & between(year, year_base, year_last), 
		.(delta_base = sum(wt_LDLcSc[LDLc_curr_xps_original >= ldlc_threshold_LDLCsc]) / sum(wt_LDLcSc)), 
		by = year
		] %>% setorder(year)

	# select lower proportion of high LDL-c 
	effect_size <- effect_size_cfscenario[effect_size_baseSc, on = "year"]
	effect_size[, delta := pmin(delta_base, delta_cfscenario), ]
	effect_size[, ":="(
		delta_cfscenario = NULL,	
		delta_base = NULL
		), ]
		

	# baseline padding for years before the initial year of the intevention
	synthpop$pop[, delta := NA_real_, ]

		print(synthpop$pop[, unique(delta), by = year] %>% setorder(year))
    synthpop$pop[effect_size, on = 'year', delta := i.delta]
		print(synthpop$pop[, unique(delta), by = year] %>% setorder(year))

	
	
	#-- Perform intervention (who)	
	synthpop$pop[age >= age_target_LDLCsc & year %in% (year_base:year_last),
			LDLc_curr_xps := proportional_reduction_threshold_highrisk(
				exposure = LDLc_curr_xps_original, 
				threshold_impact = threshold_impact_LDLCsc,
				threshold_target = ldlc_threshold_LDLCsc, 
				target_prop = unique(delta), 
				weights = wt_LDLcSc, 
				penalty = penalty,
				lower_clip = lower_clip#,
				#verbose = TRUE
				),
			by = year
			]

		#synthpop$pop[, summary(LDLc_curr_xps), ]
	synthpop$pop[
		age >= age_target_LDLCsc & year %in% (year_base:year_last),
		LDLc_curr_xps := pmin(LDLc_curr_xps, LDLc_curr_xps_original), # the policy does not have bad effect.
		]				 
		#synthpop$pop[, summary(LDLc_curr_xps), ]



	# delete unnecessary colums and objects
	 synthpop$pop[, ":="(
	 	LDLc_curr_xps_original = NULL, 
	 	wt_LDLcSc = NULL, 
	 	delta = NULL
	 	)
	 	, ]
	 
	 rm(
	 	age_target_LDLCsc
	 	, effect_size
	 	, effect_size_baseSc
	 	, effect_size_cfscenario
	 	, ldlc_threshold_LDLCsc
	 	, penalty
	 	, sc0_prp
	 	, sc1_trgt
	 	, target_reduction_rate                                      
	 	, tmp
	 	, tmp1
	 	, tmp2
	 )

})
		
IMPACTncd$
  run(1:mcnum, multicore = TRUE, "LDLc")


    
                     












#-----------------------------
# Scenario for suggestive diabetes
#-----------------------------
# Scenario Goal: Japanese government obtained future projections based on the trend-analysis tool by gender and age group (ages 20–39, 40–49, 50–59, 60–69, and 70 and over). Assuming the current trend continues, the number of people with diabetes is expected to reach approximately 14.48 million by fiscal year 2032. Therefore, to further curb this by 6.7% (to be 93.3%) through the promotion of countermeasures, the target has been set at 13.5 million people.

#	1) assuming that our modeling, we define “Individuals Strongly Suspected of Having Diabetes” based only on HbA1c ≥ 6.5%. 
#		In addition, we ignore the treatment for diabetes because it must be a strange policy to reduce treatment for people with high HbA1c.
#		However, in Health Japan 21, definition of “Individuals Strongly Suspected of Having Diabetes” is as follows.
#		Based on the National Health and Nutrition Survey: A person is classified as "strongly suspected of having diabetes" if:
#			Their measured Hemoglobin A1c (HbA1c, NGSP) is 6.5% or higher,
#				OR
#			They answered “Yes” to the question:“Are you currently receiving treatment for diabetes (including regular checkups or lifestyle guidance at a medical institution)?”
# 	2) assuming that the annual decrease in HbA1c is constant from 2024 to 2032. 
#	3) assuming decreases in HbA1c can achieve the target reduction in the proportion of HbA1c ≥ 6.5%. 
# 	4) assuming that people with higher HbA1c will experience a larger decrease in HbA1c.
# 	5) assuming that the decrease in HbA1c depends on HbA1c only (HbA1c depends on age, sex, .......) 
#	6) assuming the difference between the base-case and intervention scenarios remains after the intervention period.
# 	7) assuming that apply the decrease in HbA1c to individuals aged 30 years old and older. But, HJ21 targets people aged 20 yo and older. So, we assumed suggestive DM of 0% in 20-29 yo. This assumption would be valid based on NHNS results between 2019 showing low prevalence of sug DM in 20-29 yo.

 
# apply the scenario to the synthpop datasets
IMPACTncd$update_primary_prevention_scn(function(synthpop){
	#-----------------------------
	# setting for Healthy Japan 21
	#-----------------------------
	#year
	year_base <- 2024 # R6
	year_goal <- 2032 # R14
	year_last <- 2040 # R26
	
	
	# SONOTE added lower_clip to proportional_reduction() to limit the lowest value after intervention
	proportional_reduction_bounded <- function(
		exposure,     # exposure: eg. SBP_curr_xps,
		change,       # average change （minus change = - X）
		weights,      # weight = age-standardised
		penalty = 1,  # penalty: determines how sharply the penalty increases with x. penalty
		lower_clip = -Inf  # the lowest value after the intervention (not delta, but value)
	) {
	# 
	wm <- weighted.mean(exposure, weights)
	# 
	c <- (wm - (wm + change)) / weighted.mean(exposure^penalty, weights)
	message("c: ", c)
	# 
	new_exposure <- exposure - c * exposure^penalty
	# 
	new_exposure <- pmax(new_exposure, lower_clip)
	# 
	return(invisible(new_exposure))
	}
	
	
	# population-level approach interventions
	# SONOTE: SO added the lower_clip, the lowest value after the intervention (not delta, but value)
	proportional_reduction_threshold_struct <-
	function(exposure, threshold, target_prop, weights, lower_clip, penalty = 1) {
	
	
	# Function to compute proportion above threshold given a specific change
	calc_prop_above <- function(change){
		adj_exposure <-
		proportional_reduction_bounded(exposure, change, weights, penalty)
		sum(weights[adj_exposure > threshold]) / sum(weights)
	}
	
	# Find the change required to achieve the target proportion above threshold
	optimal_change <- uniroot(
		f = function(change) calc_prop_above(change) - target_prop,
		interval = c(-max(exposure), max(exposure)),
		extendInt = "yes", tol = .Machine$double.eps, maxiter = 1000, trace = 0
	)$root
	
	message(optimal_change)
	# Apply the optimal change
	exposure <- proportional_reduction_bounded(exposure, optimal_change, weights, penalty)
	exposure <- pmax(exposure, lower_clip)
	return(exposure)
	
	}
	
	
	
	
	


	# goal of high sug.DM proportion decrease
	# Find current proportion of people with high HbA1c  >= 6.5% taking into account weights
	# setting #1 for baseline value
	age_target_HbA1csc <- 30
	HbA1c_threshold_HbA1csc <- 6.5

	# setting #2 for baseline and target values
	target_reduction_rate <- (1 - 0.067)
	
	# setting #3 for the function
	penalty <- 2
	lower_clip <- synthpop$pop[, quantile(HbA1c_curr_xps, p = 0.0001)]

	#-- preparation
    # CKNOTE should be setkeyv(), better than setorder()
	setkeyv(synthpop$pop, c("pid", "year"))
	synthpop$pop[, HbA1c_curr_xps_original := copy(HbA1c_curr_xps), ]
	synthpop$pop[, wt_HbA1cSc := wt, ] # NOT age-standardised proportion reduction for each age group, leading to absolute number



	# baseline values: we defined these data in 2032 by age groups following the concept of this target in Health Japan 21
	synthpop$pop[, agegrp_HbA1cSc := 
		fifelse(between(age, 20, 29), "20-29",
		fifelse(between(age, 30, 39), "30-39",
		fifelse(between(age, 40, 49), "40-49",
		fifelse(between(age, 50, 59), "50-59",
		fifelse(between(age, 60, 69), "60-69",
		"70+"))))), ]
		# synthpop$pop[, table(age, agegrp_HbA1cSc) ]

	tmp_base <- synthpop$pop[
			year == year_base & age >= age_target_HbA1csc,
			.(sc0_base = .SD[HbA1c_curr_xps_original >= HbA1c_threshold_HbA1csc, .N, ] / .N),
		by = .(agegrp_HbA1cSc, sex)
		] %>% setorder(., sex, agegrp_HbA1cSc)


	#-- setting intervention (when where who what how)
    ## We assume no change for year_base
	## target values; Let's assume a 6.7% reduction (relative) of people with HbA1c_curr_xps_original >= 6.5% in 2032 in Health Japan 21 scenario cmpared to that in 2032 in base-case scenario (trend-based projection restuls by a previous calculation). Instead of using the previous projections, we used the projected results in the base-case because of the aforementioned assumption (1).
	tmp_goal <- synthpop$pop[
			year == year_goal & age >= age_target_HbA1csc,
			.(sc0_trgt = .SD[HbA1c_curr_xps_original >= HbA1c_threshold_HbA1csc, .N, ] / .N),
		by = .(agegrp_HbA1cSc, sex)
		] %>% setorder(., sex, agegrp_HbA1cSc)
		
	tmp_goal[, sc1_trgt := sc0_trgt * target_reduction_rate, ]
	tmp_goal[, sc0_trgt := NULL, ]

	tmp_combine <- merge(tmp_base, tmp_goal, by = c("agegrp_HbA1cSc", "sex"))

	effect_size_cf <- tmp_combine[, .(
		year = year_base:year_last,
		delta_cfscenario = c(seq(sc0_base, sc1_trgt, length.out = (year_goal - year_base + 1)), rep(sc1_trgt, year_last - year_goal))
		), by = .(agegrp_HbA1cSc, sex)
		]
		
	# effect_size in the base-case scenario
	effect_size_bs <- synthpop$pop[
			between(year, year_base, year_last) & age >= age_target_HbA1csc,
			.(delta_base = .SD[HbA1c_curr_xps_original >= HbA1c_threshold_HbA1csc, .N, ] / .N),
		by = .(agegrp_HbA1cSc, sex, year)
		] %>% setorder(., sex, agegrp_HbA1cSc)
		
		
		
	# select the lower proportion of individuals with high HbA1c between base-case and Health Japan 21 scenarios
	effect_size <- merge(effect_size_bs, effect_size_cf, by = c("agegrp_HbA1cSc", "sex", "year")) 
	effect_size[, delta := pmin(delta_base, delta_cfscenario), ]
	effect_size[, ":="(
		delta_cfscenario = NULL,	
		delta_base = NULL
		), ]


	# baseline padding for years before the initial year of the intevention
	synthpop$pop[, delta := NA_real_, ]
    synthpop$pop[effect_size, on = c("year", "agegrp_HbA1cSc", "sex") , delta := i.delta]
	
	#-- Perform intervention (who)	
	synthpop$pop[age >= age_target_HbA1csc & year %in% (year_base:year_last),
			HbA1c_curr_xps := proportional_reduction_threshold_struct(
				exposure = HbA1c_curr_xps_original, 
				threshold = HbA1c_threshold_HbA1csc, 
				target_prop = unique(delta), 
				weights = wt_HbA1cSc, 
				penalty = penalty,
				lower_clip = lower_clip
				),
			by = .(year, agegrp_HbA1cSc, sex)
			]
	
	synthpop$pop[
		age >= age_target_HbA1csc & year %in% (year_base:year_last),
		HbA1c_curr_xps := pmin(HbA1c_curr_xps, HbA1c_curr_xps_original), # the policy does not have bad effect.
		]				 






	# delete unnecessary colums and objects
	 synthpop$pop[, ":="(
	 	HbA1c_curr_xps_original = NULL, 
	 	wt_HbA1cSc = NULL, 
	 	agegrp_HbA1cSc = NULL, 
	 	delta = NULL
	 	)
	 	, ]
	 
	 rm(
	 	age_target_HbA1csc
	 	, effect_size
	 	, effect_size_bs
	 	, effect_size_cf
	 	, HbA1c_threshold_HbA1csc
	 	, lower_clip
	 	, penalty
	 	, sc0_prp
	 	, sc1_trgt
	 	, target_reduction_rate                                      
	 	, tmp
	 	, tmp_base
	 	, tmp_goal
	 	, tmp_combine
	 	, tmp1
	 	, tmp2
	 )


})


IMPACTncd$
  run(1:mcnum, multicore = TRUE, "HbA1c")




















#-----------------------------
# Scenario for smoking
#-----------------------------
# Scenario Goal: Reduce smoking proportion among people aged 20 years and older to be 12% by 2032.
# 	1) assuming that the annual decrease in smoking proportion is constant from 2024 to 2032 .
# 	2) assuming that the annual decrease in smoking proportion is constant regardless age and sex.
# 	3) assuming that this intervention is applied only to current smokers, meaning that chaning current smokers will be changed to be ex-smokers (3 -> 2 in the variable, "Smoking_curr_xps") and smoking number ("Smoking_number_curr_xps") of NA.
#	4) assuming the difference between the base-case and intervention scenarios remains after the intervention period.
# 	5) assuming that apply the decrease to individuals aged 30 years old and older. But, HJ21 targets people aged 20 yo and older. So, we assumed smoking prevalenc of XXX in in 20-29 yo. 




#--------------------#--------------------#--------------------
#-------------------- Effect of smoking cessation on BMI
#--------------------#--------------------#--------------------
# BMI increased by 1.14 kg/m² (95% CI: 0.50–1.79) after smoking cessation over an average follow-up of 4.84 years (The association between quitting smoking and weight gain: a systematic review and meta-analysis of prospective cohort studies).

# Calculate the cumulative years of smoking cessation according to the Health Japan 21 smoking scenario. If smoking is resumed, subtract the corresponding years from the cumulative total.



# apply the scenario to the synthpop datasets
IMPACTncd$update_primary_prevention_scn(function(synthpop){
	#-----------------------------
	# setting for Healthy Japan 21
	#-----------------------------
	#year
	year_base <- 2024 # R6
	year_goal <- 2032 # R14
	year_last <- 2040 # R26
	
	# goal of smoking proportion decrease
	# Find current proportion of people with current smokier
	# setting #1 for baseline value
	age_target_smokingsc <- 30
	
	# setting #2 for baseline and target values
	target_reduction_rate <- 0.12 / ( (1 -  0.115822198) + (0.115822198 / 0.9805446))
	# Accounting for differences in the prevalence of current smoking between adults aged 30+ years 
	# and those aged 20–29 years from 2015 to 2019 using the ratio method. 
	# The proportion of the population aged 20–29 years among all adults aged 20 and older (0.115822198) 
	# was used together with the prevalence ratio of current smoking (30+ divided by 20–29 = 0.9805446) 
	# to adjust the target.
	
	# target_reduction_rate <- 0.12 + 0.115822198 * (-0.003533689) 
	# Accounting for differences in the prevalence of current smoking between adults aged 30+ years 
	# and those aged 20–29 years from 2015 to 2019 using the difference method. 
	# The proportion of the population aged 20–29 years among all adults aged 20 and older (0.115822198) 
	# was multiplied by the difference in current smoking prevalence (30+ minus 20–29 = -0.003533689).





	#-- preparation
    # CKNOTE should be setkeyv(), better than setorder()
	setkeyv(synthpop$pop, c("pid", "year"))
	synthpop$pop[, Smoking_curr_xps_original := copy(Smoking_curr_xps), ]
	synthpop$pop[, Smoking_number_curr_xps_original := copy(Smoking_number_curr_xps), ]
	synthpop$pop[, BMI_curr_xps_original := copy(BMI_curr_xps), ]
	synthpop$pop[, wt_smokingsc := wt, ] # NOT age-standardised


	# baseline value: we defined the value in 2024
	# baseline value
	sc0_prp <- synthpop$pop[
			year == year_base & age >= age_target_smokingsc,
			sum(wt_smokingsc[Smoking_curr_xps_original == 3]) / sum(wt_smokingsc)
		]		
		#synthpop$pop[sex == "men", table(Smoking_curr_xps), ]
		#synthpop$pop[sex == "women", table(Smoking_curr_xps), ]


	#-- setting intervention (when where who what how)
    ## CKNOTE the above assumes change in year_base. Sometimes we assume no change for year_base
	# target values; Let's assume we aim target_reduction_rate (around 12%) of people with smoking_curr_xps_original == 1. The scenario target should then be
	sc1_trgt <- (sc0_prp - target_reduction_rate) / sc0_prp
		print(sc0_prp)
		print(sc1_trgt)
		(sc0_prp - (sc0_prp * sc1_trgt)) == target_reduction_rate
		(sc0_prp - (sc0_prp * sc1_trgt)) 
		target_reduction_rate

    effect_size_cfscenario <- data.table(
						year = year_base:year_last,
						delta_cfscenario = c(seq(0, sc1_trgt, length.out = (year_goal - year_base + 1)), rep(sc1_trgt, year_last - year_goal))
                   )
	
	effect_size_cfscenario[, smoking_prevalence_cfscenario := sc0_prp - (sc0_prp * delta_cfscenario), ]
				   
	# effect size in the base-case scenario
	effect_size_baseSc <- synthpop$pop[
		age >= age_target_smokingsc & between(year, year_base, year_last), 
		.(smoking_prevalence_base = sum(wt_smokingsc[Smoking_curr_xps_original == 3]) / sum(wt_smokingsc)), 
		by = year
		] %>% setorder(year)
		
	effect_size_baseSc[, delta_base := (sc0_prp - smoking_prevalence_base) / sc0_prp, ]
	

	# select larger probability of smoking cessesioin in current smoking prevalence 
	effect_size <- effect_size_cfscenario[effect_size_baseSc, on = "year"]
	effect_size[, delta := pmax(delta_base, delta_cfscenario), ]
	effect_size[, ":="(
		smoking_prevalence_cfscenario = NULL,	
		smoking_prevalence_base = NULL,	
		delta_cfscenario = NULL,	
		delta_base = NULL
		), ]				   
				   

	# baseline padding for years before the initial year of the intevention
	synthpop$pop[, delta := NA_real_, ]

		print(synthpop$pop[, unique(delta), by = year] %>% setorder(year))
    synthpop$pop[effect_size, on = 'year', delta := i.delta]
		print(synthpop$pop[, unique(delta), by = year] %>% setorder(year))

	
	
	#-- Perform intervention (who)
	rs <- .Random.seed # it saves the current random seed. This is recovered after getting "Smoking_stop" and "bmi_effect".
	set.seed(42L + synthpop$mc)

	synthpop$pop[age >= age_target_smokingsc & Smoking_curr_xps_original == 3 & year %in% (year_base:year_last),
		Smoking_stop := rbinom(n = .N, size = 1, prob = delta),
			#by = year
			]
	
	set.seed(rs)

	
	synthpop$pop[Smoking_stop == 1, Smoking_curr_xps := 2, ]
	synthpop$pop[Smoking_stop == 1, Smoking_number_curr_xps := NA, ]

	
	
	# delete unnecessary colums and objects
	rm(
	effect_size
	, effect_size_baseSc
	, effect_size_cfscenario
	, sc0_prp
	, sc1_trgt
	, tmp
	, tmp1
	, tmp2
	, target_reduction_rate
	)





#--------------------#--------------------#--------------------
#-------------------- Effect of smoking cessation on BMI
#--------------------#--------------------#--------------------

    # how much: set effect size of smoking cessation on BMI (kg/m^2)  
    bmi_se  <- (1.79 - 0.50)/(2 * (qnorm(0.975)))
    bmi_effect <- rnorm(1, mean = 1.14, sd = bmi_se) / 4.84 #change provided is for 4.84 years from smoking cessation. Thus, this effect is devided by 4.84
    bmi_effect[bmi_effect < 0] <- 0 # make sure effect is nor reversed
	

	
	# Calculate the cumulative years of smoking cessation according to the Health Japan 21 smoking scenario. If smoking is resumed, subtract the corresponding years from the cumulative total.
	setkeyv(synthpop$pop, c("pid","year"))
	synthpop$pop[
		is.na(Smoking_stop) == FALSE, 
		Smoking_score := {
			# initial score = the first value of Smoking_stop (0 or 1)
			init <- Smoking_stop[1L]
			# step: +1 if Smoking_stop==1, -1 if Smoking_stop==0
			step <- ifelse(Smoking_stop == 1L, 1L, -1L)
			# for the very first row, keep the initial score (no increment/decrement)
			step[1L] <- 0L
			# cumulative process: update score at each step and clamp it within [0, 5]
			Reduce(
				f = function(previous_score, delta){
					# update score = previous score + change
					updated <- previous_score + delta
					# clamp the score to the range [0, 5]
					bounded <- pmin(5L, pmax(0L, updated))
					return(bounded)
				},
				x = step,
				init = init,
				accumulate = TRUE
			)[-1]  # drop the initial value from the result
	}, 
	by = pid]



	#　Assign mediation effect of smoking cessation on BMI increase.
    synthpop$pop[
		is.na(Smoking_stop) == FALSE, 
		":="(
			BMI_curr_xps = BMI_curr_xps_original + (Smoking_score * bmi_effect)
			)
		, ]



	synthpop$pop[, ":="(
		Smoking_curr_xps_original = NULL
		, Smoking_number_curr_xps_original = NULL 
		, wt_smokingsc = NULL
		, delta = NULL
		, Smoking_stop = NULL
		, Smoking_score = NULL
		, BMI_curr_xps_med = NULL
		, BMI_curr_xps_original = NULL
		)
		, ]


	# delete unnecessary colums and objects
	rm(
	effect_size
	, effect_size_baseSc
	, effect_size_cfscenario
	, sc0_prp
	, sc1_trgt
	, tmp
	, tmp1
	, tmp2
	, target_reduction_rate
	, age_target_smokingsc
	, bmi_effect
	, bmi_se
	)



})


IMPACTncd$
  run(1:mcnum, multicore = TRUE, "Smoking")


















#-----------------------------
# Scenario for BMI
#-----------------------------
# Scenario Goal: adequate BMI
# Increase in the number of people maintaining an appropriate body weight (reduction in obesity, underweight young women, and elderly people with a tendency toward malnutrition)
# Proportion of individuals with a BMI of 18.5 or more and less than 25 (for those aged 65 and older, a BMI over 20 and less than 25)
# To achieve this goal, Health Japan 21 set the following strategy
# When the targets in [a] to [d] are achieved, the projected proportion of individuals with an appropriate body weight in 2032 (R14) is 65.9%. Therefore, the target value is set at 66% or higher.
# [a] Men aged 20–69: Reduction in the proportion of obese individuals (BMI ≥ 25): Less than 30%
# [b] Women aged 40–69: Reduction in the proportion of obese individuals (BMI ≥ 25): Less than 15%
# [c] Women aged 20–39: Reduction in the proportion of underweight individuals (BMI < 18.5): Less than 15%
# [d] Elderly individuals (aged 65 and older): Reduction in the proportion with a tendency toward undernutrition (BMI ≤ 20): Less than 13%
# IMPORTANT: We skip [c] and [d] because our IMPACT NCD-JPN has not have a structrue for association of under weight with disease incidence and mortality.

#	1) assuming decreases in BMI can achieve the target reduction in the proportion of BMI ≥ 25 among men aged 30–69 and women aged 40–69.
# 	2) assuming that apply the decrease in BMI only to men aged 30–69 and women aged 40–69 with BMI ≥ 25 (high risk approach)
#	3) to account for men aged 20-29, we used a caliblation factor based on differences in prevalence of BMI >= 25 between 30-99 yo versus 20-29 yo (=REF) from 2015 to 2019. (30-99 minus 20-29) in men.
# 	4) assuming that people with higher BMI will experience a larger decrease in BMI.
# 	5) assuming that the annual decrease in BMI is constant from 2024 to 2032 
# 	6) assuming that the decrease in BMI depends on BMI only (BMI depends on age, sex, .......) 
#	7) assuming the difference between the base-case and intervention scenarios remains after the intervention period.

#--------------------#--------------------#--------------------
#-------------------- Effect of BMI on SBP, LDL-c, and HbA1c
#--------------------#--------------------#--------------------
# Clinic SBP was reduced by 5.79 mmHg (95% CI, 3.54–8.05) per mean BMI reduction of 2.27 kg/m². 
# (Effect of Weight Loss on Blood Pressure Changes in Overweight Patients: A Systematic Review and Meta-Analysis. Yang MM, 2023, J Clin Hypertens.)
#
# We applied the pharmacotherapy effect for the mean reduction in LDL-C associated with a 1-unit reduction in BMI after 12 months: LDL-C was reduced by 7.92 mg/dL (95% CI, –9.80 to –6.04 mg/dL) per 1-unit loss in BMI (kg/m²). 
# (Weight Loss and Serum Lipids in Overweight and Obese Adults: A Systematic Review and Meta-Analysis. Bashar Hasan, 2020.)
# 
# Based on completer analysis results: mean percentage weight change was –4.0% (95% CI, –4.0 to –3.9), and mean change in POCT HbA1c was –0.19% (95% CI, –0.19 to –0.18). We used a –0.19% (95% CI, –0.19 to –0.18) reduction in HbA1c per –4.0% reduction in BMI. 
# (Early Outcomes From the English National Health Service Diabetes Prevention Programme. Valabhji, 2020, Diabetes Care.)

 

 
# apply the scenario to the synthpop datasets
IMPACTncd$update_primary_prevention_scn(function(synthpop){
	#-----------------------------
	# setting for Healthy Japan 21
	#-----------------------------
	#year
	year_base <- 2024 # R6
	year_goal <- 2032 # R14
	year_last <- 2040 # R26
	


	# SONOTE added lower_clip to proportional_reduction() to limit the lowest value after intervention
	proportional_reduction_bounded <- function(
	exposure,     # exposure: eg. SBP_curr_xps,
	change,       # average change （minus change = - X）
	weights,      # weight = age-standardised
	penalty = 1,  # penalty: determines how sharply the penalty increases with x. penalty
	lower_clip = -Inf  # the lowest value after the intervention (not delta, but value)
	) {
	# 
	wm <- weighted.mean(exposure, weights)
	# 
	c <- (wm - (wm + change)) / weighted.mean(exposure^penalty, weights)
	message("c: ", c)
	# 
	new_exposure <- exposure - c * exposure^penalty
	# 
	new_exposure <- pmax(new_exposure, lower_clip)
	# 
	return(invisible(new_exposure))
	}
		
	
	
	# high-risk approach interventions
	proportional_reduction_threshold_highrisk <-
	function(exposure, threshold_impact, threshold_target, target_prop, weights, penalty = 1, lower_clip) {
		# it only applies changes above the threshold_impacts and optimises the
		# target proportion above the threshold_target
		exposure_bounded <- exposure[exposure > threshold_impact]
		weights_bounded <- weights[exposure > threshold_impact]
		
		# Function to compute proportion above threshold given a specific change
		calc_prop_above <- function(change) {
		adj_exposure <- exposure
		adj_exposure[exposure > threshold_impact] <-
			proportional_reduction_bounded(exposure_bounded, change, weights_bounded, penalty)
		sum(weights[adj_exposure > threshold_target]) / sum(weights)
		}
	
		# Find the change required to achieve the target proportion above threshold
		optimal_change <- uniroot(
		f = function(change) calc_prop_above(change) - target_prop,
		interval = c(-max(exposure), max(exposure)),
		extendInt = "yes", tol = .Machine$double.eps, maxiter = 1000, trace = 0
		)$root
	
		message("optimal_change: ", optimal_change)
		# Apply the optimal change
		exposure[exposure > threshold_impact] <-
		proportional_reduction_bounded(exposure_bounded, optimal_change, weights_bounded, penalty)
	
		exposure <- pmax(exposure, lower_clip)
		return(exposure)    
	}
	






	#--------------------#--------------------#--------------------
	#-- preparation
    #--------------------#--------------------#--------------------
	# CKNOTE should be setkeyv(), better than setorder()
	setkeyv(synthpop$pop, c("pid", "year"))
	synthpop$pop[, BMI_curr_xps_original := copy(BMI_curr_xps), ]
	synthpop$pop[, wt_BMISc := copy(wt_esp), ] # age-standardised prevalece  NEED CHANGE


	#--------------------#--------------------#--------------------
	#-------------------- Men
	#--------------------#--------------------#--------------------
	# goal of high BMI proportion decrease
	# Find current proportion of people with BMI_curr_xps_original >= 25 taking into account weights
	# setting #1 for baseline value
	BMI_threshold_BMIsc <- 25
	threshold_impact_BMIsc <- 25

	sex_target <- "men" # require women
	age_target_BMIsc <- c(30, 69) 

	# setting #2 for baseline and target values
	target_reduction_rate <- 0.30 / ( (1 -  0.164747344) + (0.164747344 / 1.418655)) 
	# Accounting for differences in the prevalence of BMI ≥ 25 between adults aged 30–69 years 
	# and those aged 20–29 years during 2015–2019 using the ratio method. 
	# The proportion of the population aged 20–29 years among all adults aged 20–69 (0.164747344) 
	# was used together with the prevalence ratio of BMI ≥ 25 (30–69 divided by 20–29 = 1.418655) 
	# to adjust the target.
	
	# target_reduction_rate <- 0.30 + 0.164747344 * (0.1006847) 
	# Accounting for differences in the prevalence of BMI ≥ 25 between adults aged 30–69 years 
	# and those aged 20–29 years during 2015–2019 using the difference method. 
	# The proportion of the population aged 20–29 years among adults aged 20–69 (0.164747344) 
	# was multiplied by the difference in BMI ≥ 25 (30–69 minus 20–29 = 0.1006847) 
	# for men and women.
	
	# setting #3 for the function
	penalty <- 2

	# clip
	lower_clip <- synthpop$pop[sex == sex_target & between(age, age_target_BMIsc[1], age_target_BMIsc[2]), quantile(BMI_curr_xps_original, p = 0.0001)]
	


	# baseline value
	sc0_prp <- synthpop$pop[
			year == year_base & sex == sex_target & between(age, age_target_BMIsc[1], age_target_BMIsc[2]),
			sum(wt_BMISc[BMI_curr_xps_original >= BMI_threshold_BMIsc]) / sum(wt_BMISc)
		]

	#-- setting intervention (when where who what how)
    ## CKNOTE the above assumes change in year_base. Sometimes we assume no change for year_base
	# target values; Let's assume we aim that BMI >= 25 will be 30% among men aged 20–69. 
	sc1_trgt <- target_reduction_rate
		print(sc0_prp)
		print(sc1_trgt)

    effect_size_cfscenario <- data.table(
						sex = sex_target,
						year = year_base:year_last,
						delta_cfscenario = c(seq(sc0_prp, sc1_trgt, length.out = (year_goal - year_base + 1)), rep(sc1_trgt, year_last - year_goal))
                   )
	
	# effect size in the base-case scenario
	effect_size_baseSc <- synthpop$pop[
		between(year, year_base, year_last) & sex == sex_target & between(age, age_target_BMIsc[1], age_target_BMIsc[2]), 
		.(delta_base = sum(wt_BMISc[BMI_curr_xps_original >= BMI_threshold_BMIsc]) / sum(wt_BMISc)), 
		by = year
		] %>% setorder(year)

	# select lower proportion of high BMI 
	effect_size <- effect_size_cfscenario[effect_size_baseSc, on = "year"]
	effect_size[, delta := pmin(delta_base, delta_cfscenario), ]
	effect_size[, ":="(
		delta_cfscenario = NULL,	
		delta_base = NULL
		), ]
		

	# baseline padding for years before the initial year of the intevention
	synthpop$pop[, delta := NA_real_, ]

		print(synthpop$pop[, unique(delta), by = year] %>% setorder(year))
    synthpop$pop[effect_size, on = c("sex", "year"), delta := i.delta]
		print(synthpop$pop[, unique(delta), by = year] %>% setorder(year))

	
	
	#-- Perform intervention (who)	
	synthpop$pop[
		year %in% (year_base:year_last) & sex == sex_target & between(age, age_target_BMIsc[1], age_target_BMIsc[2]),	
		BMI_curr_xps := proportional_reduction_threshold_highrisk(
				exposure = BMI_curr_xps_original, 
				threshold_impact = threshold_impact_BMIsc,
				threshold_target = BMI_threshold_BMIsc, 
				target_prop = unique(delta), 
				weights = wt_BMISc, 
				penalty = penalty,
				lower_clip = lower_clip
				),
			by = year
			]
			
	synthpop$pop[
		year %in% (year_base:year_last) & sex == sex_target & between(age, age_target_BMIsc[1], age_target_BMIsc[2]), ":="(
			BMI_curr_xps = pmin(BMI_curr_xps, BMI_curr_xps_original), 
			BMI_curr_xps_mediation = copy(BMI_curr_xps_original) 
			)
		]

	
	# delete unnecessary colums and objects
	 synthpop$pop[, ":="(
	 	# BMI_curr_xps_original = NULL, 
	 	# wt_BMISc = NULL, 
	 	delta = NULL
	 	)
	 	, ]
	 
	 rm(
	 	age_target_BMIsc
	 	, BMI_threshold_BMIsc
	 	, threshold_impact_BMIsc
	 	, effect_size
	 	, effect_size_baseSc
	 	, effect_size_cfscenario
	 	, lower_clip
	 	, penalty
	 	, sc0_prp
	 	, sc1_trgt
	 	, sex_target
	 	, target_reduction_rate                                      
	 	, tmp
	 	, tmp1
	 	, tmp2
	 )




	#--------------------#--------------------#--------------------
	#-------------------- Women
	#--------------------#--------------------#--------------------
	
	#-- preparation
	# goal of high BMI proportion decrease
	# Find current proportion of people with BMI_curr_xps_original >= 25 taking into account weights
	# setting #1 for baseline value
	BMI_threshold_BMIsc <- 25
	threshold_impact_BMIsc <- 25

	sex_target <- "women" # require women
	age_target_BMIsc <- c(40, 69)

	# setting #2 for baseline and target values
	target_reduction_rate <- 0.15 # requre men women
		
	# setting #3 for the function
	penalty <- 2
	
	# limit
	lower_clip <- synthpop$pop[sex == sex_target & between(age, age_target_BMIsc[1], age_target_BMIsc[2]), quantile(BMI_curr_xps_original, p = 0.0001)]
	


	# baseline value
	sc0_prp <- synthpop$pop[
			year == year_base & sex == sex_target & between(age, age_target_BMIsc[1], age_target_BMIsc[2]),
			sum(wt_BMISc[BMI_curr_xps_original >= BMI_threshold_BMIsc]) / sum(wt_BMISc)
		]

	#-- setting intervention (when where who what how)
    ## CKNOTE the above assumes change in year_base. Sometimes we assume no change for year_base
	# target values; Let's assume we aim that BMI >= 25 will be 30% among men aged 20–69. 
	sc1_trgt <- target_reduction_rate
		print(sc0_prp)
		print(sc1_trgt)

    effect_size_cfscenario <- data.table(
						sex = sex_target,
						year = year_base:year_last,
						delta_cfscenario = c(seq(sc0_prp, sc1_trgt, length.out = (year_goal - year_base + 1)), rep(sc1_trgt, year_last - year_goal))
                   )
	
	# effect size in the base-case scenario
	effect_size_baseSc <- synthpop$pop[
		between(year, year_base, year_last) & sex == sex_target & between(age, age_target_BMIsc[1], age_target_BMIsc[2]), 
		.(delta_base = sum(wt_BMISc[BMI_curr_xps_original >= BMI_threshold_BMIsc]) / sum(wt_BMISc)), 
		by = year
		] %>% setorder(year)

	# select lower proportion of high BMI 
	effect_size <- effect_size_cfscenario[effect_size_baseSc, on = "year"]
	effect_size[, delta := pmin(delta_base, delta_cfscenario), ]
	effect_size[, ":="(
		delta_cfscenario = NULL,	
		delta_base = NULL
		), ]
		

	# baseline padding for years before the initial year of the intevention
	synthpop$pop[, delta := NA_real_, ]

		print(synthpop$pop[, unique(delta), by = year] %>% setorder(year))
    synthpop$pop[effect_size, on = c("sex", "year"), delta := i.delta]
		print(synthpop$pop[, unique(delta), by = year] %>% setorder(year))

	
	
	#-- Perform intervention (who)	
	synthpop$pop[
		year %in% (year_base:year_last) & sex == sex_target & between(age, age_target_BMIsc[1], age_target_BMIsc[2]),	
		BMI_curr_xps := proportional_reduction_threshold_highrisk(
				exposure = BMI_curr_xps_original, 
				threshold_impact = threshold_impact_BMIsc,
				threshold_target = BMI_threshold_BMIsc, 
				target_prop = unique(delta), 
				weights = wt_BMISc, 
				penalty = penalty,
				lower_clip = lower_clip
				),
			by = year
			]
			
	synthpop$pop[
		year %in% (year_base:year_last) & sex == sex_target & between(age, age_target_BMIsc[1], age_target_BMIsc[2]), ":="(
			BMI_curr_xps = pmin(BMI_curr_xps, BMI_curr_xps_original), 
			BMI_curr_xps_mediation = copy(BMI_curr_xps_original) 
			)
		]
	
	
	# delete unnecessary colums and objects
	 synthpop$pop[, ":="(
	 	BMI_curr_xps_original = NULL, 
	 	wt_BMISc = NULL, 
	 	delta = NULL
	 	)
	 	, ]
	 
	 rm(
	 	age_target_BMIsc
	 	, BMI_threshold_BMIsc
	 	, threshold_impact_BMIsc
	 	, effect_size
	 	, effect_size_baseSc
	 	, effect_size_cfscenario
	 	, lower_clip
	 	, penalty
	 	, sc0_prp
	 	, sc1_trgt
	 	, sex_target
	 	, target_reduction_rate                                      
	 	, tmp
	 	, tmp1
	 	, tmp2
	 )








#--------------------#--------------------#--------------------
#-------------------- Effect of BMI on SBP, LDL-c, and HbA1c
#--------------------#--------------------#--------------------
    

	

	
	# how much: set effect size of BMI (1 kg/m^2) on SBP 
    sbp_se  <- (8.05 - 3.54)/(2 * (qnorm(0.975)))
    sbp_effect <- rnorm(1, mean = -5.79, sd = sbp_se) / 2.27 #change provided is for 2.27 decrease in BMI
    sbp_effect[sbp_effect > 0] <- 0 # make sure effect is nor reversed
	
	
    # how much: set effect size of BMI (1 kg/m^2) on LDLc 
    ldlc_se  <- (-6.04 - (-9.80))/(2*(qnorm(0.975)))
    ldl_effect <- rnorm(1, mean = -7.92, ldlc_se)
    ldl_effect[ldl_effect > 0] <- 0 # make sure effect is nor reversed
	

    # how much: set effect size of BMI (1 percentage change) on HbA1c 
    hba1c_se  <- (-0.18 - (-0.19))/(2*(qnorm(0.975)))
    hba1c_effect <- rnorm(1, mean = -0.19, hba1c_se) / 4 # The effect size was based on the change in HbA1c (%) per a 4% reduction in BMI.
    hba1c_effect[hba1c_effect > 0] <- 0 # make sure effect is nor reversed






	#--------------------#--------------------#--------------------
	#-------------------- Men
	#--------------------#--------------------#--------------------
	#-- preparation
	# setting
	BMI_threshold_BMIsc <- 25
	sex_target <- "men"
	age_target_BMIsc <- c(30, 69) 

	
	# calculate changes in BMI by Health Japan 21 for people who were effected by BMI scenario of Health Japan 21
	setkeyv(synthpop$pop, c("pid", "year"))
	synthpop$pop[
		year %in% (year_base:year_last) & sex == sex_target & between(age, age_target_BMIsc[1], age_target_BMIsc[2]) & BMI_curr_xps_mediation >= BMI_threshold_BMIsc, 
		":="(
			bmi_change = BMI_curr_xps - BMI_curr_xps_mediation, 
			bmi_percentage_change = (BMI_curr_xps - BMI_curr_xps_mediation) / BMI_curr_xps_mediation * 100 #(Valabhji et al. Diabetes Care 2020; https://doi.org/10.2337/dc19-1425) For completers, the mean baseline weight was 82.4 kg with a mean weight change of -3.3 kg (95% CI -3.4, -3.2). The mean percentage weight change was -4.0% (-4.0, -3.9) ==> (-3.3 / 82.4) * 100 = -4.004854 ==== changes in weight / baseline weight
			)
		, ]
			


	#　Assign mediation effect of changes in BMI on SBP, LDL-c, and HbA1c.
    synthpop$pop[
		year %in% (year_base:year_last) & sex == sex_target & between(age, age_target_BMIsc[1], age_target_BMIsc[2]) & BMI_curr_xps_mediation >= BMI_threshold_BMIsc, 
		":="(
			SBP_curr_xps = SBP_curr_xps - (bmi_change * sbp_effect),  
			LDLc_curr_xps = LDLc_curr_xps - (bmi_change * ldl_effect),
			HbA1c_curr_xps = HbA1c_curr_xps - (bmi_percentage_change * hba1c_effect)
			)
		, ]



	#--------------------#--------------------#--------------------
	#-------------------- Women
	#--------------------#--------------------#--------------------
	#-- preparation
	# setting
	BMI_threshold_BMIsc <- 25
	sex_target <- "women"
	age_target_BMIsc <- c(40, 69) 
	
	# calculate changes in BMI by Health Japan 21 for people who were effected by BMI scenario of Health Japan 21
	setkeyv(synthpop$pop, c("pid", "year"))
	synthpop$pop[
		year %in% (year_base:year_last) & sex == sex_target & between(age, age_target_BMIsc[1], age_target_BMIsc[2]) & BMI_curr_xps_mediation >= BMI_threshold_BMIsc, 
		":="(
			bmi_change = BMI_curr_xps - BMI_curr_xps_mediation, 
			bmi_percentage_change = (BMI_curr_xps - BMI_curr_xps_mediation) / BMI_curr_xps_mediation * 100 #(Valabhji et al. Diabetes Care 2020; https://doi.org/10.2337/dc19-1425) For completers, the mean baseline weight was 82.4 kg with a mean weight change of -3.3 kg (95% CI -3.4, -3.2). The mean percentage weight change was -4.0% (-4.0, -3.9) ==> (-3.3 / 82.4) * 100 = -4.004854 ==== changes in weight / baseline weight
			)
		, ]
			

	#　Assign mediation effect of changes in BMI on SBP, LDL-c, and HbA1c.
    synthpop$pop[
		year %in% (year_base:year_last) & sex == sex_target & between(age, age_target_BMIsc[1], age_target_BMIsc[2]) & BMI_curr_xps_mediation >= BMI_threshold_BMIsc, 
		":="(
			SBP_curr_xps = SBP_curr_xps - (bmi_change * sbp_effect),  
			LDLc_curr_xps = LDLc_curr_xps - (bmi_change * ldl_effect),
			HbA1c_curr_xps = HbA1c_curr_xps - (bmi_percentage_change * hba1c_effect)
			)
		, ]


					
	# delete unnecessary colums and objects
	 synthpop$pop[, ":="(
	 	BMI_curr_xps_mediation = NULL
	 	, bmi_percentage_change = NULL 
	 	, bmi_change = NULL
	 	)
	 	, ]

	 rm(
	 	age_target_BMIsc
	 	, BMI_threshold_BMIsc
	 	, sex_target
	 	, hba1c_effect
	 	, hba1c_se
	 	, ldl_effect
	 	, ldlc_se
	 	, sbp_effect
	 	, sbp_se
	 )

 


})
		
IMPACTncd$
  run(1:mcnum, multicore = TRUE, "BMI")









#-----------------------------
# The all-combined scenario
#-----------------------------
# apply the scenario to the synthpop datasets
IMPACTncd$update_primary_prevention_scn(function(synthpop){



	#-----------------------------
	# setting for Healthy Japan 21
	#-----------------------------
	#year
	year_base <- 2024 # R6
	year_goal <- 2032 # R14
	year_last <- 2040 # R26



#-----------------------------
# Scenario for smoking
#-----------------------------
#--------------------#--------------------#--------------------
#-------------------- Effect of smoking cessation on BMI
#--------------------#--------------------#--------------------


	# goal of smoking proportion decrease
	# Find current proportion of people with current smokier
	# setting #1 for baseline value
	age_target_smokingsc <- 30
	
	# setting #2 for baseline and target values
	target_reduction_rate <- 0.12 / ( (1 -  0.115822198) + (0.115822198 / 0.9805446))
	# Accounting for differences in the prevalence of current smoking between adults aged 30+ years 
	# and those aged 20–29 years from 2015 to 2019 using the ratio method. 
	# The proportion of the population aged 20–29 years among all adults aged 20 and older (0.115822198) 
	# was used together with the prevalence ratio of current smoking (30+ divided by 20–29 = 0.9805446) 
	# to adjust the target.
	
	# target_reduction_rate <- 0.12 + 0.115822198 * (-0.003533689) 
	# Accounting for differences in the prevalence of current smoking between adults aged 30+ years 
	# and those aged 20–29 years from 2015 to 2019 using the difference method. 
	# The proportion of the population aged 20–29 years among all adults aged 20 and older (0.115822198) 
	# was multiplied by the difference in current smoking prevalence (30+ minus 20–29 = -0.003533689).



	#-- preparation
    # CKNOTE should be setkeyv(), better than setorder()
	setkeyv(synthpop$pop, c("pid", "year"))
	synthpop$pop[, Smoking_curr_xps_original := copy(Smoking_curr_xps), ]
	synthpop$pop[, Smoking_number_curr_xps_original := copy(Smoking_number_curr_xps), ]
	synthpop$pop[, BMI_curr_xps_original := copy(BMI_curr_xps), ] #changed for combined scenario
	synthpop$pop[, wt_smokingsc := wt, ] # NOT age-standardised


	# baseline value: we defined the value in 2024
	# baseline value
	sc0_prp <- synthpop$pop[
			year == year_base & age >= age_target_smokingsc,
			sum(wt_smokingsc[Smoking_curr_xps_original == 3]) / sum(wt_smokingsc)
		]		
		#synthpop$pop[sex == "men", table(Smoking_curr_xps), ]
		#synthpop$pop[sex == "women", table(Smoking_curr_xps), ]


	#-- setting intervention (when where who what how)
    ## CKNOTE the above assumes change in year_base. Sometimes we assume no change for year_base
	# target values; Let's assume we aim target_reduction_rate (around 12%) of people with smoking_curr_xps_original == 1. The scenario target should then be
	sc1_trgt <- (sc0_prp - target_reduction_rate) / sc0_prp
		print(sc0_prp)
		print(sc1_trgt)
		(sc0_prp - (sc0_prp * sc1_trgt)) == target_reduction_rate
		(sc0_prp - (sc0_prp * sc1_trgt)) 
		target_reduction_rate

    effect_size_cfscenario <- data.table(
						year = year_base:year_last,
						delta_cfscenario = c(seq(0, sc1_trgt, length.out = (year_goal - year_base + 1)), rep(sc1_trgt, year_last - year_goal))
                   )
	
	effect_size_cfscenario[, smoking_prevalence_cfscenario := sc0_prp - (sc0_prp * delta_cfscenario), ]
				   
	# effect size in the base-case scenario
	effect_size_baseSc <- synthpop$pop[
		age >= age_target_smokingsc & between(year, year_base, year_last), 
		.(smoking_prevalence_base = sum(wt_smokingsc[Smoking_curr_xps_original == 3]) / sum(wt_smokingsc)), 
		by = year
		] %>% setorder(year)
		
	effect_size_baseSc[, delta_base := (sc0_prp - smoking_prevalence_base) / sc0_prp, ]
	

	# select larger probability of smoking cessesioin in current smoking prevalence 
	effect_size <- effect_size_cfscenario[effect_size_baseSc, on = "year"]
	effect_size[, delta := pmax(delta_base, delta_cfscenario), ]
	effect_size[, ":="(
		smoking_prevalence_cfscenario = NULL,	
		smoking_prevalence_base = NULL,	
		delta_cfscenario = NULL,	
		delta_base = NULL
		), ]				   
				   

	# baseline padding for years before the initial year of the intevention
	synthpop$pop[, delta := NA_real_, ]

		print(synthpop$pop[, unique(delta), by = year] %>% setorder(year))
    synthpop$pop[effect_size, on = 'year', delta := i.delta]
		print(synthpop$pop[, unique(delta), by = year] %>% setorder(year))

	
	
	#-- Perform intervention (who)
	rs <- .Random.seed # it saves the current random seed. This is recovered after getting "Smoking_stop" and "bmi_effect".
	set.seed(4200L + synthpop$mc)

	synthpop$pop[age >= age_target_smokingsc & Smoking_curr_xps_original == 3 & year %in% (year_base:year_last),
		Smoking_stop := rbinom(n = .N, size = 1, prob = delta),
			#by = year
			]
	
	set.seed(rs)
	
	
	synthpop$pop[Smoking_stop == 1, Smoking_curr_xps := 2, ]
	synthpop$pop[Smoking_stop == 1, Smoking_number_curr_xps := NA, ]

	
	
	# delete unnecessary colums and objects
	rm(
	effect_size
	, effect_size_baseSc
	, effect_size_cfscenario
	, sc0_prp
	, sc1_trgt
	, tmp
	, tmp1
	, tmp2
	, target_reduction_rate
	)





#--------------------#--------------------#--------------------
#-------------------- Effect of smoking cessation on BMI
#--------------------#--------------------#--------------------

    # how much: set effect size of smoking cessation on BMI (kg/m^2)  
    bmi_se  <- (1.79 - 0.50)/(2 * (qnorm(0.975)))
    bmi_effect <- rnorm(1, mean = 1.14, sd = bmi_se) / 4.84 #change provided is for 4.84 years from smoking cessation. Thus, this effect is devided by 4.84
    bmi_effect[bmi_effect < 0] <- 0 # make sure effect is nor reversed

	

	
	# Calculate the cumulative years of smoking cessation according to the Health Japan 21 smoking scenario. If smoking is resumed, subtract the corresponding years from the cumulative total.
	setkeyv(synthpop$pop, c("pid","year"))
	synthpop$pop[
		is.na(Smoking_stop) == FALSE, 
		Smoking_score := {
			# initial score = the first value of Smoking_stop (0 or 1)
			init <- Smoking_stop[1L]
			# step: +1 if Smoking_stop==1, -1 if Smoking_stop==0
			step <- ifelse(Smoking_stop == 1L, 1L, -1L)
			# for the very first row, keep the initial score (no increment/decrement)
			step[1L] <- 0L
			# cumulative process: update score at each step and clamp it within [0, 5]
			Reduce(
				f = function(previous_score, delta){
					# update score = previous score + change
					updated <- previous_score + delta
					# clamp the score to the range [0, 5]
					bounded <- pmin(5L, pmax(0L, updated))
					return(bounded)
				},
				x = step,
				init = init,
				accumulate = TRUE
			)[-1]  # drop the initial value from the result
	}, 
	by = pid]



	# Assign mediation effect of smoking cessation on BMI increase.
    synthpop$pop[
		is.na(Smoking_stop) == FALSE, 
		":="(
			BMI_curr_xps = BMI_curr_xps_original + (Smoking_score * bmi_effect)#changed for combined scenario
			)
		, ]



	synthpop$pop[, ":="(
		# Smoking_curr_xps_original = NULL #changed for combined scenario
		# , Smoking_number_curr_xps_original = NULL #changed for combined scenario
		# , wt_smokingsc = NULL #changed for combined scenario
		delta = NULL
		# , Smoking_stop = NULL #changed for combined scenario
		# , Smoking_score = NULL #changed for combined scenario
		# , BMI_curr_xps_med = NULL #changed for combined scenario
		# , BMI_curr_xps_original = NULL #changed for combined scenario
		)
		, ]

	# delete unnecessary colums and objects
	rm(
	effect_size
	, effect_size_baseSc
	, effect_size_cfscenario
	, sc0_prp
	, sc1_trgt
	, tmp
	, tmp1
	, tmp2
	, target_reduction_rate
	, age_target_smokingsc
	, bmi_effect
	, bmi_se
	)



print("FINISHED Smoking scenario")
#fwrite(synthpop$pop, "./fin_smk.csv")



#-----------------------------
# Scenario for BMI
#-----------------------------
#--------------------#--------------------#--------------------
#-------------------- Effect of BMI on SBP, LDL-c, and HbA1c
#--------------------#--------------------#--------------------
	# SONOTE added lower_clip to proportional_reduction() to limit the lowest value after intervention
	proportional_reduction_bounded <- function(
	exposure,     # exposure: eg. SBP_curr_xps,
	change,       # average change （minus change = - X）
	weights,      # weight = age-standardised
	penalty = 1,  # penalty: determines how sharply the penalty increases with x. penalty
	lower_clip = -Inf  # the lowest value after the intervention (not delta, but value)
	) {
	# 
	wm <- weighted.mean(exposure, weights)
	# 
	c <- (wm - (wm + change)) / weighted.mean(exposure^penalty, weights)
	message("c: ", c)
	# 
	new_exposure <- exposure - c * exposure^penalty
	# 
	new_exposure <- pmax(new_exposure, lower_clip)
	# 
	return(invisible(new_exposure))
	}
		
	
	
	# high-risk approach interventions
	proportional_reduction_threshold_highrisk <-
	function(exposure, threshold_impact, threshold_target, target_prop, weights, penalty = 1, lower_clip) {
		# it only applies changes above the threshold_impacts and optimises the
		# target proportion above the threshold_target
		exposure_bounded <- exposure[exposure > threshold_impact]
		weights_bounded <- weights[exposure > threshold_impact]
		
		# Function to compute proportion above threshold given a specific change
		calc_prop_above <- function(change) {
		adj_exposure <- exposure
		adj_exposure[exposure > threshold_impact] <-
			proportional_reduction_bounded(exposure_bounded, change, weights_bounded, penalty)
		sum(weights[adj_exposure > threshold_target]) / sum(weights)
		}
	
		# Find the change required to achieve the target proportion above threshold
		optimal_change <- uniroot(
		f = function(change) calc_prop_above(change) - target_prop,
		interval = c(-max(exposure), max(exposure)),
		extendInt = "yes", tol = .Machine$double.eps, maxiter = 1000, trace = 0
		)$root
	
		message("optimal_change: ", optimal_change)
		# Apply the optimal change
		exposure[exposure > threshold_impact] <-
		proportional_reduction_bounded(exposure_bounded, optimal_change, weights_bounded, penalty)
	
		exposure <- pmax(exposure, lower_clip)
		return(exposure)    
	}
	





	#--------------------#--------------------#--------------------
	#-- preparation
    #--------------------#--------------------#--------------------
	# CKNOTE should be setkeyv(), better than setorder()
	setkeyv(synthpop$pop, c("pid", "year"))
	synthpop$pop[, BMI_curr_xps_original_after_smoking := copy(BMI_curr_xps), ] #changed for combined scenario
	synthpop$pop[, SBP_curr_xps_original := copy(SBP_curr_xps), ] #changed for combined scenario
	synthpop$pop[, LDLc_curr_xps_original := copy(LDLc_curr_xps), ] #changed for combined scenario
	synthpop$pop[, HbA1c_curr_xps_original := copy(HbA1c_curr_xps), ] #changed for combined scenario
	synthpop$pop[, wt_BMISc := copy(wt_esp), ] # age-standardised prevalece  NEED CHANGE



	#--------------------#--------------------#--------------------
	#-------------------- Men
	#--------------------#--------------------#--------------------
	# goal of high BMI proportion decrease
	# Find current proportion of people with BMI_curr_xps_original >= 25 taking into account weights
	# setting #1 for baseline value
	BMI_threshold_BMIsc <- 25
	threshold_impact_BMIsc <- 25

	sex_target <- "men" # require women
	age_target_BMIsc <- c(30, 69) 

	# setting #2 for baseline and target values
	target_reduction_rate <- 0.30 / ( (1 -  0.164747344) + (0.164747344 / 1.418655)) 
	# Accounting for differences in the prevalence of BMI ≥ 25 between adults aged 30–69 years 
	# and those aged 20–29 years during 2015–2019 using the ratio method. 
	# The proportion of the population aged 20–29 years among all adults aged 20–69 (0.164747344) 
	# was used together with the prevalence ratio of BMI ≥ 25 (30–69 divided by 20–29 = 1.418655) 
	# to adjust the target.
	
	# target_reduction_rate <- 0.30 + 0.164747344 * (0.1006847) 
	# Accounting for differences in the prevalence of BMI ≥ 25 between adults aged 30–69 years 
	# and those aged 20–29 years during 2015–2019 using the difference method. 
	# The proportion of the population aged 20–29 years among adults aged 20–69 (0.164747344) 
	# was multiplied by the difference in BMI ≥ 25 (30–69 minus 20–29 = 0.1006847) 
	# for men and women.
	
	# setting #3 for the function
	penalty <- 2

	# limit
	lower_clip <- synthpop$pop[sex == sex_target & between(age, age_target_BMIsc[1], age_target_BMIsc[2]), quantile(BMI_curr_xps_original, p = 0.0001)]
	


	# baseline value
	sc0_prp <- synthpop$pop[
			year == year_base & sex == sex_target & between(age, age_target_BMIsc[1], age_target_BMIsc[2]),
			sum(wt_BMISc[BMI_curr_xps_original >= BMI_threshold_BMIsc]) / sum(wt_BMISc)
		]

	#-- setting intervention (when where who what how)
    ## CKNOTE the above assumes change in year_base. Sometimes we assume no change for year_base
	# target values; Let's assume we aim that BMI >= 25 will be 30% among men aged 20–69. 
	sc1_trgt <- target_reduction_rate
		print(sc0_prp)
		print(sc1_trgt)

    effect_size_cfscenario <- data.table(
						sex = sex_target,
						year = year_base:year_last,
						delta_cfscenario = c(seq(sc0_prp, sc1_trgt, length.out = (year_goal - year_base + 1)), rep(sc1_trgt, year_last - year_goal))
                   )
	
	# effect size in the base-case scenario
	effect_size_baseSc <- synthpop$pop[
		between(year, year_base, year_last) & sex == sex_target & between(age, age_target_BMIsc[1], age_target_BMIsc[2]), 
		.(delta_base = sum(wt_BMISc[BMI_curr_xps_original >= BMI_threshold_BMIsc]) / sum(wt_BMISc)), 
		by = year
		] %>% setorder(year)

	# select lower proportion of high BMI 
	effect_size <- effect_size_cfscenario[effect_size_baseSc, on = "year"]
	effect_size[, delta := pmin(delta_base, delta_cfscenario), ]
	effect_size[, ":="(
		delta_cfscenario = NULL,	
		delta_base = NULL
		), ]
		

	# baseline padding for years before the initial year of the intevention
	synthpop$pop[, delta := NA_real_, ]

		print(synthpop$pop[, unique(delta), by = year] %>% setorder(year))
    synthpop$pop[effect_size, on = c("sex", "year"), delta := i.delta]
		print(synthpop$pop[, unique(delta), by = year] %>% setorder(year))

	
	
	#-- Perform intervention (who)	
	synthpop$pop[
		year %in% (year_base:year_last) & sex == sex_target & between(age, age_target_BMIsc[1], age_target_BMIsc[2]),	
		BMI_curr_xps := proportional_reduction_threshold_highrisk(
				exposure = BMI_curr_xps_original_after_smoking, #changed for combined scenario 
				threshold_impact = threshold_impact_BMIsc,
				threshold_target = BMI_threshold_BMIsc, 
				target_prop = unique(delta), 
				weights = wt_BMISc, 
				penalty = penalty,
				lower_clip = lower_clip
				),
			by = year
			]
			
	synthpop$pop[
		year %in% (year_base:year_last) & sex == sex_target & between(age, age_target_BMIsc[1], age_target_BMIsc[2]), ":="(
			BMI_curr_xps = pmin(BMI_curr_xps, BMI_curr_xps_original_after_smoking), #changed for combined scenario
			BMI_curr_xps_mediation = copy(BMI_curr_xps_original_after_smoking) #changed for combined scenario
			)
		]

	
	# delete unnecessary colums and objects
	 synthpop$pop[, ":="(
	 	#BMI_curr_xps_original_after_smoking = NULL, #changed for combined scenario
	 	#wt_BMISc = NULL, 
	 	delta = NULL
	 	)
	 	, ]
	 
	 rm(
	 	age_target_BMIsc
	 	, BMI_threshold_BMIsc
	 	, threshold_impact_BMIsc
	 	, effect_size
	 	, effect_size_baseSc
	 	, effect_size_cfscenario
	 	, lower_clip
	 	, penalty
	 	, sc0_prp
	 	, sc1_trgt
	 	, sex_target
	 	, target_reduction_rate                                      
	 	, tmp
	 	, tmp1
	 	, tmp2
	 )


print("FINISHED BMI scenario for men")
#fwrite(synthpop$pop, "./fin_bmi_men.csv")


	#--------------------#--------------------#--------------------
	#-------------------- Women
	#--------------------#--------------------#--------------------
	
	#-- preparation
    # CKNOTE should be setkeyv(), better than setorder()
	setkeyv(synthpop$pop, c("pid", "year"))


	# goal of high BMI proportion decrease
	# Find current proportion of people with BMI_curr_xps_original >= 25 taking into account weights
	# setting #1 for baseline value
	BMI_threshold_BMIsc <- 25
	threshold_impact_BMIsc <- 25

	sex_target <- "women" # require women
	age_target_BMIsc <- c(40, 69)

	# setting #2 for baseline and target values
	target_reduction_rate <- 0.15 # requre men women
		
	# setting #3 for the function
	penalty <- 2

	# limit
	lower_clip <- synthpop$pop[sex == sex_target & between(age, age_target_BMIsc[1], age_target_BMIsc[2]), quantile(BMI_curr_xps_original, p = 0.0001)]
	


	# baseline value
	sc0_prp <- synthpop$pop[
			year == year_base & sex == sex_target & between(age, age_target_BMIsc[1], age_target_BMIsc[2]),
			sum(wt_BMISc[BMI_curr_xps_original >= BMI_threshold_BMIsc]) / sum(wt_BMISc)
		]

	#-- setting intervention (when where who what how)
    ## CKNOTE the above assumes change in year_base. Sometimes we assume no change for year_base
	# target values; Let's assume we aim that BMI >= 25 will be 30% among men aged 20–69. 
	sc1_trgt <- target_reduction_rate
		print(sc0_prp)
		print(sc1_trgt)

    effect_size_cfscenario <- data.table(
						sex = sex_target,
						year = year_base:year_last,
						delta_cfscenario = c(seq(sc0_prp, sc1_trgt, length.out = (year_goal - year_base + 1)), rep(sc1_trgt, year_last - year_goal))
                   )
	
	# effect size in the base-case scenario
	effect_size_baseSc <- synthpop$pop[
		between(year, year_base, year_last) & sex == sex_target & between(age, age_target_BMIsc[1], age_target_BMIsc[2]), 
		.(delta_base = sum(wt_BMISc[BMI_curr_xps_original >= BMI_threshold_BMIsc]) / sum(wt_BMISc)), 
		by = year
		] %>% setorder(year)

	# select lower proportion of high BMI 
	effect_size <- effect_size_cfscenario[effect_size_baseSc, on = "year"]
	effect_size[, delta := pmin(delta_base, delta_cfscenario), ]
	effect_size[, ":="(
		delta_cfscenario = NULL,	
		delta_base = NULL
		), ]
		

	# baseline padding for years before the initial year of the intevention
	synthpop$pop[, delta := NA_real_, ]

		print(synthpop$pop[, unique(delta), by = year] %>% setorder(year))
    synthpop$pop[effect_size, on = c("sex", "year"), delta := i.delta]
		print(synthpop$pop[, unique(delta), by = year] %>% setorder(year))

	
	
	#-- Perform intervention (who)	
	synthpop$pop[
		year %in% (year_base:year_last) & sex == sex_target & between(age, age_target_BMIsc[1], age_target_BMIsc[2]),	
		BMI_curr_xps := proportional_reduction_threshold_highrisk(
				exposure = BMI_curr_xps_original_after_smoking, #changed for combined scenario 
				threshold_impact = threshold_impact_BMIsc,
				threshold_target = BMI_threshold_BMIsc, 
				target_prop = unique(delta), 
				weights = wt_BMISc, 
				penalty = penalty,
				lower_clip = lower_clip
				),
			by = year
			]
			
	synthpop$pop[
		year %in% (year_base:year_last) & sex == sex_target & between(age, age_target_BMIsc[1], age_target_BMIsc[2]), ":="(
			BMI_curr_xps = pmin(BMI_curr_xps, BMI_curr_xps_original_after_smoking), #changed for combined scenario 
			BMI_curr_xps_mediation = copy(BMI_curr_xps_original_after_smoking)  #changed for combined scenario 
			)
		]
	
	
	# delete unnecessary colums and objects
	 synthpop$pop[, ":="(
	 	#BMI_curr_xps_original = NULL, 
	 	#wt_BMISc = NULL, 
	 	delta = NULL
	 	)
	 	, ]
	 
	 rm(
	 	age_target_BMIsc
	 	, BMI_threshold_BMIsc
	 	, threshold_impact_BMIsc
	 	, effect_size
	 	, effect_size_baseSc
	 	, effect_size_cfscenario
	 	, lower_clip
	 	, penalty
	 	, sc0_prp
	 	, sc1_trgt
	 	, sex_target
	 	, target_reduction_rate                                      
	 	, tmp
	 	, tmp1
	 	, tmp2
	 )


print("FINISHED BMI scenario for women")
#fwrite(synthpop$pop, "./fin_smk_women.csv")




#--------------------#--------------------#--------------------
#-------------------- Effect of BMI on SBP, LDL-c, and HbA1c
#--------------------#--------------------#--------------------



    # how much: set effect size of BMI (1 kg/m^2) on SBP 
    sbp_se  <- (8.05 - 3.54)/(2 * (qnorm(0.975)))
    sbp_effect <- rnorm(1, mean = -5.79, sd = sbp_se) / 2.27 #change provided is for 2.27 decrease in BMI
    sbp_effect[sbp_effect > 0] <- 0 # make sure effect is nor reversed
	
	
    # how much: set effect size of BMI (1 kg/m^2) on LDLc 
    ldlc_se  <- (-6.04 - (-9.80))/(2*(qnorm(0.975)))
    ldl_effect <- rnorm(1, mean = -7.92, ldlc_se)
    ldl_effect[ldl_effect > 0] <- 0 # make sure effect is nor reversed
	

    # how much: set effect size of BMI (1 percentage change) on HbA1c 
    hba1c_se  <- (-0.18 - (-0.19))/(2*(qnorm(0.975)))
    hba1c_effect <- rnorm(1, mean = -0.19, hba1c_se) / 4 # The effect size was based on the change in HbA1c (%) per a 4% reduction in BMI.
    hba1c_effect[hba1c_effect > 0] <- 0 # make sure effect is nor reversed
	
	


	#--------------------#--------------------#--------------------
	#-------------------- Men
	#--------------------#--------------------#--------------------
	#-- preparation
	# setting
	BMI_threshold_BMIsc <- 25
	sex_target <- "men"
	age_target_BMIsc <- c(30, 69) 
	
	# calculate changes in BMI by Health Japan 21 for people who were effected by BMI scenario of Health Japan 21
	setkeyv(synthpop$pop, c("pid", "year"))
	synthpop$pop[
		year %in% (year_base:year_last) & sex == sex_target & between(age, age_target_BMIsc[1], age_target_BMIsc[2]) & BMI_curr_xps_mediation >= BMI_threshold_BMIsc, 
		":="(
			bmi_change = BMI_curr_xps - BMI_curr_xps_mediation, 
			bmi_percentage_change = (BMI_curr_xps - BMI_curr_xps_mediation) / BMI_curr_xps_mediation * 100 #(Valabhji et al. Diabetes Care 2020; https://doi.org/10.2337/dc19-1425) For completers, the mean baseline weight was 82.4 kg with a mean weight change of -3.3 kg (95% CI -3.4, -3.2). The mean percentage weight change was -4.0% (-4.0, -3.9) ==> (-3.3 / 82.4) * 100 = -4.004854 ==== changes in weight / baseline weight
			)
		, ]
			


	# Assign mediation effect of changes in BMI on SBP, LDL-c, and HbA1c.
    synthpop$pop[
		year %in% (year_base:year_last) & sex == sex_target & between(age, age_target_BMIsc[1], age_target_BMIsc[2]) & BMI_curr_xps_mediation >= BMI_threshold_BMIsc, 
		":="(
			SBP_curr_xps = SBP_curr_xps - (bmi_change * sbp_effect),  
			LDLc_curr_xps = LDLc_curr_xps - (bmi_change * ldl_effect),
			HbA1c_curr_xps = HbA1c_curr_xps - (bmi_percentage_change * hba1c_effect)
			)
		, ]


print("FINISHED BMI scenario mediation for men")
#fwrite(synthpop$pop, "./fin_bmi_med_men.csv")

	#--------------------#--------------------#--------------------
	#-------------------- Women
	#--------------------#--------------------#--------------------
	#-- preparation
	# setting
	BMI_threshold_BMIsc <- 25
	sex_target <- "women"
	age_target_BMIsc <- c(40, 69) 
	
	# calculate changes in BMI by Health Japan 21 for people who were effected by BMI scenario of Health Japan 21
	setkeyv(synthpop$pop, c("pid", "year"))
	synthpop$pop[
		year %in% (year_base:year_last) & sex == sex_target & between(age, age_target_BMIsc[1], age_target_BMIsc[2]) & BMI_curr_xps_mediation >= BMI_threshold_BMIsc, 
		":="(
			bmi_change = BMI_curr_xps - BMI_curr_xps_mediation, 
			bmi_percentage_change = (BMI_curr_xps - BMI_curr_xps_mediation) / BMI_curr_xps_mediation * 100 #(Valabhji et al. Diabetes Care 2020; https://doi.org/10.2337/dc19-1425) For completers, the mean baseline weight was 82.4 kg with a mean weight change of -3.3 kg (95% CI -3.4, -3.2). The mean percentage weight change was -4.0% (-4.0, -3.9) ==> (-3.3 / 82.4) * 100 = -4.004854 ==== changes in weight / baseline weight
			)
		, ]
			

	#Assign mediation effect of changes in BMI on SBP, LDL-c, and HbA1c.
    synthpop$pop[
		year %in% (year_base:year_last) & sex == sex_target & between(age, age_target_BMIsc[1], age_target_BMIsc[2]) & BMI_curr_xps_mediation >= BMI_threshold_BMIsc, 
		":="(
			SBP_curr_xps = SBP_curr_xps - (bmi_change * sbp_effect),  
			LDLc_curr_xps = LDLc_curr_xps - (bmi_change * ldl_effect),
			HbA1c_curr_xps = HbA1c_curr_xps - (bmi_percentage_change * hba1c_effect)
			)
		, ]


					
	# delete unnecessary colums and objects
	# synthpop$pop[, ":="(
	# 	BMI_curr_xps_mediation = NULL
	# 	, bmi_percentage_change = NULL 
	# 	, bmi_change = NULL
	# 	)
	# 	, ]

	 rm(
	 	age_target_BMIsc
	 	, BMI_threshold_BMIsc
	 	, sex_target
	 	, hba1c_effect
	 	, hba1c_se
	 	, ldl_effect
	 	, ldlc_se
	 	, sbp_effect
	 	, sbp_se
	 )

 


print("FINISHED BMI scenario mediation for women")
#fwrite(synthpop$pop, "./fin_bmi_med_women.csv")




#-----------------------------
# Scenario for SBP
#-----------------------------
# Scenario Goal: Reduce the age-standardized average SBP (for individuals 40 years and older, including those on medical treatment) by 5 mmHg from 2024 to 2032.
# 	1) assuming that the annual decrease in SBP is constant from 2024 to 2032 .
# 	2) assuming that people with higher SBP will experience a larger decrease in SBP.
#	3) assuming that the decrease in SBP depnds on SBP only (SBP depends on age, sex, .......)
#	4) assuming the difference between the base-case and intervention scenarios remains after the intervention period.
#	5) assuming that apply the decrease in SBP only to individuals aged 40 years and older.
	
	
	# SONOTE added lower_clip to proportional_reduction() to limit the lowest value after intervention
	proportional_reduction_bounded <- function(
	exposure,     # exposure: eg. SBP_curr_xps,
	change,       # average change （minus change = - X）
	weights,      # weight = age-standardised
	penalty = 1,  # penalty: determines how sharply the penalty increases with x. penalty
	lower_clip = -Inf  # the lowest value after the intervention (not delta, but value)
	) {
	# 
	wm <- weighted.mean(exposure, weights)
	# 
	c <- (wm - (wm + change)) / weighted.mean(exposure^penalty, weights)
	message("c: ", c)
	# 
	new_exposure <- exposure - c * exposure^penalty
	# 
	new_exposure <- pmax(new_exposure, lower_clip)
	# 
	return(invisible(new_exposure))
	}
		


    # goal of SBP decrease
    # CKNOTE you need to define these in the function
    total_delta_SBPsc <- -5
    age_target_SBPsc <- 40 #40 and over yo
	penalty <- 3
	lower_clip <- synthpop$pop[, quantile(SBP_curr_xps, p = 0.0001)]
	
	#-- preparation
    # CKNOTE should be setkeyv(), better than setorder()
	setkeyv(synthpop$pop, c("pid", "year"))
	synthpop$pop[, SBP_curr_xps_original_after_BMI := copy(SBP_curr_xps), ] #changed for combined scenario 
	synthpop$pop[, wt_SBPSc := copy(wt_esp), ] # age-standardised prevalece NEED CHANGE

	
	#-- setting intervention (when where who what how)
    ## CKNOTE the above assumes change in year_base. Sometimes we assume no change for year_base
	effect_size <- synthpop$pop[
		year %in% (year_base:year_last) & age >= age_target_SBPsc, 
		.(sbp_mean_base = weighted.mean(SBP_curr_xps_original, wt_SBPSc)), 
		by = year
		]
	setorder(effect_size, year)
	
	## Annually "equally decreasing"
	effect_size[, sbp_mean_target_tmp := c(	
		seq(
			from = .SD[year == year_base, sbp_mean_base, ], 
			to = .SD[year == year_base, sbp_mean_base, ] + total_delta_SBPsc, 
			length.out = (year_goal - year_base + 1)
			),
		rep(
			x = .SD[year == year_base, sbp_mean_base, ] + total_delta_SBPsc, 
			time = year_last - year_goal
			)
		)
	, ]
	
	#assuming the difference between the base-case and intervention scenarios remains after the intervention period.
	effect_size[between(year, year_goal, year_last), sbp_mean_target_tmp := sbp_mean_base +	.SD[year == year_goal, pmin(sbp_mean_target_tmp, sbp_mean_base) - sbp_mean_base, ], ]
	effect_size[, sbp_mean_target := pmin(sbp_mean_target_tmp, sbp_mean_base), ]
	effect_size[, delta := sbp_mean_target - sbp_mean_base, ]
	effect_size

	effect_size <- effect_size[, list(year, delta), ]
	effect_size
	
				   
	# zero padding for years before the initial year of the intevention
    synthpop$pop[, delta := NA_real_]
    synthpop$pop[effect_size, on = 'year', delta := i.delta]
	
	#-- Perform intervention (who, when)
    synthpop$pop[age >= age_target_SBPsc & year %in% (year_base:year_last),
                 SBP_curr_xps := proportional_reduction_bounded(
					exposure = SBP_curr_xps_original_after_BMI, #changed for combined scenario  
					change = first(delta), 
					weights = wt_SBPSc, 
					penalty = penalty, 
					lower_clip = lower_clip
					),
                 by = year
				 ]
				 # SONOTE 20250711  := with keyby is only possible when i is not supplied since you can't setkey on a subset of rows. Either change keyby to by or remove i
	
		#synthpop$pop[, summary(SBP_curr_xps), ]
	synthpop$pop[
		age >= age_target_SBPsc & year %in% (year_base:year_last),
		SBP_curr_xps := pmin(SBP_curr_xps, SBP_curr_xps_original_after_BMI),  # the policy does not have bad effect. #changed for combined scenario 
		]				 
		#synthpop$pop[, summary(SBP_curr_xps), ]



	# delete objects
	# delete unnecessary colums and objects
	 synthpop$pop[, ":="(
	 	#SBP_curr_xps_original = NULL,
	 	#wt_SBPSc = NULL,
	 	delta = NULL
	 	), ]
	 
	 rm(
	 	age_target_SBPsc
	 	, effect_size
	 	, tmp
	 	, total_delta_SBPsc
	 	, penalty
		)






print("FINISHED SBP scenario")
#fwrite(synthpop$pop, "./fin_sbp.csv")

#-----------------------------
# Scenario for high ldl-c 
#-----------------------------
# Scenario Goal: Achieve a 25% reduction from baseline in the age-adjusted proportion of adults aged 40 and older who have LDL cholesterol levels ≥160 mg/dL (including those receiving oral treatment). The denominator is defined as adults aged 40 and older.
#	1) assuming decreases in LDL-C can achieve the target reduction in the proportion of LDL-C ≥160 mg/dL among adults aged 40 and older.
# 	2) assuming that people with higher LDLc will experience a larger decrease in LDLc.

# 	1) assuming that the annual decrease in LDL-c is constant from 2024 to 2032 
# 	4) assuming that the decrease in LDLc depends on LDLc only (LDLc depends on age, sex, .......) 
#	5) assuming the difference between the base-case and intervention scenarios remains after the intervention period.
# 	6) assuming that apply the decrease in LDLc only to individuals aged 40 years and older, and LDLc >= 160 mg/dl (high risk approach)


 


	
	# SONOTE added lower_clip to proportional_reduction() to limit the lowest value after intervention
	proportional_reduction_bounded <- function(
	exposure,     # exposure: eg. SBP_curr_xps,
	change,       # average change （minus change = - X）
	weights,      # weight = age-standardised
	penalty = 1,  # penalty: determines how sharply the penalty increases with x. penalty
	lower_clip = -Inf  # the lowest value after the intervention (not delta, but value)
	) {
	# 
	wm <- weighted.mean(exposure, weights)
	# 
	c <- (wm - (wm + change)) / weighted.mean(exposure^penalty, weights)
	message("c: ", c)
	# 
	new_exposure <- exposure - c * exposure^penalty
	# 
	new_exposure <- pmax(new_exposure, lower_clip)
	# 
	return(invisible(new_exposure))
	}



	
	# high-risk approach interventions
	proportional_reduction_threshold_highrisk <-
	function(exposure, threshold_impact, threshold_target, target_prop, weights, penalty = 1, lower_clip) {
		# it only applies changes above the threshold_impacts and optimises the
		# target proportion above the threshold_target
		exposure_bounded <- exposure[exposure > threshold_impact]
		weights_bounded <- weights[exposure > threshold_impact]
		
		# Function to compute proportion above threshold given a specific change
		calc_prop_above <- function(change) {
		adj_exposure <- exposure
		adj_exposure[exposure > threshold_impact] <-
			proportional_reduction_bounded(exposure_bounded, change, weights_bounded, penalty)
		sum(weights[adj_exposure > threshold_target]) / sum(weights)
		}
	
		# Find the change required to achieve the target proportion above threshold
		optimal_change <- uniroot(
		f = function(change) calc_prop_above(change) - target_prop,
		interval = c(-max(exposure), max(exposure)),
		extendInt = "yes", tol = .Machine$double.eps, maxiter = 1000, trace = 0
		)$root
	
		message("optimal_change: ", optimal_change)
		# Apply the optimal change
		exposure[exposure > threshold_impact] <-
		proportional_reduction_bounded(exposure_bounded, optimal_change, weights_bounded, penalty)
	
		exposure <- pmax(exposure, lower_clip)
		return(exposure)    
	}
	

	# goal of high ldl-c proportion decrease
	# Find current proportion of people with LDLc_curr_xps_original >= 160 taking into account weights
	# setting #1 for baseline value
	age_target_LDLCsc <- 40
	ldlc_threshold_LDLCsc <- 160
	threshold_impact_LDLCsc <- 160

	# setting #2 for baseline and target values
	target_reduction_rate <- (1 - 0.25)
	
	# setting #3 for the function
	penalty <- 2
	lower_clip <- synthpop$pop[, quantile(LDLc_curr_xps, p = 0.0001)]
	
	#-- preparation
    # CKNOTE should be setkeyv(), better than setorder()
	setkeyv(synthpop$pop, c("pid", "year"))
	synthpop$pop[, LDLc_curr_xps_original_after_BMI := copy(LDLc_curr_xps), ] #changed for combined scenario 
	synthpop$pop[, wt_LDLcSc := copy(wt_esp), ] # age-standardised prevalece  NEED CHANGE


	# baseline value
	sc0_prp <- synthpop$pop[
			year == year_base & age >= age_target_LDLCsc,
			sum(wt_LDLcSc[LDLc_curr_xps_original >= ldlc_threshold_LDLCsc]) / sum(wt_LDLcSc)
		]

	#-- setting intervention (when where who what how)
    ## CKNOTE the above assumes change in year_base. Sometimes we assume no change for year_base
	# target values; Let's assume we aim a 25% reduction (relative) of people with LDLc_curr_xps_original >= 160. The scenario target should then be
	sc1_trgt <- sc0_prp * target_reduction_rate
		print(sc0_prp)
		print(sc1_trgt)

    effect_size_cfscenario <- data.table(
						year = year_base:year_last,
						delta_cfscenario = c(seq(sc0_prp, sc1_trgt, length.out = (year_goal - year_base + 1)), rep(sc1_trgt, year_last - year_goal))
                   )
	
	# effect size in the base-case scenario
	effect_size_baseSc <- synthpop$pop[
		age >= age_target_LDLCsc & between(year, year_base, year_last), 
		.(delta_base = sum(wt_LDLcSc[LDLc_curr_xps_original >= ldlc_threshold_LDLCsc]) / sum(wt_LDLcSc)), 
		by = year
		] %>% setorder(year)

	# select lower proportion of high LDL-c 
	effect_size <- effect_size_cfscenario[effect_size_baseSc, on = "year"]
	effect_size[, delta := pmin(delta_base, delta_cfscenario), ]
	effect_size[, ":="(
		delta_cfscenario = NULL,	
		delta_base = NULL
		), ]
		

	# baseline padding for years before the initial year of the intevention
	synthpop$pop[, delta := NA_real_, ]

		print(synthpop$pop[, unique(delta), by = year] %>% setorder(year))
    synthpop$pop[effect_size, on = 'year', delta := i.delta]
		print(synthpop$pop[, unique(delta), by = year] %>% setorder(year))

	
	
	#-- Perform intervention (who)	
	synthpop$pop[age >= age_target_LDLCsc & year %in% (year_base:year_last),
			LDLc_curr_xps := proportional_reduction_threshold_highrisk(
				exposure = LDLc_curr_xps_original_after_BMI,#changed for combined scenario  
				threshold_impact = threshold_impact_LDLCsc,
				threshold_target = ldlc_threshold_LDLCsc, 
				target_prop = unique(delta), 
				weights = wt_LDLcSc, 
				penalty = penalty,
				lower_clip = lower_clip#,
				#verbose = TRUE
				),
			by = year
			]

		#synthpop$pop[, summary(LDLc_curr_xps), ]
	synthpop$pop[
		age >= age_target_LDLCsc & year %in% (year_base:year_last),
		LDLc_curr_xps := pmin(LDLc_curr_xps, LDLc_curr_xps_original_after_BMI), # the policy does not have bad effect.#changed for combined scenario 
		]				 
		#synthpop$pop[, summary(LDLc_curr_xps), ]



	# delete unnecessary colums and objects
	 synthpop$pop[, ":="(
	 	#LDLc_curr_xps_original = NULL, 
	 	#wt_LDLcSc = NULL, 
	 	delta = NULL
	 	)
	 	, ]
	 
	 rm(
	 	age_target_LDLCsc
	 	, effect_size
	 	, effect_size_baseSc
	 	, effect_size_cfscenario
	 	, ldlc_threshold_LDLCsc
	 	, penalty
	 	, sc0_prp
	 	, sc1_trgt
	 	, target_reduction_rate                                      
	 	, tmp
	 	, tmp1
	 	, tmp2
	 )


    
                     
print("FINISHED LDLc scenario")
#fwrite(synthpop$pop, "./fin_LDLc.csv")










#-----------------------------
# Scenario for suggestive diabetes
#-----------------------------
# Scenario Goal: Japanese government obtained future projections based on the trend-analysis tool by gender and age group (ages 20–39, 40–49, 50–59, 60–69, and 70 and over). Assuming the current trend continues, the number of people with diabetes is expected to reach approximately 14.48 million by fiscal year 2032. Therefore, to further curb this by 6.7% (to be 93.3%) through the promotion of countermeasures, the target has been set at 13.5 million people.

#	1) assuming that our modeling, we define “Individuals Strongly Suspected of Having Diabetes” based only on HbA1c ≥ 6.5%. 
#		In addition, we ignore the treatment for diabetes because it must be a strange policy to reduce treatment for people with high HbA1c.
#		However, in Health Japan 21, definition of “Individuals Strongly Suspected of Having Diabetes” is as follows.
#		Based on the National Health and Nutrition Survey: A person is classified as "strongly suspected of having diabetes" if:
#			Their measured Hemoglobin A1c (HbA1c, NGSP) is 6.5% or higher,
#				OR
#			They answered “Yes” to the question:“Are you currently receiving treatment for diabetes (including regular checkups or lifestyle guidance at a medical institution)?”
# 	2) assuming that the annual decrease in HbA1c is constant from 2024 to 2032. 
#	3) assuming decreases in HbA1c can achieve the target reduction in the proportion of HbA1c ≥ 6.5%. 
# 	4) assuming that people with higher HbA1c will experience a larger decrease in HbA1c.
# 	5) assuming that the decrease in HbA1c depends on HbA1c only (HbA1c depends on age, sex, .......) 
#	6) assuming the difference between the base-case and intervention scenarios remains after the intervention period.
# 	7) assuming that apply the decrease in HbA1c to individuals aged 30 years old and older. But, HJ21 targets people aged 20 yo and older. So, we assumed suggestive DM of 0% in 20-29 yo. This assumption would be valid based on NHNS results between 2019 showing low prevalence of sug DM in 20-29 yo.

 
	
	
	# SONOTE added lower_clip to proportional_reduction() to limit the lowest value after intervention
	proportional_reduction_bounded <- function(
		exposure,     # exposure: eg. SBP_curr_xps,
		change,       # average change （minus change = - X）
		weights,      # weight = age-standardised
		penalty = 1,  # penalty: determines how sharply the penalty increases with x. penalty
		lower_clip = -Inf  # the lowest value after the intervention (not delta, but value)
	) {
	# 
	wm <- weighted.mean(exposure, weights)
	# 
	c <- (wm - (wm + change)) / weighted.mean(exposure^penalty, weights)
	message("c: ", c)
	# 
	new_exposure <- exposure - c * exposure^penalty
	# 
	new_exposure <- pmax(new_exposure, lower_clip)
	# 
	return(invisible(new_exposure))
	}
	
	
	# population-level approach interventions
	# SONOTE: SO added the lower_clip, the lowest value after the intervention (not delta, but value)
	proportional_reduction_threshold_struct <-
	function(exposure, threshold, target_prop, weights, lower_clip, penalty = 1) {
	
	
	# Function to compute proportion above threshold given a specific change
	calc_prop_above <- function(change){
		adj_exposure <-
		proportional_reduction_bounded(exposure, change, weights, penalty)
		sum(weights[adj_exposure > threshold]) / sum(weights)
	}
	
	# Find the change required to achieve the target proportion above threshold
	optimal_change <- uniroot(
		f = function(change) calc_prop_above(change) - target_prop,
		interval = c(-max(exposure), max(exposure)),
		extendInt = "yes", tol = .Machine$double.eps, maxiter = 1000, trace = 0
	)$root
	
	message(optimal_change)
	# Apply the optimal change
	exposure <- proportional_reduction_bounded(exposure, optimal_change, weights, penalty)
	exposure <- pmax(exposure, lower_clip)
	return(exposure)
	
	}
	
	
	
	
	


	# goal of high sug.DM proportion decrease
	# Find current proportion of people with high HbA1c  >= 6.5% taking into account weights
	# setting #1 for baseline value
	age_target_HbA1csc <- 30
	HbA1c_threshold_HbA1csc <- 6.5

	# setting #2 for baseline and target values
	target_reduction_rate <- (1 - 0.067)
	
	# setting #3 for the function
	penalty <- 2
	lower_clip <- synthpop$pop[, quantile(HbA1c_curr_xps, p = 0.0001)]

	#-- preparation
    # CKNOTE should be setkeyv(), better than setorder()
	setkeyv(synthpop$pop, c("pid", "year"))
	synthpop$pop[, HbA1c_curr_xps_original_after_BMI := copy(HbA1c_curr_xps), ]#changed for combined scenario 
	synthpop$pop[, wt_HbA1cSc := wt, ] # NOT age-standardised proportion reduction for each age group, leading to absolute number



	# baseline values: we defined these data in 2032 by age groups following the concept of this target in Health Japan 21
	synthpop$pop[, agegrp_HbA1cSc := 
		fifelse(between(age, 20, 29), "20-29",
		fifelse(between(age, 30, 39), "30-39",
		fifelse(between(age, 40, 49), "40-49",
		fifelse(between(age, 50, 59), "50-59",
		fifelse(between(age, 60, 69), "60-69",
		"70+"))))), ]
		# synthpop$pop[, table(age, agegrp_HbA1cSc) ]

	tmp_base <- synthpop$pop[
			year == year_base & age >= age_target_HbA1csc,
			.(sc0_base = .SD[HbA1c_curr_xps_original >= HbA1c_threshold_HbA1csc, .N, ] / .N),
		by = .(agegrp_HbA1cSc, sex)
		] %>% setorder(., sex, agegrp_HbA1cSc)


	#-- setting intervention (when where who what how)
    ## We assume no change for year_base
	## target values; Let's assume a 6.7% reduction (relative) of people with HbA1c_curr_xps_original >= 6.5% in 2032 in Health Japan 21 scenario cmpared to that in 2032 in base-case scenario (trend-based projection restuls by a previous calculation). Instead of using the previous projections, we used the projected results in the base-case because of the aforementioned assumption (1).
	tmp_goal <- synthpop$pop[
			year == year_goal & age >= age_target_HbA1csc,
			.(sc0_trgt = .SD[HbA1c_curr_xps_original >= HbA1c_threshold_HbA1csc, .N, ] / .N),
		by = .(agegrp_HbA1cSc, sex)
		] %>% setorder(., sex, agegrp_HbA1cSc)
		
	tmp_goal[, sc1_trgt := sc0_trgt * target_reduction_rate, ]
	tmp_goal[, sc0_trgt := NULL, ]

	tmp_combine <- merge(tmp_base, tmp_goal, by = c("agegrp_HbA1cSc", "sex"))

	effect_size_cf <- tmp_combine[, .(
		year = year_base:year_last,
		delta_cfscenario = c(seq(sc0_base, sc1_trgt, length.out = (year_goal - year_base + 1)), rep(sc1_trgt, year_last - year_goal))
		), by = .(agegrp_HbA1cSc, sex)
		]
		
	# effect_size in the base-case scenario
	effect_size_bs <- synthpop$pop[
			between(year, year_base, year_last) & age >= age_target_HbA1csc,
			.(delta_base = .SD[HbA1c_curr_xps_original >= HbA1c_threshold_HbA1csc, .N, ] / .N),
		by = .(agegrp_HbA1cSc, sex, year)
		] %>% setorder(., sex, agegrp_HbA1cSc)
		
		
		
	# select the lower proportion of individuals with high HbA1c between base-case and Health Japan 21 scenarios
	effect_size <- merge(effect_size_bs, effect_size_cf, by = c("agegrp_HbA1cSc", "sex", "year")) 
	effect_size[, delta := pmin(delta_base, delta_cfscenario), ]
	effect_size[, ":="(
		delta_cfscenario = NULL,	
		delta_base = NULL
		), ]


	# baseline padding for years before the initial year of the intevention
	synthpop$pop[, delta := NA_real_, ]
    synthpop$pop[effect_size, on = c("year", "agegrp_HbA1cSc", "sex") , delta := i.delta]
	
	#-- Perform intervention (who)	
	synthpop$pop[age >= age_target_HbA1csc & year %in% (year_base:year_last),
			HbA1c_curr_xps := proportional_reduction_threshold_struct(
				exposure = HbA1c_curr_xps_original_after_BMI,#changed for combined scenario  
				threshold = HbA1c_threshold_HbA1csc, 
				target_prop = unique(delta), 
				weights = wt_HbA1cSc, 
				penalty = penalty,
				lower_clip = lower_clip
				),
			by = .(year, agegrp_HbA1cSc, sex)
			]
	
	synthpop$pop[
		age >= age_target_HbA1csc & year %in% (year_base:year_last),
		HbA1c_curr_xps := pmin(HbA1c_curr_xps, HbA1c_curr_xps_original_after_BMI), # the policy does not have bad effect. #changed for combined scenario 
		]				 






	# delete unnecessary colums and objects
	 synthpop$pop[, ":="(
	 	#HbA1c_curr_xps_original = NULL, 
	 	#wt_HbA1cSc = NULL, 
	 	#agegrp_HbA1cSc = NULL, 
	 	delta = NULL
	 	)
	 	, ]
	 
	 rm(
	 	age_target_HbA1csc
	 	, effect_size
	 	, effect_size_bs
	 	, effect_size_cf
	 	, HbA1c_threshold_HbA1csc
	 	, lower_clip
	 	, penalty
	 	, sc0_prp
	 	, sc1_trgt
	 	, target_reduction_rate                                      
	 	, tmp
	 	, tmp_base
	 	, tmp_goal
	 	, tmp_combine
	 	, tmp1
	 	, tmp2
	 )


print("FINISHED HbA1c scenario")
#fwrite(synthpop$pop, "./fin_HbA1c.csv")


	synthpop$pop[, ":="(
		Smoking_curr_xps_original = NULL 
		, Smoking_number_curr_xps_original = NULL 
		, wt_smokingsc = NULL 
		, delta = NULL
		, Smoking_stop = NULL 
		, Smoking_score = NULL 
		, BMI_curr_xps_med = NULL 
		, BMI_curr_xps_original = NULL 
		, BMI_curr_xps_original_after_smoking = NULL 
	 	, wt_BMISc = NULL 
		, BMI_curr_xps_mediation = NULL
	 	, bmi_percentage_change = NULL 
	 	, bmi_change = NULL
	 	, SBP_curr_xps_original = NULL
	 	, wt_SBPSc = NULL
	 	, LDLc_curr_xps_original = NULL 
	 	, wt_LDLcSc = NULL 
	 	, HbA1c_curr_xps_original = NULL 
	 	, wt_HbA1cSc = NULL 
	 	, agegrp_HbA1cSc = NULL 
	 	), ]
	 




})


IMPACTncd$
  run(1:mcnum, multicore = TRUE, "All")




#-----------------------
IMPACTncd$export_summaries(
  multicore = TRUE,
  type = c(
    "le", "hle", "dis_char" , 
     "prvl", "incd", "dis_mrtl", "mrtl",
     "all_cause_mrtl_by_dis", "cms", 
     "qalys", "costs"
  )
)

source("./auxil/process_out_JPN21.R")


