
################################################################################ base dataをつくる
library(fst)
library(dqrng) 
library(stringr)
library(qs)
library(data.table)
library(gamlss)
library(IMPACTncdJapan)
library(CKutils)




 
setwd("/home/rstudio/IMPACT_NCD_data/IMPACTncd_Japan-main")
		  

dt <- read_fst("./inputs/pop_estimates/observed_population_japan.fst", as.data.table = TRUE)
dt[, prbl := pops/sum(pops)][,`:=`(reg = NULL, pops = NULL)]
dt[, `:=`(pid  = .I)]
new_n <- nrow(dt)
dt
		  

cm_mean <- as.matrix(
  read_fst(
    "./inputs/exposure_distributions/exposure_corr_mean.fst", # Change-for-IMPACT-NCD-Japan
    as.data.table = TRUE
  ),
  rownames = "rn"
)

rank_mtx <- generate_corr_unifs(new_n, cm_mean)

rank_mtx <- rank_mtx * 0.999
rank_mtx[, "Fruit_vege_r"] <- rank_mtx[, "Fruit_vege_r"] * 0.95 / 0.999
rank_mtx[, "Smoking_r"] <- rank_mtx[, "Smoking_r"] * 0.95 / 0.999
rank_mtx[, "Smoking_number_r"] <- rank_mtx[, "Smoking_number_r"] * 0.95 / 0.999
rank_mtx[, "Med_HT_r"] <- rank_mtx[, "Med_HT_r"] * 0.95 / 0.999
rank_mtx[, "Med_HL_r"] <- rank_mtx[, "Med_HL_r"] * 0.95 / 0.999
rank_mtx[, "Med_DM_r"] <- rank_mtx[, "Med_DM_r"] * 0.95 / 0.999
rank_mtx[, "PA_days_r"] <- rank_mtx[, "PA_days_r"] * 0.95 / 0.999
rank_mtx[, "BMI_r"] <- rank_mtx[, "BMI_r"] * 0.95 / 0.999
rank_mtx[, "HbA1c_r"] <- rank_mtx[, "HbA1c_r"] * 0.95 / 0.999
rank_mtx[, "LDLc_r"] <- rank_mtx[, "LDLc_r"] * 0.95 / 0.999
rank_mtx[, "SBP_r"] <- rank_mtx[, "SBP_r"] * 0.95 / 0.999
rank_mtx <- data.table(rank_mtx)


			# ????? 20230206
            # NOTE rankstat_* is unaffected by the RW. Stay constant through the lifecourse
			# Change-for-IMPACT-NCD-Japan
			#dt <- cbind(dt, rank_mtx)
			#setnames(dt, colnames(rank_mtx), paste0("rank_", str_sub(colnames(rank_mtx), start = 1, end = -3)))
            dt[, c(
              "rank_Fruit_vege"
              , "rank_Smoking"
              , "rank_Smoking_number"
			  , "rank_Med_HT"
			  , "rank_Med_HL"
			  , "rank_Med_DM"        
			  , "rank_PA_days"
			  , "rank_BMI"
			  , "rank_HbA1c"         
			  , "rank_LDLc"
			  , "rank_SBP" 			  
            ) := rank_mtx]

            rm(rank_mtx)


			# ????? 20230206
            # add non-correlated RNs
			# Change-for-IMPACT-NCD-Japan
            rank_cols <-
              c(
                "rankstat_Med_HT_sug"
                , "rankstat_Med_HL_sug"
                , "rankstat_Med_DM_sug"
              )


            for (nam in rank_cols)
              set(dt, NULL, nam, dqrunif(new_n)) # NOTE do not replace with generate_rns function.



            dt[, `:=` (.id = NULL)]
			
			# Change-for-IMPACT-NCD-Japan
            if (max(dt$age) >= 90L) {
              dt[, age100 := age]
              dt[age >= 90L, age := 90L]
            } else {"No action"}


            setkeyv(dt, c("pid", "year"))
            setindexv(dt, c("year", "age", "sex")) #STRATA

            dt[, pid_mrk := mk_new_simulant_markers(pid)]

dt <- dt[age >= 20, , ]



















			# Change-for-IMPACT-NCD-Japan
			# Set limit age ranges
			Temp <- read_fst("/home/rstudio/IMPACT_NCD_data/NHNS_data/Output_data_organized/GAMLSS_created/HSE_ts.fst", as.data.table = TRUE)[between(Age, 20L, max(dt$age))]
			limit_age <- Temp[, .(min = min(Age), max = max(Age))]
			rm(Temp)
			limit_age
			
			
			
			
            # Generate Fruit_vege 
			# Change-for-IMPACT-NCD-Japan
			# Model_gamlss <- qread(paste0("/home/rstudio/IMPACT_NCD_data/NHNS_data/Output_data_organized/GAMLSS_created/GAMLSS_model_", "Fruit_vege", ".qs"))
			# Model_gamlss$parameters
			# Model_gamlss$family[1]
			# rm(Model_gamlss)
	
	
            #if (design_$sim_prm$logs) message("Generate Fruit_vege")

            tbl <-
              read_fst("./inputs/exposure_distributions/Table_Fruit_vege.fst",
                       as.data.table = TRUE)[between(Age, limit_age$min, limit_age$max)]
					   # Data had years observed and projected.
					   # We limited age raning from 20 to 90 when making the correlation matrix. 
            setnames(tbl, "Age", "age")
            setnames(tbl, "Sex", "sex")
            setnames(tbl, "Year", "year")
			tbl[, sex := ifelse(sex == 0, "men", "women"), ]
			
			
			col_nam <-
              setdiff(names(tbl), intersect(names(dt), names(tbl)))
            #if (Sys.info()["sysname"] == "Linux") {
            #  lookup_dt(dt, tbl, check_lookup_tbl_validity = FALSE) #TODO: Lookup_dt
            #} else {
              dt <- absorb_dt(dt, tbl)
            #}
			
			# ????? 20230206 I cannot find my_ function
			# ????? Age range, CJ(age = 20:100) in GAMLSS, Population 0-89 years old, Correlation 20 - 89,
			# For now, we use q___ insted of my_
			# Change-for-IMPACT-NCD-Japan
            dt[, Fruit_vege := qZINBI(rank_Fruit_vege, mu, sigma, nu), ] #, n_cpu = design_$sim_prm$n_cpu)]

            dt[, rank_Fruit_vege := NULL]
            dt[, (col_nam) := NULL]






            # Generate Smoking 
			# Change-for-IMPACT-NCD-Japan
			# Model_gamlss <- qread(paste0("/home/rstudio/IMPACT_NCD_data/NHNS_data/Output_data_organized/GAMLSS_created/GAMLSS_model_", "Smoking", ".qs"))
			# Model_gamlss$parameters
			# Model_gamlss$family[1]
			# rm(Model_gamlss)
	
	
            #if (design_$sim_prm$logs) message("Generate Smoking")

            tbl <-
              read_fst("./inputs/exposure_distributions/Table_Smoking.fst",
                       as.data.table = TRUE)[between(Age, limit_age$min, limit_age$max)]
					   # Data had years observed and projected.
					   # We limited age raning from 20 to 90 when making the correlation matrix. 
            setnames(tbl, "Age", "age")
            setnames(tbl, "Sex", "sex")
            setnames(tbl, "Year", "year")
			tbl[, sex := ifelse(sex == 0, "men", "women"), ]
			
			
			col_nam <-
              setdiff(names(tbl), intersect(names(dt), names(tbl)))
            #if (Sys.info()["sysname"] == "Linux") {
            #  lookup_dt(dt, tbl, check_lookup_tbl_validity = FALSE) #TODO: Lookup_dt
            #} else {
              dt <- absorb_dt(dt, tbl)
            #}
			
			# ????? 20230206 I cannot find my_ function
			# ????? Age range, CJ(age = 20:100) in GAMLSS, Population 0-89 years old, Correlation 20 - 89,
			# For now, we use q___ insted of my_
			# Change-for-IMPACT-NCD-Japan
            dt[, Smoking := qMN3(rank_Smoking, mu, sigma), ] #, n_cpu = design_$sim_prm$n_cpu)]
			dt[, Smoking := factor(Smoking), ]
			dt[, table(Smoking), ]
			
            dt[, rank_Smoking := NULL]
            dt[, (col_nam) := NULL]





            # Generate Smoking_number 
			# Change-for-IMPACT-NCD-Japan
			# Model_gamlss <- qread(paste0("/home/rstudio/IMPACT_NCD_data/NHNS_data/Output_data_organized/GAMLSS_created/GAMLSS_model_", "Smoking_number", ".qs"))
			# Model_gamlss
			# rm(Model_gamlss)

	
            #if (design_$sim_prm$logs) message("Generate Smoking_number")

            tbl <-
              read_fst("./inputs/exposure_distributions/Table_Smoking_number.fst",
                       as.data.table = TRUE)[between(Age, limit_age$min, limit_age$max)]
					   # Data had years observed and projected.
					   # We limited age raning from 20 to 90 when making the correlation matrix. 
            setnames(tbl, "Age", "age")
            setnames(tbl, "Sex", "sex")
            setnames(tbl, "Year", "year")
			tbl[, sex := ifelse(sex == 0, "men", "women"), ]
			
			
			col_nam <-
              setdiff(names(tbl), intersect(names(dt), names(tbl)))
            #if (Sys.info()["sysname"] == "Linux") {
            #  lookup_dt(dt, tbl, check_lookup_tbl_validity = FALSE) #TODO: Lookup_dt
            #} else {
              dt <- absorb_dt(dt, tbl)
            #}


			dt[Smoking %in% c(3),
				Smoking_number := factor(
				levels = 0:8, labels = 0:8, ordered = TRUE,	   
				(rank_Smoking_number > pa0) +
				(rank_Smoking_number > pa1) +
					(rank_Smoking_number > pa2) +
					(rank_Smoking_number > pa3) +
					(rank_Smoking_number > pa4) +
					(rank_Smoking_number > pa5) +
					(rank_Smoking_number > pa6) +
				(rank_Smoking_number > pa7)  
				)
				]

            dt[, rank_Smoking_number := NULL]
            dt[, (col_nam) := NULL]
			
			
			
			


            # Generate Med_HT 
			# Change-for-IMPACT-NCD-Japan
			# Model_gamlss <- qread(paste0("/home/rstudio/IMPACT_NCD_data/NHNS_data/Output_data_organized/GAMLSS_created/GAMLSS_model_", "Med_HT", ".qs"))
			# Model_gamlss$parameters
			# Model_gamlss$family[1]
			# rm(Model_gamlss)
	
	
            #if (design_$sim_prm$logs) message("Generate Med_HT")

            tbl <-
              read_fst("./inputs/exposure_distributions/Table_Med_HT.fst",
                       as.data.table = TRUE)[between(Age, limit_age$min, limit_age$max)]
					   # Data had years observed and projected.
					   # We limited age raning from 20 to 90 when making the correlation matrix. 
            setnames(tbl, "Age", "age")
            setnames(tbl, "Sex", "sex")
            setnames(tbl, "Year", "year")
			tbl[, sex := ifelse(sex == 0, "men", "women"), ]
			
			
			col_nam <-
              setdiff(names(tbl), intersect(names(dt), names(tbl)))
            #if (Sys.info()["sysname"] == "Linux") {
            #  lookup_dt(dt, tbl, check_lookup_tbl_validity = FALSE) #TODO: Lookup_dt
            #} else {
              dt <- absorb_dt(dt, tbl)
            #}
			
			# ????? 20230206 I cannot find my_ function
			# ????? Age range, CJ(age = 20:100) in GAMLSS, Population 0-89 years old, Correlation 20 - 89,
			# For now, we use q___ insted of my_
			# Change-for-IMPACT-NCD-Japan
            dt[, Med_HT := qBI(rank_Med_HT, mu), ] #, n_cpu = design_$sim_prm$n_cpu)]

            dt[, rank_Med_HT := NULL]
            dt[, (col_nam) := NULL]
			
			
			




            # Generate Med_HL 
			# Change-for-IMPACT-NCD-Japan
			# Model_gamlss <- qread(paste0("/home/rstudio/IMPACT_NCD_data/NHNS_data/Output_data_organized/GAMLSS_created/GAMLSS_model_", "Med_HL", ".qs"))
			# Model_gamlss$parameters
			# Model_gamlss$family[1]
			# rm(Model_gamlss)
	
	
            #if (design_$sim_prm$logs) message("Generate Med_HL")

            tbl <-
              read_fst("./inputs/exposure_distributions/Table_Med_HL.fst",
                       as.data.table = TRUE)[between(Age, limit_age$min, limit_age$max)]
					   # Data had years observed and projected.
					   # We limited age raning from 20 to 90 when making the correlation matrix. 
            setnames(tbl, "Age", "age")
            setnames(tbl, "Sex", "sex")
            setnames(tbl, "Year", "year")
			tbl[, sex := ifelse(sex == 0, "men", "women"), ]
			
			
			col_nam <-
              setdiff(names(tbl), intersect(names(dt), names(tbl)))
            #if (Sys.info()["sysname"] == "Linux") {
            #  lookup_dt(dt, tbl, check_lookup_tbl_validity = FALSE) #TODO: Lookup_dt
            #} else {
              dt <- absorb_dt(dt, tbl)
            #}
			
			# ????? 20230206 I cannot find my_ function
			# ????? Age range, CJ(age = 20:100) in GAMLSS, Population 0-89 years old, Correlation 20 - 89,
			# For now, we use q___ insted of my_
			# Change-for-IMPACT-NCD-Japan
            dt[, Med_HL := qBI(rank_Med_HL, mu), ] #, n_cpu = design_$sim_prm$n_cpu)]

            dt[, rank_Med_HL := NULL]
            dt[, (col_nam) := NULL]






            # Generate Med_DM 
			# Change-for-IMPACT-NCD-Japan
			# Model_gamlss <- qread(paste0("/home/rstudio/IMPACT_NCD_data/NHNS_data/Output_data_organized/GAMLSS_created/GAMLSS_model_", "Med_DM", ".qs"))
			# Model_gamlss$parameters
			# Model_gamlss$family[1]
			# rm(Model_gamlss)
	
	
            #if (design_$sim_prm$logs) message("Generate Med_DM")

            tbl <-
              read_fst("./inputs/exposure_distributions/Table_Med_DM.fst",
                       as.data.table = TRUE)[between(Age, limit_age$min, limit_age$max)]
					   # Data had years observed and projected.
					   # We limited age raning from 20 to 90 when making the correlation matrix. 
            setnames(tbl, "Age", "age")
            setnames(tbl, "Sex", "sex")
            setnames(tbl, "Year", "year")
			tbl[, sex := ifelse(sex == 0, "men", "women"), ]
			
			
			col_nam <-
              setdiff(names(tbl), intersect(names(dt), names(tbl)))
            #if (Sys.info()["sysname"] == "Linux") {
            #  lookup_dt(dt, tbl, check_lookup_tbl_validity = FALSE) #TODO: Lookup_dt
            #} else {
              dt <- absorb_dt(dt, tbl)
            #}
			
			# ????? 20230206 I cannot find my_ function
			# ????? Age range, CJ(age = 20:100) in GAMLSS, Population 0-89 years old, Correlation 20 - 89,
			# For now, we use q___ insted of my_
			# Change-for-IMPACT-NCD-Japan
            dt[, Med_DM := qBI(rank_Med_DM, mu), ] #, n_cpu = design_$sim_prm$n_cpu)]

            dt[, rank_Med_DM := NULL]
            dt[, (col_nam) := NULL]
			
			
			
			
			
			
			
			
			


            # Generate PA_days 
			# Change-for-IMPACT-NCD-Japan
			# Model_gamlss <- qread(paste0("/home/rstudio/IMPACT_NCD_data/NHNS_data/Output_data_organized/GAMLSS_created/GAMLSS_model_", "PA_days", ".qs"))
			# Model_gamlss
			# rm(Model_gamlss)

	
            #if (design_$sim_prm$logs) message("Generate PA_days")

            tbl <-
              read_fst("./inputs/exposure_distributions/Table_PA_days.fst",
                       as.data.table = TRUE)[between(Age, limit_age$min, limit_age$max)]
					   # Data had years observed and projected.
					   # We limited age raning from 20 to 90 when making the correlation matrix. 
            setnames(tbl, "Age", "age")
            setnames(tbl, "Sex", "sex")
            setnames(tbl, "Year", "year")
			tbl[, sex := ifelse(sex == 0, "men", "women"), ]
			
			
			col_nam <-
              setdiff(names(tbl), intersect(names(dt), names(tbl)))
            #if (Sys.info()["sysname"] == "Linux") {
            #  lookup_dt(dt, tbl, check_lookup_tbl_validity = FALSE) #TODO: Lookup_dt
            #} else {
              dt <- absorb_dt(dt, tbl)
            #}


			dt[,
				PA_days := factor(
				levels = 0:7, labels = 0:7, ordered = TRUE,	   
				(rank_PA_days > pa0) +
				(rank_PA_days > pa1) +
					(rank_PA_days > pa2) +
					(rank_PA_days > pa3) +
					(rank_PA_days > pa4) +
					(rank_PA_days > pa5) +
					(rank_PA_days > pa6)
				)
				]

            dt[, rank_PA_days := NULL]
            dt[, (col_nam) := NULL]
			
			
			
			### Make PA days
			dt[,PA_3cat:=ifelse(PA_days %in% c(0, 1), 0,
						ifelse(PA_days %in% c(2, 3, 4), 1,
						ifelse(PA_days %in% c(5, 6, 7), 2, NA))),]
				dt[,table(PA_3cat, PA_days, useNA="always"),]
			
			
			dt[, PA_3cat := factor(PA_3cat + 1)]
				table(dt$PA_3cat, useNA = "always")
				
	
	

			
			
			
			
			
			
            # Generate BMI
			# Change-for-IMPACT-NCD-Japan
			# Model_gamlss <- qread(paste0("/home/rstudio/IMPACT_NCD_data/NHNS_data/Output_data_organized/GAMLSS_created/GAMLSS_model_", "BMI", ".qs"))
			# Model_gamlss$parameters
			# Model_gamlss$family[1]
			# rm(Model_gamlss)
	
	
            #if (design_$sim_prm$logs) message("Generate BMI")

            tbl <-
              read_fst("./inputs/exposure_distributions/Table_BMI.fst",
                       as.data.table = TRUE)[between(Age, limit_age$min, limit_age$max)]
					   # Data had years observed and projected.
					   # We limited age raning from 20 to 90 when making the correlation matrix. 
            setnames(tbl, "Age", "age")
            setnames(tbl, "Sex", "sex")
            setnames(tbl, "Year", "year")
			tbl[, sex := ifelse(sex == 0, "men", "women"), ]
			
			
			col_nam <-
              setdiff(names(tbl), intersect(names(dt), names(tbl)))
            #if (Sys.info()["sysname"] == "Linux") {
            #  lookup_dt(dt, tbl, check_lookup_tbl_validity = FALSE) #TODO: Lookup_dt
            #} else {
              dt <- absorb_dt(dt, tbl)
            #}
			
			# ????? 20230206 I cannot find my_ function
			# ????? Age range, CJ(age = 20:100) in GAMLSS, Population 0-89 years old, Correlation 20 - 89,
			# For now, we use q___ insted of my_
			# Change-for-IMPACT-NCD-Japan
            dt[, BMI := qBCTo(rank_BMI, mu, sigma, nu, tau), ] #, n_cpu = design_$sim_prm$n_cpu)]
			dt[BMI < 10, BMI := 10] #Truncate BMI predictions to avoid unrealistic values.
            dt[BMI > 70, BMI := 70] #Truncate BMI predictions to avoid unrealistic values.
					
            dt[, rank_BMI := NULL]
            dt[, (col_nam) := NULL]





            # Generate HbA1c 
			# Change-for-IMPACT-NCD-Japan
			# Model_gamlss <- qread(paste0("/home/rstudio/IMPACT_NCD_data/NHNS_data/Output_data_organized/GAMLSS_created/GAMLSS_model_", "HbA1c", ".qs"))
			# Model_gamlss$parameters
			# Model_gamlss$family[1]
			# rm(Model_gamlss)
	
	
            #if (design_$sim_prm$logs) message("Generate HbA1c")

            tbl <-
              read_fst("./inputs/exposure_distributions/Table_HbA1c.fst",
                       as.data.table = TRUE)[between(Age, limit_age$min, limit_age$max)]
					   # Data had years observed and projected.
					   # We limited age raning from 20 to 90 when making the correlation matrix. 
            setnames(tbl, "Age", "age")
            setnames(tbl, "Sex", "sex")
            setnames(tbl, "Year", "year")
			tbl[, sex := ifelse(sex == 0, "men", "women"), ]
			setnames(tbl, "BMI", "BMI_round")



			
			col_nam <-
              setdiff(names(tbl), intersect(names(dt), names(tbl)))
            #if (Sys.info()["sysname"] == "Linux") {
            #  lookup_dt(dt, tbl, check_lookup_tbl_validity = FALSE) #TODO: Lookup_dt
            #} else {
			dt[, BMI_round := round(BMI), ]
			dt <- absorb_dt(dt, tbl)
            #}
			
			# ????? 20230206 I cannot find my_ function
			# ????? Age range, CJ(age = 20:100) in GAMLSS, Population 0-89 years old, Correlation 20 - 89,
			# For now, we use q___ insted of my_
			# Change-for-IMPACT-NCD-Japan
            dt[, HbA1c := qBCT(rank_HbA1c, mu, sigma, nu, tau), ] #, n_cpu = design_$sim_prm$n_cpu)]
            
            dt[, rank_HbA1c := NULL]
            dt[, (col_nam) := NULL]
			
			
			
			
            # Generate LDLc 
			# Change-for-IMPACT-NCD-Japan
			# Model_gamlss <- qread(paste0("/home/rstudio/IMPACT_NCD_data/NHNS_data/Output_data_organized/GAMLSS_created/GAMLSS_model_", "LDLc", ".qs"))
			# Model_gamlss$parameters
			# Model_gamlss$family[1]
			# rm(Model_gamlss)
	
	
            #if (design_$sim_prm$logs) message("Generate LDLc")

            tbl <-
              read_fst("./inputs/exposure_distributions/Table_LDLc.fst",
                       as.data.table = TRUE)[between(Age, limit_age$min, limit_age$max)]
					   # Data had years observed and projected.
					   # We limited age raning from 20 to 90 when making the correlation matrix. 
            setnames(tbl, "Age", "age")
            setnames(tbl, "Sex", "sex")
            setnames(tbl, "Year", "year")
			tbl[, sex := ifelse(sex == 0, "men", "women"), ]
			setnames(tbl, "BMI", "BMI_round")


			
			col_nam <-
              setdiff(names(tbl), intersect(names(dt), names(tbl)))
            #if (Sys.info()["sysname"] == "Linux") {
            #  lookup_dt(dt, tbl, check_lookup_tbl_validity = FALSE) #TODO: Lookup_dt
            #} else {
			dt[, BMI_round := round(BMI), ]
			dt <- absorb_dt(dt, tbl)			

            #}
			
			# ????? 20230206 I cannot find my_ function
			# ????? Age range, CJ(age = 20:100) in GAMLSS, Population 0-89 years old, Correlation 20 - 89,
			# For now, we use q___ insted of my_
			# Change-for-IMPACT-NCD-Japan
            dt[, LDLc := qBCT(rank_LDLc, mu, sigma, nu, tau), ] #, n_cpu = design_$sim_prm$n_cpu)]
            
            dt[, rank_LDLc := NULL]
            dt[, (col_nam) := NULL]			
			
			
			
			
			
			
			
			
			# Generate SBP 
			# Change-for-IMPACT-NCD-Japan
			# Model_gamlss <- qread(paste0("/home/rstudio/IMPACT_NCD_data/NHNS_data/Output_data_organized/GAMLSS_created/GAMLSS_model_", "SBP", ".qs"))
			# Model_gamlss$parameters
			# Model_gamlss$family[1]
			# rm(Model_gamlss)
	
	
            #if (design_$sim_prm$logs) message("Generate SBP")

            tbl <-
              read_fst("./inputs/exposure_distributions/Table_SBP.fst",
                       as.data.table = TRUE)[between(Age, limit_age$min, limit_age$max)]
					   # Data had years observed and projected.
					   # We limited age raning from 20 to 90 when making the correlation matrix. 
            setnames(tbl, "Age", "age")
            setnames(tbl, "Sex", "sex")
            setnames(tbl, "Year", "year")
			tbl[, sex := ifelse(sex == 0, "men", "women"), ]
			setnames(tbl, "BMI", "BMI_round")
			
			
			
			
			col_nam <-
              setdiff(names(tbl), intersect(names(dt), names(tbl)))
            #if (Sys.info()["sysname"] == "Linux") {
            #  lookup_dt(dt, tbl, check_lookup_tbl_validity = FALSE) #TODO: Lookup_dt
            #} else {
            
			dt[, BMI_round := round(BMI), ]
			dt <- absorb_dt(dt, tbl)
            #}
			
			# ????? 20230206 I cannot find my_ function
			# ????? Age range, CJ(age = 20:100) in GAMLSS, Population 0-89 years old, Correlation 20 - 89,
			# For now, we use q___ insted of my_
			# Change-for-IMPACT-NCD-Japan
            dt[, SBP := qBCPE(rank_SBP, mu, sigma, nu, tau), ] #, n_cpu = design_$sim_prm$n_cpu)]
            
            dt[, rank_SBP := NULL]
            dt[, (col_nam) := NULL]
			
			
					

