
################################################################################ base dataをつくる
library(fst)
library(dqrng) 
library(stringr)
library(qs)
library(data.table)
library(gamlss)
library(IMPACTncdJapan)
library(CKutils)



setwd("/home/rstudio/IMPACT_NCD_data/IMPACTncd_Japan")


dt <- read_fst("./inputs/pop_estimates/observed_population_japan.fst", as.data.table = TRUE)
dt[, prbl := pops/sum(pops)][,`:=`(reg = NULL, pops = NULL)]
dt[, `:=`(pid  = .I)]
new_n <- nrow(dt)
dt
		  

cm_mean <- as.matrix(
  read_fst(
    "./inputs/exposure_distributions/exposure_corr_mean.fst", # Change-fo- IMPACT-NCD-Japan
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
			# Change-fo- IMPACT-NCD-Japan
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















########################################################################################################################






            # Generate PA_days 
			# Change-fo- IMPACT-NCD-Japan
			# Model_gamlss <- qread(paste0("/home/rstudio/IMPACT_NCD_data/NHNS_data/Output_data_organized/GAMLSS_created/GAMLSS_model_", "PA_days", ".qs"))
			# Model_gamlss
			# rm(Model_gamlss)

	
            if (design_$sim_prm$logs) message("Generate PA_days")

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
			
			
			
			### Change variables
			dt[,PA_3cat:=ifelse(PA_days %in% c(0, 1), 0,
						ifelse(PA_days %in% c(2, 3, 4), 1,
						ifelse(PA_days %in% c(5, 6, 7), 2, NA))),]
				dt[,table(PA_3cat, PA_days, useNA="always"),]
			
			
			dt[, PA_3cat := factor(PA_3cat + 1)]
				table(dt$PA_3cat, useNA = "always")
				
	
	

#########################################



            # Generate SBP 
			# Change-fo- IMPACT-NCD-Japan
			Model_gamlss <- qread(paste0("/home/rstudio/IMPACT_NCD_data/NHNS_data/Output_data_organized/GAMLSS_created/GAMLSS_model_", "SBP", ".qs"))
			Model_gamlss$parameters
			Model_gamlss$family[1]
			rm(Model_gamlss)
	
	
            if (design_$sim_prm$logs) message("Generate SBP")

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
			# Change-fo- IMPACT-NCD-Japan
            dt[, SBP := qBCPE(rank_SBP, mu, sigma, nu, tau), ] #, n_cpu = design_$sim_prm$n_cpu)]
            
            dt[, rank_SBP := NULL]
            dt[, (col_nam) := NULL]
			
			
			
			
#############################		  

library(stringr)
dt222 <- cbind(dt, rank_mtx)
setnames(dt222, colnames(rank_mtx), paste0("rank_", str_sub(colnames(rank_mtx), start = 1, end = -3)))



dt333 <- copy(dt)
dt333[, c(
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

identical(dt222, dt333)

dt









################################################################
################################################################
################################################################

library(fst)
A <- read_fst("C:/Users/ogata_work/Desktop/Docker/IMPACT_NCD_data/IMPACTncd_Japan-main/inputs/pop_estimates/Germany/mid_year_German_population.fst", as.data.table = TRUE)
summary(A)


A <- read_fst("C:/Users/ogata_work/Desktop/Docker/IMPACT_NCD_data/IMPACTncd_Japan-main/inputs/pop_projections/Germany/German_pop_projection.fst", as.data.table = TRUE)
summary(A)



A <- read_fst("C:/Users/ogata_work/Desktop/Docker/IMPACT_NCD_data/IMPACTncd_Japan-main/inputs/pop_projections/Germany/German_pop_combined.fst", as.data.table = TRUE)
summary(A)




		  
		  

###########################################################################
###########################################################################
###########################################################################

library(fst)
Temp <- read_fst("./IMPACT_NCD_data/IMPACTncd_Japan-main/inputs/exposure_distributions/Germany/exposure_corr.fst", as.data.table = TRUE)
Temp
fwrite(Temp, "./IMPACT_NCD_data/IMPACTncd_Japan-main/OGATA_MEMO/exposure_corr_Germany.csv")



Temp <- read_fst("./IMPACT_NCD_data/IMPACTncd_Japan-main/inputs/exposure_distributions/exposure_corr_mean.fst", as.data.table = TRUE)
Temp
fwrite(Temp, "./IMPACT_NCD_data/IMPACTncd_Japan-main/OGATA_MEMO/exposure_corr_mean_Japan.csv")







###########################################################################
###########################################################################
###########################################################################
library(fst)


###
Temp <- read_fst("./IMPACT_NCD_data/IMPACTncd_Japan-main/inputs/exposure_distributions/Germany/bmi_table.fst", as.data.table = TRUE)
Temp
fwrite(Temp, "./IMPACT_NCD_data/IMPACTncd_Japan-main/OGATA_MEMO/BMI_table_fst_Germany.csv")




###
Temp <- read_fst("./IMPACT_NCD_data/IMPACTncd_Japan-main/inputs/exposure_distributions/Table_BMI.fst", as.data.table = TRUE)
Temp
fwrite(Temp, "./IMPACT_NCD_data/IMPACTncd_Japan-main/OGATA_MEMO/BMI_table_fst_Japan.csv")






###########################################################################
###########################################################################
###########################################################################
library(fst)
library(dplyr)
library(data.table)

Data_german <- read_fst("./IMPACT_NCD_data/IMPACTncd_Japan-main/inputs/pop_estimates/Germany/mid_year_German_population.fst", as.data.table = TRUE)
Data_german[, .(sum(pops)), by = list(year)]
fwrite(Data_german, "./IMPACT_NCD_data/IMPACTncd_Japan-main/OGATA_MEMO/Population_Germany.csv")
#一人単位データ





Data_japan <- read_fst("./IMPACT_NCD_data/IMPACTncd_Japan-main/inputs/pop_estimates/October_year_Japan_population.fst") %>% data.table()

Data_japan

fwrite(Data_german, "./IMPACT_NCD_data/IMPACTncd_Japan-main/OGATA_MEMO/Population_Japan.csv")




