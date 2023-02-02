
library(fst)
library(data.table)
library(ggplot2)
library(CKutils)

mc_s <- 1000 # Number of MC samples

par_s <- 10 # Paramters

#### Health utility values ----
# Source: Laxy et al. 2021 Value in Health #

# TODO: Check whether needs to be reestimated to get marginals independent of smoking, education etc.!
# -> Reestimate old analysis to get actual heterogeneity and appropriate uncertainty!

util_combine <- data.table(NULL)

for(i in 1:mc_s){
 
  ## Ensure replicability #  
  set.seed(log(i) * 42/2 + 1337) # Seed is parameter-specific!
  
  ## Draw quantiles #
  quantiles <- runif(par_s, min = 0, max = 1) * 0.9999
  
  ## Create data shell #
  util_data <- CJ(sex =         c(1, 0), # 1 = women, 0 = men
                  t2dm_stat =   c(1, 0),
                  chd_stat =    c(3, 2, 1, 0),
                  stroke_stat = c(3, 2, 1, 0))
  
  ## Age and sex utility decrements #
  util_data[, `:=`(util_incpt = qnorm(quantiles[1], mean = 1.187, sd = 0.019),
                   util_age = qnorm(quantiles[2], mean = -0.003, sd = 0.0001),
                   util_sex = sex * qnorm(quantiles[3], mean = -0.029, sd = 0.004))]
  
  ## BMI decrement #
  util_data[, util_bmi := qnorm(quantiles[4], mean = -0.003, sd = 0.0001)]
  
  ## Disease-specific utility decrements #
  # Type 2 Diabetes
  util_data[, util_disease := fifelse(t2dm_stat == 1 & chd_stat == 0 & stroke_stat == 0,
                                      qnorm(quantiles[5], mean = -0.028, sd = 0.014), # Only Diabetes
                              fifelse(t2dm_stat == 1 & chd_stat >= 1 & stroke_stat == 0,
                                      qnorm(quantiles[5], mean = -0.028, sd = 0.014), ## TBD! Diabetes and CHD
                              fifelse(t2dm_stat == 1 & chd_stat == 0 & stroke_stat >= 1,
                                      qnorm(quantiles[6], mean = -0.122, sd = 0.018) +
                                        qnorm(quantiles[5], mean = -0.028, sd = 0.014), # Diabetes and Stroke
                              fifelse(t2dm_stat == 1 & chd_stat >= 1 & stroke_stat >= 1,
                                      qnorm(quantiles[6], mean = -0.122, sd = 0.018) +
                                        qnorm(quantiles[5], mean = -0.028, sd = 0.014), 0))))] # Diabetes, CHD and Stroke
  
  # Coronary Heart Disease and Stroke
  util_data[, util_disease := fifelse(t2dm_stat == 0 & chd_stat >= 1 & stroke_stat == 0,
                                      qnorm(quantiles[7], mean = -0.028, sd = 0.010), # Only CHD
                              fifelse(t2dm_stat == 0 & chd_stat >= 1 & stroke_stat >= 1,
                                      qnorm(quantiles[8], mean = -0.070, sd = 0.010) +
                                        qnorm(quantiles[7], mean = -0.028, sd = 0.010), ## TBD! CHD and Stroke
                              fifelse(t2dm_stat == 0 & chd_stat == 0 & stroke_stat >= 1,
                                      qnorm(quantiles[8], mean = -0.070, sd = 0.010), util_disease)))] # Only Stroke 
  
  util_data[, `:=`(sex = ifelse(sex == 1, "women", "men"),
                   mc = i)]
  
  util_combine <- rbind(util_data, util_combine)

}

write_fst(util_combine, "./inputs/other_parameters/health_utility.fst")

util_combine[, rn := .I]
tt <- util_combine[, .(from = min(rn), to = max(rn)), keyby = mc]
write_fst(tt, "./inputs/other_parameters/health_utility_indx.fst", 100L)

#### Healthcare costs ----
# Source: Kähm et al. 2020 Diabetic Medicine; Kähm et al. 2018 Diabetes Care #

# Approach: Baseline costs from Kähm 2020.
#           Uncomplicated diabetes cost from Kähm 2018.
#           CHD and Stroke costs for people with diabetes from Kähm 2018.
#           CHD and Stroke costs for people without diabetes using above - uncomplicated diabetes costs
#           from Kähm 2018.

# TODO: make variable for SD parameter!

par_s <- 80 # Paramters

cost_combine <- data.table(NULL)

for(i in 1:mc_s){
  
  ## Ensure replicability #  
  set.seed(log(i) * 42 + 1337) # Seed is parameter-specific!
  
  ## Draw quantiles #
  quantiles <- runif(par_s, min = 0, max = 1) * 0.9999
    
  cost_data <- CJ(age_cost =         c("<50", "50-59", "60-69", "70-79", "80+"),
                  sex =         c(1, 0), # 1 = women, 0 = men
                  t2dm_stat =   c(1, 0), # 2 = prevalent year, 1 = incident year, 0 = no disease
                  chd_stat =    c(3, 2, 1, 0),
                  stroke_stat = c(3, 2, 1, 0))
  
  ## Baseline costs (parameter uncertainty!) #
  cost_data[sex == 1, cost := fifelse(age_cost == "<50",   qnorm(quantiles[1], mean = 1019, sd = (1019 - 1002)/1.96),
                              fifelse(age_cost == "50-59", qnorm(quantiles[2], mean = 1483, sd = (1483 - 1465)/1.96),
                              fifelse(age_cost == "60-69", qnorm(quantiles[3], mean = 2140, sd = (2140 - 2120)/1.96),
                              fifelse(age_cost == "70-79", qnorm(quantiles[4], mean = 3156, sd = (3156 - 3127)/1.96),
                                                      qnorm(quantiles[5], mean = 3975, sd = (3975 - 3899)/1.96)))))]
  
  cost_data[sex == 0, cost := fifelse(age_cost == "<50",   qnorm(quantiles[6], mean = 1019, sd = (1019 - 1002)/1.96),
                              fifelse(age_cost == "50-59", qnorm(quantiles[7], mean = 1483, sd = (1483 - 1465)/1.96),
                              fifelse(age_cost == "60-69", qnorm(quantiles[8], mean = 2140, sd = (2140 - 2120)/1.96),
                              fifelse(age_cost == "70-79", qnorm(quantiles[9], mean = 3156, sd = (3156 - 3127)/1.96),
                                                      qnorm(quantiles[10], mean = 3975, sd = (3975 - 3899)/1.96)))))]
  
  ## Uncomplicated diabetes costs #
  
  cost_data[sex == 1 & t2dm_stat %in% c(1, 2),
            cost_t2dm := fifelse(age_cost == "<50",   qnorm(quantiles[11], mean = 3001, sd = 3001 * 0.02),
                         fifelse(age_cost == "50-59", qnorm(quantiles[12], mean = 2889, sd = 2889 * 0.02),
                         fifelse(age_cost == "60-69", qnorm(quantiles[13], mean = 2864, sd = 2864 * 0.02),
                         fifelse(age_cost == "70-79", qnorm(quantiles[14], mean = 3052, sd = 3052 * 0.02),
                                                 qnorm(quantiles[15], mean = 2388, sd = 2388 * 0.02)))))]
  
  cost_data[sex == 0 & t2dm_stat %in% c(1, 2),
            cost_t2dm := fifelse(age_cost == "<50",   qnorm(quantiles[16], mean = 2102, sd = 2102 * 0.02),
                         fifelse(age_cost == "50-59", qnorm(quantiles[17], mean = 2296, sd = 2296 * 0.02),
                         fifelse(age_cost == "60-69", qnorm(quantiles[18], mean = 2574, sd = 2574 * 0.02),
                         fifelse(age_cost == "70-79", qnorm(quantiles[19], mean = 2911, sd = 2911 * 0.02),
                                                 qnorm(quantiles[20], mean = 2558, sd = 2558 * 0.02)))))]
  
  cost_data[t2dm_stat == 0, cost_t2dm := 0]
  
  ## Incident CHD and stroke costs for people without diabetes = (- uncomplicated diabetes costs) #
  
  cost_data[sex == 1 & t2dm_stat == 0 & chd_stat == 1,
            cost_chd := fifelse(age_cost == "<50",   qnorm(quantiles[21], mean = 8894, sd = 8894 * 0.02) -
                                  qnorm(quantiles[11], mean = 3001, sd = 3001 * 0.02),
                        fifelse(age_cost == "50-59", qnorm(quantiles[22], mean = 11110, sd = 11110 * 0.02) -
                                  qnorm(quantiles[12], mean = 2889, sd = 2889 * 0.02),
                        fifelse(age_cost == "60-69", qnorm(quantiles[23], mean = 8563, sd = 8563 * 0.02) -
                                  qnorm(quantiles[13], mean = 2864, sd = 2864 * 0.02),
                        fifelse(age_cost == "70-79", qnorm(quantiles[24], mean = 8806, sd = 8806 * 0.02) -
                                  qnorm(quantiles[14], mean = 3052, sd = 3052 * 0.02),
                                                qnorm(quantiles[25], mean = 8195, sd = 8195 * 0.02) -
                                                  qnorm(quantiles[15], mean = 2388, sd = 2388 * 0.02)))))]
  
  cost_data[sex == 0 & t2dm_stat == 0 & chd_stat == 1,
            cost_chd := fifelse(age_cost == "<50",   qnorm(quantiles[26], mean = 7287, sd = 7287 * 0.02) -
                                  qnorm(quantiles[16], mean = 2102, sd = 2102 * 0.02),
                        fifelse(age_cost == "50-59", qnorm(quantiles[27], mean = 9401, sd = 9401 * 0.02) -
                                  qnorm(quantiles[17], mean = 2296, sd = 2296 * 0.02),
                        fifelse(age_cost == "60-69", qnorm(quantiles[28], mean = 9296, sd = 9296 * 0.02) -
                                  qnorm(quantiles[18], mean = 2574, sd = 2574 * 0.02),
                        fifelse(age_cost == "70-79", qnorm(quantiles[29], mean = 9123, sd = 9123 * 0.02) -
                                  qnorm(quantiles[19], mean = 2911, sd = 2911 * 0.02),
                                                qnorm(quantiles[30], mean = 8225, sd = 8225 * 0.02) -
                                                  qnorm(quantiles[20], mean = 2558, sd = 2558 * 0.02)))))]
  
  cost_data[sex == 1 & t2dm_stat == 0 & stroke_stat == 1,
            cost_stroke := fifelse(age_cost == "<50",   qnorm(quantiles[31], mean = 24804, sd = 24804 * 0.02) -
                                     qnorm(quantiles[11], mean = 3001, sd = 3001 * 0.02),
                           fifelse(age_cost == "50-59", qnorm(quantiles[32], mean = 15575, sd = 15575 * 0.02) -
                                     qnorm(quantiles[12], mean = 2889, sd = 2889 * 0.02),
                           fifelse(age_cost == "60-69", qnorm(quantiles[33], mean = 15018, sd = 15018 * 0.02) -
                                     qnorm(quantiles[13], mean = 2864, sd = 2864 * 0.02),
                           fifelse(age_cost == "70-79", qnorm(quantiles[34], mean = 14781, sd = 14781 * 0.02) -
                                     qnorm(quantiles[14], mean = 3052, sd = 3052 * 0.02),
                                                   qnorm(quantiles[35], mean = 11592, sd = 11592 * 0.02) -
                                                     qnorm(quantiles[15], mean = 2388, sd = 2388 * 0.02)))))]
  
  cost_data[sex == 0 & t2dm_stat == 0 & stroke_stat == 1,
            cost_stroke := fifelse(age_cost == "<50",   qnorm(quantiles[36], mean = 13584, sd = 13584 * 0.02) -
                                     qnorm(quantiles[16], mean = 2102, sd = 2102 * 0.02),
                           fifelse(age_cost == "50-59", qnorm(quantiles[37], mean = 13459, sd = 13459 * 0.02) -
                                     qnorm(quantiles[17], mean = 2296, sd = 2296 * 0.02),
                           fifelse(age_cost == "60-69", qnorm(quantiles[38], mean = 14684, sd = 14684 * 0.02) -
                                     qnorm(quantiles[18], mean = 2574, sd = 2574 * 0.02),
                           fifelse(age_cost == "70-79", qnorm(quantiles[39], mean = 14391, sd = 14391 * 0.02) -
                                     qnorm(quantiles[19], mean = 2911, sd = 2911 * 0.02),
                                                   qnorm(quantiles[40], mean = 12465, sd = 12465 * 0.02) -
                                                     qnorm(quantiles[20], mean = 2558, sd = 2558 * 0.02)))))]
  
  ## Incident CHD and stroke costs for people with diabetes #
  
  cost_data[sex == 1 & t2dm_stat == 1 & chd_stat == 1,
            cost_chd := fifelse(age_cost == "<50",   qnorm(quantiles[21], mean = 8894, sd = 8894 * 0.02),
                        fifelse(age_cost == "50-59", qnorm(quantiles[22], mean = 11110, sd = 11110 * 0.02),
                        fifelse(age_cost == "60-69", qnorm(quantiles[23], mean = 8563, sd = 8563 * 0.02),
                        fifelse(age_cost == "70-79", qnorm(quantiles[24], mean = 8806, sd = 8806 * 0.02),
                                                qnorm(quantiles[25], mean = 8195, sd = 8195 * 0.02)))))]
  
  cost_data[sex == 0 & t2dm_stat == 1 & chd_stat == 1,
            cost_chd := fifelse(age_cost == "<50",   qnorm(quantiles[26], mean = 7287, sd = 7287 * 0.02),
                        fifelse(age_cost == "50-59", qnorm(quantiles[27], mean = 9401, sd = 9401 * 0.02),
                        fifelse(age_cost == "60-69", qnorm(quantiles[28], mean = 9296, sd = 9296 * 0.02),
                        fifelse(age_cost == "70-79", qnorm(quantiles[29], mean = 9123, sd = 9123 * 0.02),
                                                qnorm(quantiles[30], mean = 8225, sd = 8225 * 0.02)))))]
  
  cost_data[sex == 1 & t2dm_stat == 1 & stroke_stat == 1,
            cost_stroke := fifelse(age_cost == "<50",   qnorm(quantiles[31], mean = 24804, sd = 24804 * 0.02),
                           fifelse(age_cost == "50-59", qnorm(quantiles[32], mean = 15575, sd = 15575 * 0.02),
                           fifelse(age_cost == "60-69", qnorm(quantiles[33], mean = 15018, sd = 15018 * 0.02),
                           fifelse(age_cost == "70-79", qnorm(quantiles[34], mean = 14781, sd = 14781 * 0.02),
                                                   qnorm(quantiles[35], mean = 11592, sd = 11592 * 0.02)))))]
  
  cost_data[sex == 0 & t2dm_stat == 1 & stroke_stat == 1,
            cost_stroke := fifelse(age_cost == "<50",   qnorm(quantiles[36], mean = 13584, sd = 13584 * 0.02),
                           fifelse(age_cost == "50-59", qnorm(quantiles[37], mean = 13459, sd = 13459 * 0.02),
                           fifelse(age_cost == "60-69", qnorm(quantiles[38], mean = 14684, sd = 14684 * 0.02),
                           fifelse(age_cost == "70-79", qnorm(quantiles[39], mean = 14391, sd = 14391 * 0.02),
                                                   qnorm(quantiles[40], mean = 12465, sd = 12465 * 0.02)))))]
  
  ## Prevalent CHD and stroke costs for people without diabetes = (- uncomplicated diabetes costs) #
  
  cost_data[sex == 1 & t2dm_stat == 0 & chd_stat == 2,
            cost_chd := fifelse(age_cost == "<50",   qnorm(quantiles[41], mean = 7092, sd = 7092 * 0.02) -
                                  qnorm(quantiles[11], mean = 3001, sd = 3001 * 0.02),
                        fifelse(age_cost == "50-59", qnorm(quantiles[42], mean = 4469, sd = 4469 * 0.02) -
                                  qnorm(quantiles[12], mean = 2889, sd = 2889 * 0.02),
                        fifelse(age_cost == "60-69", qnorm(quantiles[43], mean = 3614, sd = 3614 * 0.02) -
                                  qnorm(quantiles[13], mean = 2864, sd = 2864 * 0.02),
                        fifelse(age_cost == "70-79", qnorm(quantiles[44], mean = 4957, sd = 4957 * 0.02) -
                                  qnorm(quantiles[14], mean = 3052, sd = 3052 * 0.02),
                                                qnorm(quantiles[45], mean = 4832, sd = 4832 * 0.02) -
                                                  qnorm(quantiles[15], mean = 2388, sd = 2388 * 0.02)))))]
  
  cost_data[sex == 0 & t2dm_stat == 0 & chd_stat == 2,
            cost_chd := fifelse(age_cost == "<50",   qnorm(quantiles[46], mean = 2080, sd = 2080 * 0.02) -
                                  qnorm(quantiles[16], mean = 2102, sd = 2102 * 0.02),
                        fifelse(age_cost == "50-59", qnorm(quantiles[47], mean = 4649, sd = 4649 * 0.02) -
                                  qnorm(quantiles[17], mean = 2296, sd = 2296 * 0.02),
                        fifelse(age_cost == "60-69", qnorm(quantiles[48], mean = 4362, sd = 4362 * 0.02) -
                                  qnorm(quantiles[18], mean = 2574, sd = 2574 * 0.02),
                        fifelse(age_cost == "70-79", qnorm(quantiles[49], mean = 3703, sd = 3703 * 0.02) -
                                  qnorm(quantiles[19], mean = 2911, sd = 2911 * 0.02),
                                                qnorm(quantiles[50], mean = 4186, sd = 4186 * 0.02) -
                                                  qnorm(quantiles[20], mean = 2558, sd = 2558 * 0.02)))))]
  
  cost_data[sex == 1 & t2dm_stat == 0 & stroke_stat == 2,
            cost_stroke := fifelse(age_cost == "<50",   qnorm(quantiles[51], mean = 34123, sd = 34123 * 0.02) -
                                     qnorm(quantiles[11], mean = 3001, sd = 3001 * 0.02),
                           fifelse(age_cost == "50-59", qnorm(quantiles[52], mean = 14036, sd = 14036 * 0.02) -
                                     qnorm(quantiles[12], mean = 2889, sd = 2889 * 0.02),
                           fifelse(age_cost == "60-69", qnorm(quantiles[53], mean = 10534, sd = 10534 * 0.02) -
                                     qnorm(quantiles[13], mean = 2864, sd = 2864 * 0.02),
                           fifelse(age_cost == "70-79", qnorm(quantiles[54], mean = 9804, sd = 9804 * 0.02) -
                                     qnorm(quantiles[14], mean = 3052, sd = 3052 * 0.02),
                                                   qnorm(quantiles[55], mean = 5979, sd = 5979 * 0.02) -
                                                     qnorm(quantiles[15], mean = 2388, sd = 2388 * 0.02)))))]
  
  cost_data[sex == 0 & t2dm_stat == 0 & stroke_stat == 2,
            cost_stroke := fifelse(age_cost == "<50",   qnorm(quantiles[56], mean = 7958, sd = 7958 * 0.02) -
                                     qnorm(quantiles[16], mean = 2102, sd = 2102 * 0.02),
                           fifelse(age_cost == "50-59", qnorm(quantiles[57], mean = 7478, sd = 7478 * 0.02) -
                                     qnorm(quantiles[17], mean = 2296, sd = 2296 * 0.02),
                           fifelse(age_cost == "60-69", qnorm(quantiles[58], mean = 10070, sd = 10070 * 0.02) -
                                     qnorm(quantiles[18], mean = 2574, sd = 2574 * 0.02),
                           fifelse(age_cost == "70-79", qnorm(quantiles[59], mean = 9361, sd = 9361 * 0.02) -
                                     qnorm(quantiles[19], mean = 2911, sd = 2911 * 0.02),
                                                   qnorm(quantiles[60], mean = 6014, sd = 6014 * 0.02) -
                                                     qnorm(quantiles[20], mean = 2558, sd = 2558 * 0.02)))))]
  
  ## Prevalent CHD and stroke costs for people with diabetes #
  
  cost_data[sex == 1 & t2dm_stat == 1 & chd_stat == 2,
            cost_chd := fifelse(age_cost == "<50",   qnorm(quantiles[41], mean = 7092, sd = 7092 * 0.02),
                        fifelse(age_cost == "50-59", qnorm(quantiles[42], mean = 4469, sd = 4469 * 0.02),
                        fifelse(age_cost == "60-69", qnorm(quantiles[43], mean = 3614, sd = 3614 * 0.02),
                        fifelse(age_cost == "70-79", qnorm(quantiles[44], mean = 4957, sd = 4957 * 0.02),
                                                qnorm(quantiles[45], mean = 4832, sd = 4832 * 0.02)))))]
  
  cost_data[sex == 0 & t2dm_stat == 1 & chd_stat == 2,
            cost_chd := fifelse(age_cost == "<50",   qnorm(quantiles[46], mean = 2080, sd = 2080 * 0.02),
                        fifelse(age_cost == "50-59", qnorm(quantiles[47], mean = 4649, sd = 4649 * 0.02),
                        fifelse(age_cost == "60-69", qnorm(quantiles[48], mean = 4362, sd = 4362 * 0.02),
                        fifelse(age_cost == "70-79", qnorm(quantiles[49], mean = 3703, sd = 3703 * 0.02),
                                                qnorm(quantiles[50], mean = 4186, sd = 4186 * 0.02)))))]
  
  cost_data[sex == 1 & t2dm_stat == 1 & stroke_stat == 2,
            cost_stroke := fifelse(age_cost == "<50",   qnorm(quantiles[51], mean = 34123, sd = 34123 * 0.02),
                           fifelse(age_cost == "50-59", qnorm(quantiles[52], mean = 14036, sd = 14036 * 0.02),
                           fifelse(age_cost == "60-69", qnorm(quantiles[53], mean = 10534, sd = 10534 * 0.02),
                           fifelse(age_cost == "70-79", qnorm(quantiles[54], mean = 9804, sd = 9804 * 0.02),
                                                   qnorm(quantiles[55], mean = 5979, sd = 5979 * 0.02)))))]
  
  cost_data[sex == 0 & t2dm_stat == 1 & stroke_stat == 2,
            cost_stroke := fifelse(age_cost == "<50",   qnorm(quantiles[56], mean = 7958, sd = 7958 * 0.02),
                           fifelse(age_cost == "50-59", qnorm(quantiles[57], mean = 7478, sd = 7478 * 0.02),
                           fifelse(age_cost == "60-69", qnorm(quantiles[58], mean = 10070, sd = 10070 * 0.02),
                           fifelse(age_cost == "70-79", qnorm(quantiles[59], mean = 9361, sd = 9361 * 0.02),
                                                   qnorm(quantiles[60], mean = 6014, sd = 6014 * 0.02)))))]
  
  ## Fatal CHD and stroke costs for people with diabetes #
  
  cost_data[sex == 1 & t2dm_stat == 1 & stroke_stat == 3,
            cost_stroke := fifelse(age_cost == "<50",   qnorm(quantiles[61], mean = 11263, sd = 11263 * 0.02),
                        fifelse(age_cost == "50-59", qnorm(quantiles[62], mean = 11193, sd = 11193 * 0.02),
                        fifelse(age_cost == "60-69", qnorm(quantiles[63], mean = 11177, sd = 11177 * 0.02),
                        fifelse(age_cost == "70-79", qnorm(quantiles[64], mean = 11295, sd = 11295 * 0.02),
                                                qnorm(quantiles[65], mean = 10880, sd = 10880 * 0.02)))))]
  
  cost_data[sex == 0 & t2dm_stat == 1 & stroke_stat == 3,
            cost_stroke := fifelse(age_cost == "<50",   qnorm(quantiles[66], mean = 12765, sd = 12765 * 0.02),
                        fifelse(age_cost == "50-59", qnorm(quantiles[67], mean = 12874, sd = 12874 * 0.02),
                        fifelse(age_cost == "60-69", qnorm(quantiles[68], mean = 13031, sd = 13031 * 0.02),
                        fifelse(age_cost == "70-79", qnorm(quantiles[69], mean = 13220, sd = 13220 * 0.02),
                                                qnorm(quantiles[70], mean = 13021, sd = 13021 * 0.02)))))]
  
  cost_data[sex == 1 & t2dm_stat == 1 & chd_stat == 3,
            cost_chd := fifelse(age_cost == "<50",   qnorm(quantiles[71], mean = 10124, sd = 10124 * 0.02),
                           fifelse(age_cost == "50-59", qnorm(quantiles[72], mean = 10054, sd = 10054 * 0.02),
                           fifelse(age_cost == "60-69", qnorm(quantiles[73], mean = 10038, sd = 10038 * 0.02),
                           fifelse(age_cost == "70-79", qnorm(quantiles[74], mean = 10156, sd = 10156 * 0.02),
                                                   qnorm(quantiles[75], mean = 9741, sd = 9741 * 0.02)))))]
  
  cost_data[sex == 0 & t2dm_stat == 1 & chd_stat == 3,
            cost_chd := fifelse(age_cost == "<50",   qnorm(quantiles[76], mean = 24209, sd = 24209 * 0.02),
                           fifelse(age_cost == "50-59", qnorm(quantiles[77], mean = 24319, sd = 24319 * 0.02),
                           fifelse(age_cost == "60-69", qnorm(quantiles[78], mean = 24475, sd = 24475 * 0.02),
                           fifelse(age_cost == "70-79", qnorm(quantiles[79], mean = 24665, sd = 24665 * 0.02),
                                                   qnorm(quantiles[80], mean = 24466, sd = 24466 * 0.02)))))]
  
  ## Fatal CHD and stroke costs for people without diabetes #
  
  cost_data[sex == 1 & t2dm_stat == 0 & stroke_stat == 3,
            cost_stroke := fifelse(age_cost == "<50",   qnorm(quantiles[61], mean = 11263, sd = 11263 * 0.02) -
                                     qnorm(quantiles[11], mean = 3001, sd = 3001 * 0.02),
                           fifelse(age_cost == "50-59", qnorm(quantiles[62], mean = 11193, sd = 11193 * 0.02) -
                                     qnorm(quantiles[12], mean = 2889, sd = 2889 * 0.02),
                           fifelse(age_cost == "60-69", qnorm(quantiles[63], mean = 11177, sd = 11177 * 0.02) -
                                     qnorm(quantiles[13], mean = 2864, sd = 2864 * 0.02),
                           fifelse(age_cost == "70-79", qnorm(quantiles[64], mean = 11295, sd = 11295 * 0.02) -
                                     qnorm(quantiles[14], mean = 3052, sd = 3052 * 0.02),
                                                   qnorm(quantiles[65], mean = 10880, sd = 10880 * 0.02) -
                                                     qnorm(quantiles[15], mean = 2388, sd = 2388 * 0.02)))))]
  
  cost_data[sex == 0 & t2dm_stat == 0 & stroke_stat == 3,
            cost_stroke := fifelse(age_cost == "<50",   qnorm(quantiles[66], mean = 12765, sd = 12765 * 0.02) -
                                     qnorm(quantiles[16], mean = 2102, sd = 2102 * 0.02),
                           fifelse(age_cost == "50-59", qnorm(quantiles[67], mean = 12874, sd = 12874 * 0.02) -
                                     qnorm(quantiles[17], mean = 2296, sd = 2296 * 0.02),
                           fifelse(age_cost == "60-69", qnorm(quantiles[68], mean = 13031, sd = 13031 * 0.02) -
                                     qnorm(quantiles[18], mean = 2574, sd = 2574 * 0.02),
                           fifelse(age_cost == "70-79", qnorm(quantiles[69], mean = 13220, sd = 13220 * 0.02) -
                                     qnorm(quantiles[19], mean = 2911, sd = 2911 * 0.02),
                                                   qnorm(quantiles[70], mean = 13021, sd = 13021 * 0.02) -
                                                     qnorm(quantiles[20], mean = 2558, sd = 2558 * 0.02)))))]
  
  cost_data[sex == 1 & t2dm_stat == 0 & chd_stat == 3,
            cost_chd := fifelse(age_cost == "<50",   qnorm(quantiles[71], mean = 10124, sd = 10124 * 0.02) -
                                  qnorm(quantiles[11], mean = 3001, sd = 3001 * 0.02),
                        fifelse(age_cost == "50-59", qnorm(quantiles[72], mean = 10054, sd = 10054 * 0.02) -
                                  qnorm(quantiles[12], mean = 2889, sd = 2889 * 0.02),
                        fifelse(age_cost == "60-69", qnorm(quantiles[73], mean = 10038, sd = 10038 * 0.02) -
                                  qnorm(quantiles[13], mean = 2864, sd = 2864 * 0.02),
                        fifelse(age_cost == "70-79", qnorm(quantiles[74], mean = 10156, sd = 10156 * 0.02) -
                                  qnorm(quantiles[14], mean = 3052, sd = 3052 * 0.02),
                                                qnorm(quantiles[75], mean = 9741, sd = 9741 * 0.02) -
                                                  qnorm(quantiles[15], mean = 2388, sd = 2388 * 0.02)))))]
  
  cost_data[sex == 0 & t2dm_stat == 0 & chd_stat == 3,
            cost_chd := fifelse(age_cost == "<50",   qnorm(quantiles[76], mean = 24209, sd = 24209 * 0.02) -
                                  qnorm(quantiles[16], mean = 2102, sd = 2102 * 0.02),
                        fifelse(age_cost == "50-59", qnorm(quantiles[77], mean = 24319, sd = 24319 * 0.02) -
                                  qnorm(quantiles[17], mean = 2296, sd = 2296 * 0.02),
                        fifelse(age_cost == "60-69", qnorm(quantiles[78], mean = 24475, sd = 24475 * 0.02) -
                                  qnorm(quantiles[18], mean = 2574, sd = 2574 * 0.02),
                        fifelse(age_cost == "70-79", qnorm(quantiles[79], mean = 24665, sd = 24665 * 0.02) -
                                  qnorm(quantiles[19], mean = 2911, sd = 2911 * 0.02),
                                                qnorm(quantiles[80], mean = 24466, sd = 24466 * 0.02) -
                                                  qnorm(quantiles[20], mean = 2558, sd = 2558 * 0.02)))))]
  
  cost_data[chd_stat == 0, cost_chd := 0]
  cost_data[stroke_stat == 0, cost_stroke := 0]
  
  
  cost_data[, `:=`(sex = ifelse(sex == 1, "women", "men"),
                   mc = i)]
  
  cost_combine <- rbind(cost_data, cost_combine)
  
}  

write_fst(cost_combine, "./inputs/other_parameters/healthcare_costs.fst")

cost_combine[, rn := .I]
tt <- cost_combine[, .(from = min(rn), to = max(rn)), keyby = mc]
write_fst(tt, "./inputs/other_parameters/healthcare_costs_indx.fst", 100L)

### TESTING FOR COST DATA ###


#se_int <- ((8.023 - 8.012)/1.96)
#se_age50 <- ((-1.129 + 1.149)/1.96)
#se_sex <- ((0.055 - 0.047)/1.96)
#mean <- 8.023 - 1.129 + 0.055
#var <- se_int^2 + se_age^2 + 2 * se_age * 0.5 + 2 * se_sex * 0.5 
#
#shp <- (mean^2)/(var^2) 
#scl <- (var^2)/mean
#df <- as.data.table(exp(rgamma(10000, shape = shp, scale = scl)))
#
##df <- as.data.table(exp(rnorm(10000, mean = mean, sd = var)))
#
#ggplot(df, aes(V1)) + geom_density()
#
#
#exp(qgamma(0.999, shape = shp, scale = scl))
#
#
# ACTUAL UNCERTAINTY IN REGRESSION:
#
# Idea: Like prediction of standard errors! sqrt(t(newdata) %*% vcov(mod) %*% newdata)
#       newdata is predictor values!
#
#
#
