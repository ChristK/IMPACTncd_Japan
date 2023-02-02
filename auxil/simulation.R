#setwd("/home/karlemmert-fees/IMPACT NCD Germany")
source("./global.R")
source("./auxil/scenarios.R")

runif(1)

IMPACTncd <- Simulation$new("./inputs/sim_design.yaml") 
#IMPACTncd$del_outputs()$del_logs()
#g <- IMPACTncd$get_causal_structure(print_plot = T)

# Original iterations #
batch_size <- 3
iterations <- 101
batches <- split(seq(1, iterations),
                 f = findInterval(seq(1, iterations),
                                  vec = seq(1, iterations, batch_size)))

batches <- batches[31:34]
# Missing iterations due to errors #
# 
# errors <- list(c(148L, 63L, 136L, 149L, 82L),
#                 c(144L, 92L, 116L, 41L, 122L),
#                 c(113L, 146L, 138L))
# 
# batches <- batches[c(30, 13, 28, 17, 29, 19, 24, 9, 25, 23)]

for(i in batches){

scenario_fn <- function(sp) NULL

IMPACTncd$
  run(i, multicore = TRUE, "sc0", m_zero_trend = -0.03, p_zero_trend = 0)

scenario_fn <- scenario_1_fn

IMPACTncd$
  run(i, multicore = TRUE, "sc1", m_zero_trend = -0.03, p_zero_trend = 0)

scenario_fn <- scenario_2_fn

IMPACTncd$
  run(i, multicore = TRUE, "sc2", m_zero_trend = -0.03, p_zero_trend = 0)

scenario_fn <- scenario_3_fn

IMPACTncd$
  run(i, multicore = TRUE, "sc3", m_zero_trend = -0.03, p_zero_trend = 0)

scenario_fn <- scenario_4_fn

IMPACTncd$
  run(i, multicore = TRUE, "sc4", m_zero_trend = -0.03, p_zero_trend = 0)

}
IMPACTncd$export_summaries(multicore = TRUE)

#source("./validation_internal/internal_validation_plots.R")
