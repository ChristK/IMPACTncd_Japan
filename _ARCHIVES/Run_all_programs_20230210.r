setwd("~/IMPACT_NCD_data/IMPACTncd_Japan")

#source("./global.R")

library(fst)
library(dqrng) 
library(stringr)
library(qs)
library(data.table)
library(gamlss)
library(IMPACTncdJapan)
library(CKutils)
dependencies(yaml::read_yaml("./dependencies.yaml"))

design_ <- Design$new("./inputs/sim_design.yaml")
design_

sp <- SynthPop$new(1L, design_)
sp

sp$pop
se$pop[]
