## Quick C++ Implementation Testing Script
## 
## Simple and fast testing for C++ implementations
##
## Usage:
##   source("scripts/quick_test.R")

library(Rcpp)
library(data.table)
library(qs2)

qs2::qs_readm("auxil/cpp_testing/test_data/tmp_spfor_test.qs")
popref <- copy(sp$pop)

sourceCpp("auxil/cpp_testing/scripts/IMPACTncd_sim_first_paper.cpp") # simcpp() renamed to simcpp_ref() and is the benchmark
sourceCpp("Rpackage/IMPACTncd_Japan_model_pkg/src/IMPACTncd_sim.cpp") # simcpp() is the function under test

print("Benchmark timing:")
print(system.time(simcpp_ref(popref, l, sp$mc)))
print("Test timing:")
print(system.time(simcpp(sp$pop, l, sp$mc)))

print("All EQUAL???")
print(all.equal(popref, sp$pop)) # Check if results are identical