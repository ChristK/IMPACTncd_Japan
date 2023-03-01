

#source("./global.R")
library(parallel)
library(stats)   
library(graphics)  
library(grDevices) 
library(utils)     
library(datasets)  
library(methods)   
library(base)     
library(fstcore)
library(IMPACTncdJapan) 
library(data.table) 
library(igraph)        
library(piggyback)
library(digest)
library(future.apply)
library(doFuture)
library(future)
library(promises)
library(htmltools)
library(dichromat)
library(colourpicker)
library(viridis)
library(viridisLite)
library(plotly)
library(DT)
library(shinyWidgets)
library(shinyjs)
library(shinythemes)
library(bsplus)
library(shinyBS)
library(shinydashboard)
library(shiny)
library(ggplot2)
library(wrswoR)
library(fst)
library(qs)
library(dqrng)
library(gamlss.dist)
library(MASS)
library(doRNG)
library(rngtools)
library(doParallel)
library(iterators)
library(foreach)
library(yaml)
library(CKutils)




setwd("/home/rstudio/IMPACT_NCD_data/IMPACTncd_Japan")
dependencies(yaml::read_yaml("./dependencies.yaml"))
design_ <- Design$new("./inputs/sim_design.yaml")
sp <- SynthPop$new(1L, design_)
sp

sp$pop
se$pop[]








loaded via a namespace (and not attached):
 [1] colorspace_2.0-3    ellipsis_0.3.2      rprojroot_2.0.3     mc2d_0.1-21        
 [5] rstudioapi_0.14     roxygen2_7.2.1      listenv_0.8.0       remotes_2.4.2      
 [9] fansi_1.0.3         mvtnorm_1.1-3       lubridate_1.8.0     xml2_1.3.3         
[13] codetools_0.2-18    splines_4.2.1       logging_0.10-108    cachem_1.0.6       
[17] knitr_1.40          pkgload_1.3.0       jsonlite_1.8.0      compiler_4.2.1     
[21] httr_1.4.4          assertthat_0.2.1    Matrix_1.5-1        fastmap_1.1.0      
[25] lazyeval_0.2.2      cli_3.4.0           later_1.3.0         prettyunits_1.1.1  
[29] tools_4.2.1         gtable_0.3.1        glue_1.6.2          dplyr_1.0.10       
[33] Rcpp_1.0.9          jquerylib_0.1.4     vctrs_0.4.1         nlme_3.1-159       
[37] xfun_0.33           stringr_1.4.1       globals_0.16.1      ps_1.7.1           
[41] mime_0.12           miniUI_0.1.1.1      lifecycle_1.0.2     scales_1.2.1       
[45] gamlss.data_6.0-2   curl_4.3.2          memoise_2.0.1       gridExtra_2.3      
[49] sass_0.4.2          stringi_1.7.8       desc_1.4.2          pkgbuild_1.3.1     
[53] rlang_1.0.5         pkgconfig_2.0.3     BH_1.78.0-0         lattice_0.20-45    
[57] purrr_0.3.4         htmlwidgets_1.5.4   cowplot_1.1.1       tidyselect_1.1.2   
[61] processx_3.7.0      parallelly_1.32.1   magrittr_2.0.3      R6_2.5.1           
[65] generics_0.1.3      DBI_1.1.3           pillar_1.8.1        withr_2.5.0        
[69] survival_3.4-0      tibble_3.1.8        crayon_1.5.1        gamlss_5.4-3       
[73] utf8_1.2.2          RApiSerialize_0.1.2 grid_4.2.1          callr_3.7.2        
[77] xtable_1.8-4        tidyr_1.2.1         httpuv_1.6.6        RcppParallel_5.1.5 
[81] munsell_0.5.0       stringfish_0.15.7   bslib_0.4.0        