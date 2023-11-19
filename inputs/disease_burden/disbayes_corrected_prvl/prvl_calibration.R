library(CKutils)
library(fst)
library(data.table)

# load 2001 CHD corrected prvl
t1 <- read_fst("inputs/disease_burden/disbayes_corrected_prvl/chd_men_2001.fst", columns = c("age", "year", "sex", "mu"), as.data.table = TRUE)
t2 <- read_fst("inputs/disease_burden/disbayes_corrected_prvl/chd_women_2001.fst", columns = c("age", "year", "sex", "mu"), as.data.table = TRUE)
disb <- rbind(t1, t2)
disb <- clone_dt(disb, times = 18)
disb[, `:=` (year = year + 12L - .id, .id = NULL)]

# load 2013 CHD corrected prvl
t1 <- read_fst("inputs/disease_burden/disbayes_corrected_prvl/chd_men_2013.fst", columns = c("age", "year", "sex", "mu"), as.data.table = TRUE)
t2 <- read_fst("inputs/disease_burden/disbayes_corrected_prvl/chd_women_2013.fst", columns = c("age", "year", "sex", "mu"), as.data.table = TRUE)
t1 <- rbind(t1, t2)
t1 <- clone_dt(t1, times = 7)
t1[, `:=` (year = year - 1L + .id, .id = NULL)]
disb <- rbind(disb, t1)
setnames(disb, "mu", "mu_disbayes")

# load GBD prvl
chdgbd <- read_fst("inputs/disease_burden/chd_prvl.fst", as.data.table = TRUE)
absorb_dt(chdgbd, disb[age >= 30])
chdgbd[, `:=` (prvl_mltp = mu_disbayes/mu, mu_disbayes = NULL)]
write_fst(chdgbd, "inputs/disease_burden/chd_prvl.fst")
chdgbd[age == 99 & year %in% c(2001, 2013)]



# load 2001 stroke corrected prvl
t1 <- read_fst("inputs/disease_burden/disbayes_corrected_prvl/stroke_men_2001.fst", columns = c("age", "year", "sex", "mu"), as.data.table = TRUE)
t2 <- read_fst("inputs/disease_burden/disbayes_corrected_prvl/stroke_women_2001.fst", columns = c("age", "year", "sex", "mu"), as.data.table = TRUE)
disb <- rbind(t1, t2)
disb <- clone_dt(disb, times = 18)
disb[, `:=` (year = year + 12L - .id, .id = NULL)]

# load 2013 stroke corrected prvl
t1 <- read_fst("inputs/disease_burden/disbayes_corrected_prvl/stroke_men_2013.fst", columns = c("age", "year", "sex", "mu"), as.data.table = TRUE)
t2 <- read_fst("inputs/disease_burden/disbayes_corrected_prvl/stroke_women_2013.fst", columns = c("age", "year", "sex", "mu"), as.data.table = TRUE)
t1 <- rbind(t1, t2)
t1 <- clone_dt(t1, times = 7)
t1[, `:=` (year = year - 1L + .id, .id = NULL)]
disb <- rbind(disb, t1)
setnames(disb, "mu", "mu_disbayes")

# load GBD prvl
strokegbd <- read_fst("inputs/disease_burden/stroke_prvl.fst", as.data.table = TRUE)
absorb_dt(strokegbd, disb[age >= 30])
strokegbd[, `:=` (prvl_mltp = mu_disbayes/mu, mu_disbayes = NULL)]
write_fst(strokegbd, "inputs/disease_burden/stroke_prvl.fst")
strokegbd[age == 90 & year %in% c(2001, 2013)]
