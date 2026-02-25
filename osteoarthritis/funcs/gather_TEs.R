## Read model results

mvNMA.standard <- readRDS(paste0(subdir, "results/mvNMA.standard.rds"))
mvNMA.DM <- readRDS(paste0(subdir, "results/mvNMA.DM.rds"))
nma1 <- readRDS(paste0(subdir, "results/nma1.rds"))
nma2 <- readRDS(paste0(subdir, "results/nma2.rds"))


#
#
# 1) Pain
#
#

basic1 <- mvNMA.standard$Pain$basic_estimates
#
res1_mvnma_TE <-
  data.frame(treat = row.names(basic1), est = basic1$mean, se = basic1$sd)
#
res1_mvnma_TE$mod <- "mvNMA(standard)"
#
basic1 <- mvNMA.DM$Pain$basic_estimates
#
res1_mvnmaDM_TE <-
  data.frame(treat = row.names(basic1), est = basic1$mean, se = basic1$sd)
#
res1_mvnmaDM_TE$mod <- "mvNMA(DM)"
#
res1_nma_TE <- data.frame(treat = names(nma1$TE.random[, "No treatment"]),
  est =  unname(nma1$TE.random[, "No treatment"]),
  se =  unname(nma1$seTE.random[, "No treatment"]))
#
res1_nma_TE$mod <- "NMA"
#
res1 <- rbind(res1_mvnma_TE, res1_mvnmaDM_TE, res1_nma_TE)
#
res1$outcome = "Pain"


#
#
# 2) Disability
#
#

basic2 <- mvNMA.standard$Disability$basic_estimates
#
res2_mvnma_TE <-
  data.frame(treat = row.names(basic2), est = basic2$mean, se = basic2$sd)
#
res2_mvnma_TE$mod <- "mvNMA(standard)"
#
basic2 <- mvNMA.DM$Disability$basic_estimates
#
res2_mvnmaDM_TE <-
  data.frame(treat = row.names(basic2), est = basic2$mean, se = basic2$sd)
#
res2_mvnmaDM_TE$mod <- "mvNMA(DM)"
#
res2_nma_TE <- data.frame(treat = names(nma2$TE.random[, "No treatment"]),
  est =  unname(nma2$TE.random[, "No treatment"]),
  se =  unname(nma2$seTE.random[, "No treatment"]))
#
res2_nma_TE$mod <- "NMA"
#
res2 <- rbind(res2_mvnma_TE, res2_mvnmaDM_TE, res2_nma_TE)
#
res2$outcome = "Disability"


### Combine everything

res_all <- rbind(res1, res2)
