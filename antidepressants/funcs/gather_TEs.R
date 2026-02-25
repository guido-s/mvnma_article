## Read model results

mvNMA.standard <- readRDS(paste0(subdir, "results/mvNMA.standard.rds"))
mvNMA.DM <- readRDS(paste0(subdir, "results/mvNMA.DM.rds"))
nma1 <- readRDS(paste0(subdir, "results/nma1.rds"))
nma2 <- readRDS(paste0(subdir, "results/nma2.rds"))
nma3 <- readRDS(paste0(subdir, "results/nma3.rds"))
nma4 <- readRDS(paste0(subdir, "results/nma4.rds"))
nma5 <- readRDS(paste0(subdir, "results/nma5.rds"))   


#
#
# 1) Early Response 
#
#

basic1 <- mvNMA.standard$Early_Response$basic_estimates
#
res1_mvnma_TE <-
  data.frame(treat = row.names(basic1), est = basic1$mean, se = basic1$sd)
#
res1_mvnma_TE$mod <- "mvNMA(standard)"
#
basic1 <- mvNMA.DM$Early_Response$basic_estimates
#
res1_mvnmaDM_TE <-
  data.frame(treat = row.names(basic1), est = basic1$mean, se = basic1$sd)
#
res1_mvnmaDM_TE$mod <- "mvNMA(DM)"
#
res1_nma_TE <- data.frame(treat = names(nma1$TE.random[, "Placebo"]),
  est =  unname(nma1$TE.random[, "Placebo"]),
  se =  unname(nma1$seTE.random[, "Placebo"]))
#
res1_nma_TE$mod <- "NMA"
#
res1 <- rbind(res1_mvnma_TE, res1_mvnmaDM_TE, res1_nma_TE)
#
res1$outcome = "Early_Response"


#
#
# 2) Early Remission
#
#

basic2 <- mvNMA.standard$Early_Remission$basic_estimates
#
res2_mvnma_TE <-
  data.frame(treat = row.names(basic2), est = basic2$mean, se = basic2$sd)
#
res2_mvnma_TE$mod <- "mvNMA(standard)"
#
basic2 <- mvNMA.DM$Early_Remission$basic_estimates
#
res2_mvnmaDM_TE <-
  data.frame(treat = row.names(basic2), est = basic2$mean, se = basic2$sd)
#
res2_mvnmaDM_TE$mod <- "mvNMA(DM)"
#
res2_nma_TE <- data.frame(treat = names(nma2$TE.random[, "Placebo"]),
  est =  unname(nma2$TE.random[, "Placebo"]),
  se =  unname(nma2$seTE.random[, "Placebo"]))
#
res2_nma_TE$mod <- "NMA"
#
res2 <- rbind(res2_mvnma_TE, res2_mvnmaDM_TE, res2_nma_TE)
#
res2$outcome = "Early_Remission"


#
#
# 3) Adverse Events
#
#

basic3 <- mvNMA.standard$Adverse_events$basic_estimates
#
res3_mvnma_TE <-
  data.frame(treat = row.names(basic3), est = basic3$mean, se = basic3$sd)
#
res3_mvnma_TE$mod <- "mvNMA(standard)"
#
basic3 <- mvNMA.DM$Adverse_events$basic_estimates
#
res3_mvnmaDM_TE <-
  data.frame(treat = row.names(basic3), est = basic3$mean, se = basic3$sd)
#
res3_mvnmaDM_TE$mod <- "mvNMA(DM)"
#
res3_nma_TE <- data.frame(treat = names(nma3$TE.random[, "Placebo"]),
  est =  unname(nma3$TE.random[, "Placebo"]),
  se =  unname(nma3$seTE.random[, "Placebo"]))
#
res3_nma_TE$mod <- "NMA"
#
res3 <- rbind(res3_mvnma_TE, res3_mvnmaDM_TE, res3_nma_TE)
#
res3$outcome = "Adverse_events"


#
#
# 4) Loss to follow-up
#
#

basic4 <- mvNMA.standard$Loss_to_follow_up$basic_estimates
#
res4_mvnma_TE <-
  data.frame(treat = row.names(basic4), est = basic4$mean, se = basic4$sd)
#
res4_mvnma_TE$mod <- "mvNMA(standard)"
#
basic4 <- mvNMA.DM$Loss_to_follow_up$basic_estimates
#
res4_mvnmaDM_TE <-
  data.frame(treat = row.names(basic4), est = basic4$mean, se = basic4$sd)
#
res4_mvnmaDM_TE$mod <- "mvNMA(DM)"
#
res4_nma_TE <- data.frame(treat = names(nma4$TE.random[, "Placebo"]),
  est =  unname(nma4$TE.random[, "Placebo"]),
  se =  unname(nma4$seTE.random[, "Placebo"]))
#
res4_nma_TE$mod <- "NMA"
#
res4 <- rbind(res4_mvnma_TE, res4_mvnmaDM_TE, res4_nma_TE)
#
res4$outcome = "Loss_to_follow_up"


#
#
# 5) Loss to follow up due to AE
#
#

basic5 <- mvNMA.standard$Loss_to_follow_up_AE$basic_estimates
#
res5_mvnma_TE <-
  data.frame(treat = row.names(basic5), est = basic5$mean, se = basic5$sd)
#
res5_mvnma_TE$mod <- "mvNMA(standard)"
#
basic5 <- mvNMA.DM$Loss_to_follow_up_AE$basic_estimates
#
res5_mvnmaDM_TE <-
  data.frame(treat = row.names(basic5), est = basic5$mean, se = basic5$sd)
#
res5_mvnmaDM_TE$mod <- "mvNMA(DM)"
#
res5_nma_TE <- data.frame(treat = names(nma5$TE.random[, "Placebo"]),
  est =  unname(nma5$TE.random[, "Placebo"]),
  se =  unname(nma5$seTE.random[, "Placebo"]))
#
res5_nma_TE$mod <- "NMA"
#
res5 <- rbind(res5_mvnma_TE, res5_mvnmaDM_TE, res5_nma_TE)
#
res5$outcome = "Loss_to_follow_up_AE"


### Combine everything

res_all <- rbind(res1, res2, res3, res4, res5)
