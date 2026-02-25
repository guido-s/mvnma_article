#
# Install R package mvnma
#
# install.packages("remotes")
# remotes::install_github("TEvrenoglou/mvnma", ref = "main")


#
# Load libraries
#

library(mvnma)
library(tidyverse)
library(mvtnorm)


#
# Load auxiliary R functions to calculate mP-scores
#

source("funcs/Pscores_function.R")
source("funcs/league_table.R")
source("funcs/league_table_var.R")
source("funcs/intersect2.R")
source("funcs/Prepare_Multi.R")
source("funcs/Prepare_Single.R")
source("funcs/Prepare_function.R")
source("funcs/pscrs.R")
source("funcs/pscore_graph.R")


#
# Some settings
#

subdir <- "antidepressants/"
# Only consider random effects model
settings.meta(common = FALSE)


#
# Use pairwise() to obtain contrast based data for each of the five
# outcomes of interest
#

# Early response
#
pw1 <- pairwise(treat = list(treatment1, treatment2, treatment3),
  event = list(resp1, resp2, resp3), n = list(n1, n2, n3),
  studlab = id, data = dat.linde2015, sm = "OR")
#
# Early remissions
#
pw2 <- pairwise(treat = list(treatment1, treatment2, treatment3),
  event = list(remi1, remi2, remi3), n = list(n1, n2, n3),
  studlab = id, data = dat.linde2015, sm = "OR")
#
# Adverse events
#
pw3 <- pairwise(treat = list(treatment1, treatment2,treatment3),
  event = list(ae1, ae2, ae3),  n = list(n1, n2, n3),
  studlab = id, data = dat.linde2015, sm = "OR")
#
# Loss to follow-up
#
pw4 <- pairwise(treat = list(treatment1, treatment2, treatment3),
  event = list(loss1, loss2, loss3), n = list(n1, n2, n3),
  studlab = id, data = dat.linde2015, sm = "OR")
#
# Loss_to_follow_up_(AE)
#
pw5 <- pairwise(treat = list(treatment1, treatment2, treatment3),
  event = list(loss.ae1, loss.ae2, loss.ae3), n = list(n1, n2, n3),
  studlab = id, data = dat.linde2015, sm = "OR", allstudies = TRUE)
#
# Define outcome labels
#
outcomes <- c("Early_Response", "Early_Remission", "Adverse_events",
  "Loss_to_follow_up", "Loss_to_follow_up_AE")


#
#
# Run mvnma() and store rds-files
#
#

# Define lower and upper bounds for uniform priors of the correlation
# coefficients: 
# rho12, rho13, rho14, rho15, rho23, rho24, rho25, rho34, rho35, rho45 where 
# 1: Early_Response, 2: Early_Remission, 3: Adverse Events,
# 4: Loss to follow up, 5: Loss_to_follow_up_AE
# lb.rho <- c(0, -1, -1, -1, -1, -1, -1, 0, 0, 0)
# ub.rho <- c(1, 0, 0, 0, 0, 0, 0, 1, 1, 1)
#
# set.seed(1909)
# Fit the mvNMA(standard) model
# mvNMA.standard<- mvnma(pw1, pw2, pw3, pw4, pw5,
#   lower.rho = lb.rho, upper.rho = ub.rho,
#   reference.group = "Placebo", 
#   outclab = outcomes,
#   n.iter = 30000, n.burnin = 10000, n.thin = 20)
# #
# saveRDS(mvNMA.standard,
#   file = paste0(subdir, "results/mvNMA.standard.rds"))
# 
# Fit the mvNMA(DM) model
# mvNMA.DM <- mvnma(pw1, pw2, pw3, pw4, pw5,
#   lower.rho = lb.rho, upper.rho = ub.rho,
#   method = "DM", reference.group = "Placebo", 
#   outclab = outcomes,
#   n.iter = 30000, n.burnin = 10000, n.thin = 20)
# #
# saveRDS(mvNMA.DM, file = paste0(subdir, "results/mvNMA.DM.rds"))


#
#
# Load results stored in "results" folder
#
#

# mvNMA(standard)
mvNMA.standard <- readRDS(paste0(subdir, "results/mvNMA.standard.rds"))
# mvNMA(DM)
mvNMA.DM <- readRDS(paste0(subdir, "results/mvNMA.DM.rds"))
#
# Get correlations
mvNMA.standard$cor
mvNMA.DM$cor
#
# Save model results in separate forest plots
forest(mvNMA.standard, file = paste0(subdir, "figures/forest_standard.pdf"))
forest(mvNMA.DM, file = paste0(subdir, "figures/forest_DM.pdf"))


#
#
# Rankings
#
#

# Define direction of effect for each outcome
#
sv <- c("undesirable", "undesirable", "desirable", "desirable", "desirable")
type <-
  as.character(sv, levels = c("undesirable", "desirable"), labels = c("B", "H"))

# Outcome-specific SUCRA values for mvNMA(standard)
#
ranks_standard_sucra <- mvrank(mvNMA.standard, small.values = sv)
ranks_standard_sucra

# Outcome-specific median ranks for mvNMA(standard)
#
ranks_standard_median <-
  mvrank(mvNMA.standard, small.values = sv, method = "ranks")
ranks_standard_median

# VIKOR based on mvNMA(standard) SUCRA
#
vikor(ranks_standard_sucra)

# Fuzzy VIKOR based on mvNMA(standard) median ranks
#
vikor(ranks_standard_median)

# Outcome-specific SUCRA values for mvNMA(DM)
#
ranks_DM_sucra <- mvrank(mvNMA.DM, small.values = sv)
ranks_DM_sucra

# Outcome-specific median ranks for mvNMA(DM)
#
ranks_DM_median <- mvrank(mvNMA.DM, small.values = sv, method = "ranks")
ranks_DM_median

# VIKOR based on mvNMA(DM) SUCRA
#
vikor(ranks_DM_sucra)

# Fuzzy VIKOR
#
vikor(ranks_DM_median)


#
#
# Network meta-analyses for each outcome separately
#
#

# Early Response
#
nma1 <- netmeta(pw1, reference.group = "Placebo", small.values = sv[1])
saveRDS(nma1, file = paste0(subdir, "results/nma1.rds"))

# Early Remission
#
nma2 <- netmeta(pw2, reference.group = "Placebo", small.values = sv[2])
saveRDS(nma2, file = paste0(subdir, "results/nma2.rds"))

# Adverse events
#
nma3 <- netmeta(pw3, reference.group = "Placebo", small.values = sv[3])
saveRDS(nma3, file = paste0(subdir, "results/nma3.rds"))

# Loss to follow up
#
nma4 <- netmeta(pw4, reference.group = "Placebo", small.values = sv[4])
saveRDS(nma4, file = paste0(subdir, "results/nma4.rds"))

# Loss to follow up due to AE
#
nma5 <- netmeta(pw5, reference.group = "Placebo", small.values = sv[5])
saveRDS(nma5, file = paste0(subdir, "results/nma5.rds"))


#
#
# Network graphs
#
#

filename <- paste0(subdir, "figures/netgraph_Response.tiff")
tiff(filename = filename, width = 2200, height = 2200,res = 300)
ng1 <- netgraph(nma1, seq = "o",
  pch = 21, cex.points = 3,
  col.points = "red", bg.points = "red",
  lwd.max = max(nma1$A.matrix),
  rotate = 4 / n * 360)
trts1 <- ng1$nodes$trts
dev.off()
#
filename <- paste0(subdir, "figures/netgraph_Remission.tiff")
tiff(filename = filename, width = 2200, height = 2200, res = 300)
netgraph(nma2, seq = trts1[trts1 %in% nma2$trts],
  pch = 21, cex.points = 3,
  col.points = "red", bg.points = "red",
  lwd.max = max(nma2$A.matrix),
  rotate = 4 / n * 360)
dev.off()
#
filename <- paste0(subdir, "figures/netgraph_AE.tiff")
tiff(filename = filename, width = 2200, height = 2200, res = 300)
netgraph(nma3, seq = trts1[trts1 %in% nma3$trts],
  pch = 21, cex.points = 3,
  col.points = "red", bg.points = "red",
  lwd.max = max(nma3$A.matrix),
  rotate = 3 / n * 360)
dev.off()
#
filename <- paste0(subdir, "figures/netgraph_Loss_to_follow_up.tiff")
tiff(filename = filename, width = 2200, height = 2200, res = 300)
netgraph(nma4, seq = trts1[trts1 %in% nma4$trts],
  pch = 21, cex.points = 3,
  col.points = "red", bg.points = "red",
  lwd.max = max(nma4$A.matrix),
  rotate = 4 / n * 360)
dev.off()
#
filename <- paste0(subdir, "figures/netgraph_Loss_to_follow_up_AE.tiff")
tiff(filename = filename, width = 2200, height = 2200,  res = 300)
netgraph(nma5, seq = trts1[trts1 %in% nma5$trts],
  pch = 21, cex.points = 3,
  col.points = "red", bg.points = "red",
  lwd.max = max(nma5$A.matrix),
  rotate = 4 / n * 360)
dev.off()


#
#
# Outcome specific ranking using P-scores
#
#

# Early Response
#
netrank(nma1)

# Early Remission
#
netrank(nma2)

# Adverse Events (AE)
#
netrank(nma3)

# Loss to follow up
#
netrank(nma4)

# Loss to follow up due to AE
#
netrank(nma5)


#
#
# mP-scores across outcomes
#
#

# mP-scores assuming 0 between-outcomes correlation
#
p_scores(list(nma1, nma2, nma3, nma4, nma5),
  CIV = c(0, 0, 0, 0, 0), type = type)

# mP-scores assuming between-outcomes correlation equal to the correlation
# obtained from mvNMA
# create the 5x5 correlation matrix
#
correlation <- diag(1, 5)
m1 <- 1 - correlation
m1[upper.tri(m1)] <- 0
m1[m1 == 1] <- mvNMA.standard$cor$mean
m1 <- m1 + t(m1)
m1 <- m1 + correlation
correlation <- m1
#
# calculate mP-scores
p_scores(list(nma1, nma2, nma3, nma4, nma5),
  CIV = c(0, 0, 0, 0, 0), type = type, correlation = correlation)
