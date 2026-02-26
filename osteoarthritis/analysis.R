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

subdir <- "osteoarthritis/"
# Only consider random effects model
settings.meta(common = FALSE)


#
# Load osteoarthritis dataset
#

data <- read.csv(paste0(subdir, "data/data_Hong.csv"))


#
# Use pairwise() to obtain contrast based data for each of the two
# outcomes of interest
#

pw1 <- pairwise(data = data,
  treat = treatment,
  n = n, mean = mean_pain, sd = sd_pain,
  sm = "MD",
  studlab = study)
#
pw2 <- pairwise(data = data,
  treat = treatment,
  n = n, mean = mean_disability, sd = sd_disability,
  sm = "MD",
  studlab = study)
#
# Define outcome labels
#
outcomes <- c("Pain", "Disability")


#
#
# Run mvnma() and store rds-files
#
#


# Fit the mvNMA(standard) model
# set.seed(1909)
# mvNMA.standard <- mvnma(pw1,pw2,
#   reference.group = "No treatment",
#   outclab = outcomes,
#   n.iter = 30000, n.burnin = 10000)
# #
# saveRDS(mvNMA.standard,
#   file = paste0(subdir, "results/mvNMA.standard.rds"))
# 
# Fit the mvNMA(DM) model 
# mvNMA.DM <- mvnma(pw1,pw2,
#   reference.group = "No treatment",
#   method = "DM",
#   outclab = outcomes,
#   n.iter = 30000, n.burnin = 10000)
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
sv <- rep("desirable", 2)
type <- rep("H", 2)

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

# Pain
#
nma1 <- netmeta(pw1, reference.group = "No treatment", small.values = sv[1])
saveRDS(nma1, file = paste0(subdir, "results/nma1.rds"))

# Disability
#
nma2 <- netmeta(pw2, reference.group = "No treatment", small.values = sv[2])
saveRDS(nma2, file = paste0(subdir, "results/nma2.rds"))


#
#
# Network graphs
#
#

filename <- paste0(subdir, "figures/netgraph_pain.tiff")
tiff(filename = filename, width = 2250, height = 2250,res = 300)
ng1 <- netgraph(nma1, seq = "o",
  pch = 21, cex.points = 3,
  col.points = "red", bg.points = "red",
  lwd.max = max(nma1$A.matrix),
  rotate = 10)
trts1 <- ng1$nodes$trts
dev.off()
#
filename <- paste0(subdir, "figures/netgraph_disability.tiff")
tiff(filename = filename, width = 2250, height = 2250, res = 300)
netgraph(nma2, seq = trts1[trts1 %in% nma2$trts],
         pch = 21, cex.points = 3,
         col.points = "red", bg.points = "red",
         lwd.max = max(nma2$A.matrix),
         rotate = 10)
dev.off()


#
#
# Outcome specific ranking using P-scores
#
#

# Pain
#
netrank(nma1)

# Disability
#
netrank(nma2)


#
#
# mP-scores across outcomes
#
#

# mP-scores assuming 0 between-outcomes correlation
#
p_scores(list(nma1, nma2), CIV = c(0, 0), type = type)

# mP-scores assuming between-outcomes correlation equal to the correlation
# obtained from mvNMA
# create the 2x2 correlation matrix
#
correlation <- diag(1, 2)
m1 <- 1 - correlation
m1[upper.tri(m1)] <- 0
m1[m1 == 1] <- mvNMA.standard$cor$mean
m1 <- m1 + t(m1)
m1 <- m1 + correlation
correlation <- m1
#
# calculate mP-scores
p_scores(list(nma1, nma2), CIV = c(0, 0), type = type,
  correlation = correlation)
