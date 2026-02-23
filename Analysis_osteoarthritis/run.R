# Install "mvnma" 
# install.packages("remotes")
# remotes::install_github("TEvrenoglou/mvnma", ref = "main")
#
# Load libraries
library(mvnma)
library(tidyverse)
library(mvtnorm)
#
# Load osteoarthritis dataset
data <- read.csv("Analysis_osteoarthritis/Data/data_Hong.csv")
# Load helpers functions used to calculated mP-scores
source("helpers/Pscores_function.R")
source("helpers/league_table.R")
source("helpers/league_table_var.R")
source("helpers/intersect2.R")
source("helpers/Prepare_Multi.R")
source("helpers/Prepare_Single.R")
source("helpers/Prepare_function.R")
source("helpers/pscrs.R")
source("helpers/pscore_graph.R")

# Use 'pairwise' to obtain contrast based data for each one of the two outcomes of interest
p1 <- pairwise(data = data,
               treat = treatment,
               mean = mean_pain,
               n = n,
               sd = sd_pain,
               sm = "MD",
               reference.group = "No treatment",
               studlab = study)
#
p2 <- pairwise(data = data,
               treat = treatment,
               mean = mean_disability,
               n = n,
               sd = sd_disability,
               sm = "MD",
               reference.group = "No treatment",
               studlab = study)
#
# Define outcome labels
outcomes = c("Pain","Disability")
#
# Fit the mvNMA(standard) model
# set.seed(1909)
# 
# mvNMA.standard <- mvnma(p1,p2,
#                         reference.group = "No treatment",
#                         outclab = outcomes,
#                         n.iter = 30000,
#                         n.burnin = 10000)
# 
# Fit the mvNMA(DM) model 
# mvNMA.DM <- mvnma(p1,p2,
#                   reference.group = "No treatment",
#                   method = "DM",
#                   outclab = outcomes,
#                   n.iter = 30000,
#                   n.burnin = 10000)

# Read results directly from the "Model_files" folder
# mvNMA(standard)
mvNMA.standard <- readRDS("Analysis_osteoarthritis/Model_files/mvNMA.standard.rds")
# mvNMA(DM)
mvNMA.DM <- readRDS("Analysis_osteoarthritis/Model_files/mvNMA.DM.rds")
#
# Get correlations
mvNMA.standard$cor
mvNMA.DM$cor
#
# save each model results on separate forest plots
forest(mvNMA.standard,file="Analysis_osteoarthritis/Results/forest_standard.pdf")
forest(mvNMA.DM,file="Analysis_osteoarthritis/Results/forest_DM.pdf")
#
# Define small.values for each outcome
small.values = c("desirable","desirable")  
#
# Outcome-specific SUCRA values for mvNMA(standard)
ranks_standard_sucra <- mvrank(mvNMA.standard,small.values = small.values)
# print results
ranks_standard_sucra
#
# Outcome-specific median ranks for mvNMA(standard)
ranks_standard_median <- mvrank(mvNMA.standard,small.values = small.values,method = "ranks")
# print results
ranks_standard_median
#
# VIKOR based on mvNMA(standard) SUCRA
vikor_standard_sucra <- vikor(ranks_standard_sucra)
# print results
vikor_standard_sucra
#
# Fuzzy VIKOR based on mvNMA(standard) median ranks
vikor_standard_ranks <- vikor(ranks_standard_median)
# print results
vikor_standard_ranks
#
# Outcome-specific SUCRA values for mvNMA(DM)
ranks_DM_sucra <- mvrank(mvNMA.DM,small.values = small.values)
# print results
ranks_DM_sucra
#
# Outcome-specific median ranks for mvNMA(DM)
ranks_DM_median <- mvrank(mvNMA.DM,small.values = small.values,method = "ranks")
# print results
ranks_DM_median
#
# VIKOR based on mvNMA(DM) SUCRA
vikor_DM_sucra <- vikor(ranks_DM_sucra)
# print results
vikor_DM_sucra
#
# Fuzzy VIKOR
vikor_DM_ranks <- vikor(ranks_DM_median)
# print results
vikor_DM_ranks
#
# Network meta-analysis for each outcome separately
# Pain
net1 <- netmeta(p1,small.values = small.values[1],common = FALSE)
# Disability
net2 <- netmeta(p2,small.values = small.values[2],common = FALSE)
#
# Network graphs
filename <- "Analysis_osteoarthritis/Results/netgraph_pain.tiff"
tiff(filename = filename, width = 2200, height = 2200, res = 300)
netgraph(net1,
         col = "black",
         plastic = FALSE,
         points = TRUE,
         pch=21,
         cex.points = 3,
         col.points = "red",
         bg.points = "red",
         thickness = "number",
         multiarm = FALSE,
         number.of.studies = TRUE,
         start.layout = "circle",
         lwd.max = max(net1$A.matrix))
dev.off()
#
filename <- "Analysis_osteoarthritis/Results/netgraph_disability.tiff"
tiff(filename = filename, width = 2200, height = 2200, res = 300)
netgraph(net2,
         col = "black",
         plastic = FALSE,
         points = TRUE,
         pch=21,
         cex.points = 3,
         col.points = "red",
         bg.points = "red",
         thickness = "number",
         multiarm = FALSE,
         number.of.studies = TRUE,
         start.layout = "circle",
         lwd.max = max(net2$A.matrix))
dev.off()
#
# outcome specific ranking using P-scores
# Pain
netrank(net1)
# Disability
netrank(net2)
#
# mP-scores across outcomes
# save all netmeta objects
t <- list(net1,net2)
#
# mP-scores assuming 0 between-outcomes correlation
mPscores1 <- p_scores(x=t,CIV=c(0,0),correlation=NULL,type=c("H","H"))
# print results
mPscores1
# mP-scores assuming between-outcomes correlation equal to the correlation obtained from mvNMA
# create the 2x2 correlation matrix
correlation=matrix(c(1,0.78,0.78,1),2,2)
mPscores2 <- p_scores(x=t,CIV=c(0,0),correlation=correlation,type=c("H","H"))
mPscores2