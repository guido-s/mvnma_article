# Install "mvnma" 
# install.packages("remotes")
# remotes::install_github("TEvrenoglou/mvnma", ref = "main")
#
# Load libraries
library(mvnma)
library(tidyverse)
library(mvtnorm)
#
# Load the antidepressants dataset
data("Linde2015")
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
#
# Use 'pairwise' to obtain contrast based data for each one of the five outcomes of interest
# Early response
p1 <- pairwise(treat = list(treatment1, treatment2, treatment3),
               event = list(resp1, resp2, resp3), n = list(n1, n2, n3),
               studlab = id, data = dat.linde2015, sm = "OR")
# Early remissions
p2 <- pairwise(treat = list(treatment1, treatment2, treatment3),
               event = list(remi1, remi2, remi3), n = list(n1, n2, n3),
               studlab = id, data = dat.linde2015, sm = "OR")
# Adverse events
p3 <- pairwise(treat = list(treatment1, treatment2,treatment3),
               event = list(ae1, ae2, ae3),  n = list(n1, n2, n3),
               studlab = id, data = dat.linde2015, sm = "OR")
# Loss to follow-up
p4 <- pairwise(treat = list(treatment1, treatment2, treatment3),
               event = list(loss1, loss2, loss3), n = list(n1, n2, n3),
               studlab = id, data = dat.linde2015, sm = "OR")
# Loss_to_follow_up_(AE)
p5 <- pairwise(treat = list(treatment1, treatment2, treatment3),
               event = list(loss.ae1, loss.ae2, loss.ae3), n = list(n1, n2, n3),
               studlab = id, data = dat.linde2015, sm = "OR",allstudies = TRUE)
# Define outcome labels
outcomes <- c("Early_Response", "Early_Remission","Adverse_events", "Loss_to_follow_up", "Loss_to_follow_up_AE" )
# Define lower and upper bounds for uniform priors of the correlation coefficients: 
# rho12,rho13,rho14,rho15, rho23,rho24,rho25,rho34,rho35,rho45 where 
#1:Early_Response, 2:Early_Remission, 3:Adverse Events, 4: Loss to follow up and 5: Loss_to_follow_up_AE 
lb.rho <- c(0,-1,-1,-1,-1,-1,-1,0,0,0)
ub.rho <- c(1,0,0,0,0,0,0,1,1,1)
# Fit the mvNMA(standard) model
# set.seed(1909)
# mvNMA.standard<- mvnma(p1,p2,p3,p4,p5,
#                         lower.rho = lb.rho,
#                         upper.rho = ub.rho,
#                         reference.group = "Placebo", 
#                         outclab = outcomes,
#                         n.iter = 30000, 
#                         n.burnin = 10000,
#                         n.thin = 20)
# 
# Fit the mvNMA(DM) model
# mvNMA.DM <- mvnma(p1,p2,p3,p4,p5,
#                   lower.rho = lb.rho,
#                   upper.rho = ub.rho,
#                   method = "DM",
#                   reference.group = "Placebo", 
#                   outclab = outcomes,
#                   n.iter = 30000, 
#                   n.burnin = 10000,
#                   n.thin = 20)
#
# Read results directly from the "Model_files" folder
# mvNMA(standard)
mvNMA.standard <- readRDS(paste(getwd(),"/Analysis_antidepressants/Model_files/mvNMA.standard.rds",sep = ""))
# mvNMA(DM)
mvNMA.DM <- readRDS(paste(getwd(),"/Analysis_antidepressants/Model_files/mvNMA.DM.rds",sep = ""))
#
# Get correlations
mvNMA.standard$cor
mvNMA.DM$cor
#
# save each model results on separate forest plots
forest(mvNMA.standard,file="Analysis_antidepressants/Results/forest_standard.pdf")
forest(mvNMA.DM,file="Analysis_antidepressants/Results/forest_DM.pdf")
#
# RANKING
#
# Define small.values for each outcome
small.values <- c("undesirable","undesirable","desirable","desirable","desirable")
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
#
# Network meta-analysis for each outcome separately
#
# Early Response
net1 <- netmeta(p1,reference.group = "Placebo",common = FALSE,small.values = small.values[1])
# Early Remission
net2 <- netmeta(p2,reference.group = "Placebo",common = FALSE,small.values =small.values[2])
# Adverse events
net3 <- netmeta(p3,reference.group = "Placebo",common = FALSE,small.values =small.values[3])
# Loss to follow up
net4 <- netmeta(p4,reference.group = "Placebo",common = FALSE,small.values =small.values[4])
# Loss to follow up due to AE
net5 <- netmeta(p5,reference.group = "Placebo",common = FALSE,small.values =small.values[5])
#
# Network graphs
filename <- "Analysis_antidepressants/Results/netgraph_Response.tiff"
tiff(filename = filename,width = 2200,height = 2200,res = 300)
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
         lwd.max = max(net1$A.matrix))
dev.off()
#
filename <- "Analysis_antidepressants/Results/netgraph_Remission.tiff"
tiff(filename = filename,width = 2200,height = 2200, res = 300)
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
         lwd.max = max(net2$A.matrix))
dev.off()
#
filename <- "Analysis_antidepressants/Results/netgraph_AE.tiff"
tiff(filename = filename,width = 2200,height = 2200, res = 300)
netgraph(net3,
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
         lwd.max = max(net3$A.matrix))
dev.off()
#
filename <- "Analysis_antidepressants/Results/netgraph_Loss_to_follow_up.tiff"
tiff(filename = filename,width = 2200,height = 2200, res = 300)
netgraph(net4,
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
         lwd.max = max(net4$A.matrix))
dev.off()
#
filename <- "Analysis_antidepressants/Results/netgraph_Loss_to_follow_up_AE.tiff"
tiff(filename = filename,width = 2200,height = 2200,  res = 300)
netgraph(net5,
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
         lwd.max = max(net5$A.matrix))
dev.off()
#
# outcome specific ranking using P-scores
# Early Response
netrank(net1)
# Early Remission
netrank(net2)
# Adverse Events (AE)
netrank(net3)
# Loss to follow up
netrank(net4)
# Loss to follow up due to AE
netrank(net5)
#
# mP-scores across outcomes
# save all netmeta objects
t <-list(net1,net2,net3,net4,net5)
#
# mP-scores assuming 0 between-outcomes correlation
mPscores1 <- p_scores(x=t,CIV=c(0,0,0,0,0),correlation=NULL,type=c("B","B","H","H","H"))
# print results
mPscores1
#
# mP-scores assuming between-outcomes correlation equal to the correlation obtained from mvNMA
# create the 5x5 correlation matrix
correlation <- diag(1,5)
m1 <- 1- correlation
m1[upper.tri(m1)] <- 0
m1[m1 == 1] <- mvNMA.standard$cor$mean
m1 <- m1 +t(m1)
m1 <- m1+correlation
correlation <- m1
#
# calculate mP-scores
mPscores2 <- p_scores(x=t,CIV=c(0,0,0,0,0),correlation=correlation,type=c("B","B","H","H","H"))
# print results
mPscores2