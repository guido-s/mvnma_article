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
library(ggplot2)


#
# Code obtained from the original publication by Daly et al. (2020)
# https://pubmed.ncbi.nlm.nih.gov/33115431/
#

source("funcs/code_spie_chart.R")


#
# Load SUCRA values as obtained from Table 4 in Daly et al. (2020)
#

data <- read.csv("psoriasis_supplement/data.csv")
row.names(data) <- data$treat
data$treat <- NULL
dat <- data %>% 
  dplyr::select(SUCRA1, SUCRA2, SUCRA3, SUCRA4)
#
# Apply VIKOR to the SUCRA values
res <- mvnma:::vikor_internal(dat, weights =  NULL, v = 0.5)
res <- res %>% 
  mutate_if(is.numeric, round, 2)
res

#
# Calculate spie area for all treatments across outcomes
#

x <- s <- spie_area <- vector("list")

for (i in 1:nrow(dat)) {
  x[[i]] <- unname(unlist(c(dat[i,])))
  
  s[[i]] <- spie.chart(outcome = x[[i]],
                       theta = rep(0.5 * pi, 4),
                       outcome.range = c(0, 1),
                       outcome.label = paste("Outcome", 1:4))
  spie_area[[i]] <-
    as.numeric(str_trim(gsub("Area inside spie chart = ", "", s[[i]][[2]])))
  names(spie_area[[i]]) <- row.names(dat)[i]
}
#
unlist(spie_area)
