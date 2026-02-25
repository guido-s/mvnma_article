library(meta)
settings.meta(common = FALSE)
#
library(tidyverse)
library(ggplot2)
library(ggsci)


mods <- unique(res_all$mod)

out <- unique(res_all$outcome)

res_all$color <-
  as.character(factor(res_all$mod, levels = mods,
                      labels = c("blue", "chartreuse3", "black")))

res_all$treat <- factor(res_all$treat, levels = unique(res_all$treat))


#
#
# 1) Create graph for Pain
#
#

dat1 <- res_all %>% filter(outcome == out[1], treat != "No treatment")
#
mod1 <-
  metagen(est, se, subgroup = treat, studlab = mod, data = dat1, sm = "MD")
#
filename1 <- paste0(subdir, "figures/TE_Pain.tiff")
tiff(filename = filename1, width = 2000, height = 3600, res = 300)
forest(mod1,
       subgroup.name = "",
       header.line = TRUE,
       smlab = "",
       leftcols = "studlab",
       leftlabs = "Comparison\nTreatment vs No treatment",
       xlab = "Mean difference",
       weight.study = "same",
       label.left = "Favors Treatment",
       label.right = "Favors No treatment",
       col.study = dat1$color,
       col.square = dat1$color,
       col.square.lines = dat1$color,
       addrow.overall = FALSE, hetstat = FALSE,
       overall = FALSE, subgroup = FALSE,
       subgroup.hetstat = FALSE, test.subgroup = FALSE)
dev.off()


#
#
# 2) Create graph for Disability
#
#

dat2 <- res_all %>% filter(outcome == out[2], treat != "No treatment")
#
mod2 <-
  metagen(est, se, subgroup = treat, studlab = mod, data = dat2, sm = "MD")
#
filename2 <- paste0(subdir, "figures/TE_Disability.tiff")
tiff(filename = filename2, width = 2000, height = 3600, res = 300)
forest(mod2,
       subgroup.name = "",
       header.line = TRUE,
       smlab = "",
       leftcols = "studlab",
       leftlabs = "Comparison\nTreatment vs No treatment",
       xlab = "Mean difference",
       weight.study = "same",
       label.left = "Favors Treatment",
       label.right = "Favors No treatment",
       col.study = dat2$color,
       col.square = dat2$color,
       col.square.lines = dat2$color,
       addrow.overall = FALSE, hetstat = FALSE,
       overall = FALSE, subgroup = FALSE,
       subgroup.hetstat = FALSE, test.subgroup = FALSE)
dev.off()
