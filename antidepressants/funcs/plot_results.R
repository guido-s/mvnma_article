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
# 1) Create graph for Early Response
#
#

dat1 <- res_all %>% filter(outcome == out[1], treat != "Placebo")
#
mod1 <-
  metagen(est, se, subgroup = treat, studlab = mod, data = dat1, sm = "OR")
#
filename1 <- paste0(subdir, "figures/TE_Response.tiff")
tiff(filename = filename1, width = 2000, height = 3000, res = 300)
forest(mod1,
       subgroup.name = "",
       header.line = TRUE,
       smlab = "",
       leftcols = "studlab",
       leftlabs = "Comparison\nTreatment vs Placebo",
       xlab = "Odds ratio",
       weight.study = "same",
       label.left = "Favors Placebo",
       label.right = "Favors Treatment",
       col.study = dat1$color,
       col.square = dat1$color,
       col.square.lines = dat1$color,
       addrow.overall = FALSE, hetstat = FALSE,
       overall = FALSE, subgroup = FALSE,
       subgroup.hetstat = FALSE, test.subgroup = FALSE)
dev.off()


#
#
# 2) Create graph for Early Remission
#
#

dat2 <- res_all %>% filter(outcome == out[2], treat != "Placebo")
#
mod2 <-
  metagen(est, se, subgroup = treat, studlab = mod, data = dat2, sm = "OR")
#
filename2 <- paste0(subdir, "figures/TE_Remission.tiff")
tiff(filename = filename2, width = 2000, height = 3000, res = 300)
forest(mod2,
       subgroup.name = "",
       header.line = TRUE,
       smlab = "",
       leftcols = "studlab",
       leftlabs = "Comparison\nTreatment vs Placebo",
       xlab = "Odds ratio",
       weight.study = "same",
       label.left = "Favors Placebo",
       label.right = "Favors Treatment",
       col.study = dat2$color,
       col.square = dat2$color,
       col.square.lines = dat2$color,
       addrow.overall = FALSE, hetstat = FALSE,
       overall = FALSE, subgroup = FALSE,
       subgroup.hetstat = FALSE, test.subgroup = FALSE)
dev.off()


#
#
# 3) Create graph for Adverse Events
#
#

dat3 <- res_all %>% filter(outcome == out[3], treat != "Placebo")
#
mod3 <-
  metagen(est, se, subgroup = treat, studlab = mod, data = dat3, sm = "OR")
#
filename3 <- paste0(subdir, "figures/TE_AE.tiff")
tiff(filename = filename3, width = 2000, height = 3000, res = 300)
forest(mod3,
       subgroup.name = "",
       header.line = TRUE,
       smlab = "",
       leftcols = "studlab",
       leftlabs = "Comparison\nTreatment vs Placebo",
       xlab = "Odds ratio",
       weight.study = "same",
       label.left = "Favors Placebo",
       label.right = "Favors Treatment",
       col.study = dat3$color,
       col.square = dat3$color,
       col.square.lines = dat3$color,
       addrow.overall = FALSE, hetstat = FALSE,
       overall = FALSE, subgroup = FALSE,
       subgroup.hetstat = FALSE, test.subgroup = FALSE)
dev.off()


#
#
# 4) Create graph for Loss to follow-up
#
#

dat4 <- res_all %>% filter(outcome == out[4], treat != "Placebo")
#
mod4 <-
  metagen(est, se, subgroup = treat, studlab = mod, data = dat4, sm = "OR")
#
filename4 <- paste0(subdir, "figures/TE_Loss_to_follow_up.tiff")
tiff(filename = filename4, width = 2000, height = 3000, res = 300)
forest(mod4,
       subgroup.name = "",
       header.line = TRUE,
       smlab = "",
       leftcols = "studlab",
       leftlabs = "Comparison\nTreatment vs Placebo",
       xlab = "Odds ratio",
       weight.study = "same",
       label.left = "Favors Placebo",
       label.right = "Favors Treatment",
       col.study = dat4$color,
       col.square = dat4$color,
       col.square.lines = dat4$color,
       addrow.overall = FALSE, hetstat = FALSE,
       overall = FALSE, subgroup = FALSE,
       subgroup.hetstat = FALSE, test.subgroup = FALSE)
dev.off()


#
#
# 5) Create graph for Loss to follow-up AE
#
#

dat5 <- res_all %>% filter(outcome == out[5], treat != "Placebo")
#
mod5 <-
  metagen(est, se, subgroup = treat, studlab = mod, data = dat5, sm = "OR")
#
filename5 <- paste0(subdir, "figures/TE_Loss_to_follow_up_AE.tiff")
tiff(filename = filename5, width = 2000, height = 3000, res = 300)
forest(mod5,
       subgroup.name = "",
       header.line = TRUE,
       smlab = "",
       leftcols = "studlab",
       leftlabs = "Comparison\nTreatment vs Placebo",
       xlab = "Odds ratio",
       weight.study = "same",
       label.left = "Favors Placebo",
       label.right = "Favors Treatment",
       col.study = dat5$color,
       col.square = dat5$color,
       col.square.lines = dat5$color,
       addrow.overall = FALSE, hetstat = FALSE,
       overall = FALSE, subgroup = FALSE,
       subgroup.hetstat = FALSE, test.subgroup = FALSE)
dev.off()
