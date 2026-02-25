library(magick)
#
subdir <- "antidepressants/"
figdir <- paste0(subdir, "figures/")


#
# Load auxiliary R functions
#

source(paste0(subdir, "funcs/gather_TEs.R"))
source(paste0(subdir, "funcs/plot_results.R"))


#
# Combine network graphs to create Figure 3(A-E)
#

outcomes <- c("Response", "Remission", "AE",
  "Loss_to_follow_up", "Loss_to_follow_up_AE")
#
file_fig3 <- paste0("netgraph_", outcomes)
files_fig3 <- paste0(figdir, file_fig3, ".tiff")
imgs_fig3 <- lapply(files_fig3, image_read)
grid_fig3 <- image_montage(
  image_join(imgs_fig3),
  tile = "3x2", # 3 columns, 2 rows
  geometry = "+10+10") # spacing between images
#
# Save combined graph
#
image_write(grid_fig3,"figures/Figure3.tiff",
  format = "tiff", density = "300x300")


#
# Combine forest plots for treatments effects to create Figure 4(A-E)
#

file_fig4 <- paste0("TE_", outcomes)
files_fig4 <- paste0(figdir, file_fig4, ".tiff")
imgs_fig4 <- lapply(files_fig4, image_read)
grid_fig4 <- image_montage(
  image_join(imgs_fig4),
  tile = "3x2", # 3 columns, 2 rows
  geometry = "+10+10") # spacing between images
#
# Save combined graph
#
image_write(grid_fig4, "figures/Figure4.tiff",
  format = "tiff", density = "300x300")
