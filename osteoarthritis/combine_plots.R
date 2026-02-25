library(magick)
#
subdir <- "osteoarthritis/"
figdir <- paste0(subdir, "figures/")


#
# Load auxiliary R functions
#

source(paste0(subdir, "funcs/gather_TEs.R"))
source(paste0(subdir, "funcs/plot_results.R"))


#
# Combine network graphs to create Figure 1(A-B)
#

outcomes <- c("Pain", "Disability")
#
file_fig1 <- paste0("netgraph_", outcomes)
files_fig1 <- paste0(figdir, file_fig1, ".tiff")
imgs_fig1 <- lapply(files_fig1, image_read)
grid_fig1 <- image_montage(
  image_join(imgs_fig1),
  tile = "2x1", # 2 columns, 1 rows
  geometry = "+10+10") # spacing between images
#
# Save combined graph
#
image_write(grid_fig1, "figures/Figure1.tiff",
  format = "tiff", density = "300x300")


#
# Combine forest plots for treatments effects to create Figure 2(A-B)
#

file_fig2 <- paste0("TE_", outcomes)
files_fig2 <- paste0(figdir, file_fig2, ".tiff")
imgs_fig2 <- lapply(files_fig2, image_read)
grid_fig2 <- image_montage(
  image_join(imgs_fig2),
  tile = "2x1", # 2 columns, 1 rows
  geometry = "+10+10") # spacing between images
#
# Save combined graph
#
image_write(grid_fig2, "figures/Figure2.tiff",
  format = "tiff", density = "300x300")
