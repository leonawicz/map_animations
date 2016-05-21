library(parallel)
library(data.table)
library(dplyr)
library(ggplot2)
library(RColorBrewer)

outDir <- "/atlas_scratch/mfleonawicz/projects/map_animations"
setwd(outDir)
source("functions.R")
ortho <- TRUE # set TRUE for globe plots, FALSE for flat maps
res <- "annual" # use "monthly_original" as default, otherwise "annual", "seasonal", "monthly"
infile <- paste0("data_", ifelse(res=="monthly_original", res, paste0(res, "_MA")), ".rds")

d.list <- readRDS(infile)
z.range <- purrr::map(d.list, ~range(.x$z, na.rm=TRUE)) %>% unlist %>% range
n.frames <- length(d.list)

world <- map_data("world")

# @knitr globe
if(ortho){
  d.list <- pad_frames(d.list)
  n.frames <- length(d.list)
  d.list <- mclapply(d.list, do_projection, lat=41, n.frames=n.frames, mc.cores=32)
  d.world <- purrr::map(1:360, ~mutate(world, frameID=.x))
}

# @knitr plot
pal <- rev(brewer.pal(11,"RdYlBu"))
suffix <- paste(ifelse(res=="monthly_original", res, paste0(res, "_MA")), ifelse(ortho, "globe", "flat"), sep="_")
mclapply(d.list, save_maps, lat=41, n.frames=n.frames, col=pal, type="maptiles", suffix=suffix, z.range=z.range, mc.cores=32)
