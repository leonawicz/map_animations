library(parallel)
library(data.table)
library(dplyr)
library(ggplot2)
library(RColorBrewer)

outDir <- "/atlas_scratch/mfleonawicz/projects/map_animations"
setwd(outDir)
source("functions.R")
ortho <- TRUE # set TRUE for globe plots, FALSE for flat maps
res <- "monthly" # use "monthly_original" as default, otherwise annual, seasonal, monthly
season <- "winter" # winter, spring, summer, autumn. Ignored if res != seasonal
infile <- paste0("data_", switch(res, monthly_original="monthly_original", monthly="monthly_MA", annual="annual_MA", seasonal=paste0(season, "_MA")), ".rds")

d.list <- readRDS(infile)
z.range <- purrr::map(d.list, ~range(.x$z, na.rm=TRUE)) %>% unlist %>% range
n.frames <- length(d.list)

# @knitr globe
if(ortho){
  lat <- 41
  d.list <- pad_frames(d.list)
  n.frames <- length(d.list)
  d.list <- mclapply(d.list, do_projection, lat=lat, n.frames=n.frames, mc.cores=32)
}
gc()

# @knitr plot
pal <- rev(brewer.pal(11,"RdYlBu"))
suffix <- paste(ifelse(ortho, "360x1_lat41", "flat"), switch(res, monthly_original="monthly_original", monthly="monthly_MA", annual="annual_MA", seasonal=paste0(season, "_MA")), sep="_")
mclapply(d.list, save_maps, lat=lat, n.frames=n.frames, col=pal, type="maptiles", suffix=suffix, z.range=z.range, mc.cores=32)
