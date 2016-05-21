library(parallel)
library(rgdal)
library(raster)
library(data.table)
library(dplyr)
library(ggplot2)

outDir <- "/atlas_scratch/mfleonawicz/projects/map_animations"
setwd(outDir)
source("functions.R")
ortho <- TRUE # set TRUE for globe plots, FALSE for flat maps
d.bath <- read.csv("../rga/marmap_coord_-180;-90;180;90_res_10.csv") %>% data.table %>% setnames(c("long", "lat", "z"))
r <- raster(extent(-180, 180, -90, 90), res=1/6)
projection(r) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
r <- setValues(r, d.bath$z)
world <- map_data("world")

# @knitr globe
n.frames <- 360
lat <- 41
d.bath <- aggregate(r, 2) %>% rasterToPoints %>% data.table %>% setnames(c("long", "lat", "z"))
z.range <- range(d.bath$z, na.rm=TRUE)
d.bath <- purrr::map(1:n.frames, ~mutate(d.bath, frameID=.x))
d.bath <- mclapply(d.bath, do_projection, lat=lat, n.frames=n.frames, mc.cores=32)
d.world <- purrr::map(1:n.frames, ~mutate(world, frameID=.x))

# @knitr plot
mclapply(d.world, save_maps, lat=lat, n.frames=n.frames,
         type="maplines", suffix="360x1_lat41_overlay_borders", mc.cores=32)
mclapply(d.bath, save_maps, lat=lat, n.frames=n.frames, col=c("black", "gray40"),
         type="maptiles", suffix="360x1_lat41_bkgd_bathymetry", z.range=z.range, mc.cores=32)

