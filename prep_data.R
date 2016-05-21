# @knitr setup
library(parallel)
library(rgdal)
library(raster)
library(RcppRoll)
library(data.table)
library(dplyr)

dir.create(outDir <- "/atlas_scratch/mfleonawicz/projects/map_animations", showWarnings=FALSE)
setwd(outDir)
source("functions.R")
mainDir <- "/Data/Base_Data/Climate/World/World_10min"
histDir <- "historical/AR5_CMIP5_models/GISS-E2-R/tas"
projDir <- "projected/AR5_CMIP5_models/rcp60/GISS-E2-R/tas"
files.hist <- list.files(file.path(mainDir, histDir), full=TRUE)
files.proj <- list.files(file.path(mainDir, projDir), full=TRUE)
files <- c(files.hist, files.proj)
mo <- strsplit(basename(files), "_") %>% purrr::map(7) %>% unlist
yr <- strsplit(basename(files), "_") %>% purrr::map(~substr(.x[8], 1, 4)) %>% unlist
ord <- order(paste(yr, mo))
mo <- mo[ord]
yr <- yr[ord]
files <- files[ord]

# @knitr load_data
d.list <-mclapply(seq_along(files),
  function(i, files){
    print(length(files)-i)
    raster(files[i]) %>% rotate %>% aggregate(2) %>% rasterToPoints %>% data.table %>% setnames(c("long", "lat", "z"))
  }, files=files, mc.cores=32)

# @knitr climate_deltas
clim <- get_clim(d.list, yr, mo)
d.list <- purrr::map2(d.list, mo, ~mutate(.x, z=z - clim[[match(.y, unique(mo))]]))
d.list <- purrr::map2(d.list, yr, ~mutate(.x, Year=as.numeric(.y)))
d.list <- purrr::map2(d.list, mo, ~mutate(.x, Month=as.numeric(.y)))

# @knitr moving_average
res <- "monthly_original" # use "monthly_original" as default, otherwise "annual", "seasonal", "monthly"
if(res %in% c("annual", "seasonal", "monthly")){
  idx <- sort(rep(1:32, length=nrow(d.list[[1]])))
  d.list <- purrr::map(d.list, ~mutate(.x, idx=idx)) %>% bind_rows
  d.list <- d.list %>% split(.$idx)
  gc()
  system.time( d.list <- get_ma(d.list, res) )
}
gc()

d.list <- purrr::map2(d.list, seq_along(d.list), ~mutate(.x, frameID=.y))
outfile <- paste0("data_", ifelse(res=="monthly_original", res, paste0(res, "_MA")), ".rds")
saveRDS(d.list, file=outfile)
