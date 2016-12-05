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
    raster(files[i]) %>% aggregate(2) %>% rotate %>% rasterToPoints %>% data.table %>% mutate(Year=yr[i], Month=mo[i]) %>% setnames(c("long", "lat", "z", "Year", "Month"))
  }, files=files, mc.cores=32) %>% bind_rows

# @knitr climate_deltas
clim <- filter(d.list, Year > 1960 & Year <= 1990) %>% group_by(long, lat, Month) %>% summarise(z=mean(z))
d.list <- left_join(d.list, clim, c("long", "lat", "Month")) %>% mutate(z=z.x-z.y) %>% select(-z.x, -z.y) %>% arrange(Year, Month, long, lat)
#clim <- get_clim(d.list)
#d.list <- purrr::map2(d.list, mo, ~mutate(.x, z=z - clim[[match(.y, unique(mo))]]))
#d.list <- purrr::map2(d.list, yr, ~mutate(.x, Year=as.numeric(.y)))
#d.list <- purrr::map2(d.list, mo, ~mutate(.x, Month=as.numeric(.y)))

# @knitr moving_average
res <- "seasonal" # use "monthly_original" as default, otherwise annual, seasonal, monthly
season <- "winter" # winter, spring, summer, autumn. Ignored if res != seasonal
season.idx <- switch(season, winter=c(12,1,2), spring=3:5, summer=6:8, autumn=9:11)
if(res %in% c("annual", "seasonal", "monthly")){
  idx <- rep(sort(rep(1:32, length=nrow(d.list)/length(files))), length(files))
  d.list <- mutate(d.list, idx=idx)
  #idx <- sort(rep(1:32, length=nrow(d.list[[1]])))
  #d.list <- purrr::map(d.list, ~mutate(.x, idx=idx)) %>% bind_rows
  if(res=="seasonal") d.list <- filter(d.list, Month %in% season.idx)
  d.list <- d.list %>% split(.$idx)
  gc()
  system.time( d.list <- get_ma(d.list, res, season) )
}
gc()

d.list <- purrr::map2(d.list, seq_along(d.list), ~mutate(.x, frameID=.y))
outfile <- paste0("data_", switch(res, monthly_original="monthly_original", monthly="monthly_MA", annual="annual_MA", seasonal=paste0(season, "_MA")), ".rds")
saveRDS(d.list, file=outfile)
