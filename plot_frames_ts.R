library(parallel)
library(data.table)
library(dplyr)
library(ggplot2)

outDir <- "/atlas_scratch/mfleonawicz/projects/map_animations"
setwd(outDir)
source("functions.R")
d.origin <- readRDS("data_monthly_original.rds")
d.mo <- readRDS("data_monthly_MA.rds")
d.ann <- readRDS("data_annual_MA.rds")
d.winter <- readRDS("data_winter_MA.rds")
d.spring <- readRDS("data_spring_MA.rds")
d.summer <- readRDS("data_summer_MA.rds")
d.autumn <- readRDS("data_autumn_MA.rds")

d.origin <- purrr::map(d.origin, ~group_by(.x, Year, Month, frameID) %>% summarise(Mean=mean(z))) %>% bind_rows
d.origin <- mutate(d.origin, Year=Year + Month/12 - 1/24, ID="monthly_original")

d.mo <- purrr::map(d.mo, ~group_by(.x, Year, Month, frameID) %>% summarise(Mean=mean(z))) %>% bind_rows
d.mo <- mutate(d.mo, Year=Year + Month/12 - 1/24, ID="monthly_MA")

prep_fun <- function(x, offset, id){
  x <- purrr::map(x, ~group_by(.x, Year, frameID) %>% summarise(Mean=mean(z))) %>% bind_rows
  mutate(x, Year=Year + 1/2, ID=id)
}

d.ann <- prep_fun(d.ann, 0.5, "annual_MA")
d.winter <- prep_fun(d.winter, 1/24, "winter_MA")
d.spring <- prep_fun(d.spring, 7/24, "spring_MA")
d.summer <- prep_fun(d.summer, 13/24, "summer_MA")
d.autumn <- prep_fun(d.autumn, 19/24, "autumn_MA")

d <- bind_rows(d.origin, d.mo, d.ann, d.winter, d.spring, d.summer, d.autumn)
xlm <- range(d$Year)
xlm <- c(floor(xlm[1]), ceiling(xlm[2]))
ylm <- range(d$Mean)
saveRDS(d, file="ts_globalMeanTemps_all.rds")
d <- d %>% split(.$ID)
gc()

# @knitr plot
for(i in seq_along(d)){
  x <- d[[i]]
  mclapply(x$frameID, save_ts, x, lab=paste0("ts_", x$ID[1]), col="white", xlm=xlm, ylm=ylm, mc.cores=32)
}
