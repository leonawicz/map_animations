get_clim <- function(x, yr, mo){
  x <- purrr::map(x, ~as.matrix(.x))
  x <- purrr::map(unique(mo), ~x[yr >=1961 & yr <= 1990 & mo==.x])
  purrr::map(x, ~Reduce("+", .x)[,3]/30)
}

get_ma <- function(x, type, season=NULL, size=10, output="list", n.cores=32){
  if(!(type %in% c("monthly", "annual", "seasonal"))) stop("invalid type.")
  if(type=="monthly"){
    x <- mclapply(x,
                  function(x, size) group_by(x, Month, long, lat) %>%
                    mutate(z=roll_mean(z, size, fill=NA), idx=NULL) %>% filter(!is.na(z)),
                  size=size, mc.cores=n.cores)
  }
  if(type=="annual"){
    x <- mclapply(x,
                  function(x, size) group_by(x, long, lat, Year) %>%
                    summarise(z=mean(z)) %>% mutate(z=roll_mean(z, size, fill=NA), idx=NULL) %>% filter(!is.na(z)),
                  size=size, mc.cores=n.cores)
  }
  if(type=="seasonal"){
    if(is.null(season) || !(season %in% c("winter", "spring", "summer", "autumn")))
      stop("If res='seasonal', season must be 'winter', 'spring', 'summer' or 'autumn'.")
    idx <- switch(season, winter=c(12,1,2), spring=3:5, summer=6:8, autumn=9:11)
    yr.lim <- range(x[[1]]$Year)
    x <- mclapply(x,
                  function(x, size){
                    mutate(x, Year=ifelse(Month==12, Year+1, Year), Month=ifelse(Month %in% idx, 1, 0)) %>%
                      filter(Year > yr.lim[1] & Year <= yr.lim[2] & Month==1) %>%
                      group_by(long, lat, Month, Year) %>% summarise(z=mean(z)) %>%
                      mutate(z=roll_mean(z, size, fill=NA), Month=NULL, idx=NULL) %>% filter(!is.na(z))
                  }, size=size, mc.cores=n.cores)
  }
  #arr <- if(type=="annual") list("Year", "long", "lat") else list("Year", "Month", "long", "lat")
  x <- bind_rows(x) %>% group_by # %>% arrange_(.dots=arr)
  x <- if(type %in% c("seasonal", "annual")) x %>% split(.$Year) else x %>% split(paste(.$Year, .$Month+9))
  x
}

project_to_hemisphere <- function(lat, long, lat0, long0){
  hold <- cbind(lat, long)
  x <- purrr::map(list(lat, lat0, long-long0), ~.x*pi/180)
  inview <- sin(x[[1]])*sin(x[[2]]) + cos(x[[1]])*cos(x[[2]])*cos(x[[3]]) > 0
  data.table(long=hold[,2], lat=hold[,1], inview=inview)
}

pad_frames <- function(x, n.period=360, rotation="add", force=TRUE){
  n <- length(x)
  if(n >= n.period & !force) return(x)
  if(rotation=="add") x2 <- purrr::map(1:(n.period-1), ~x[[n]] %>% mutate(frameID=.x + n))
  if(rotation=="pad") x2 <- purrr::map(1:(n.period-n), ~x[[n]] %>% mutate(frameID=.x + n))
  c(x, x2)
}

get_lonlat_seq <- function(lon, lat, n.period=360, n.frames=n.period){
  if(length(lon) != 1 & length(lon) != n.period) stop("lon must be length one or length n.period")
  if(length(lat) != 1 & length(lat) != n.period) stop("lat must be length one or length n.period")
  if(length(lon)==1){
    lon <- rep(rev(seq(lon, lon+360, length.out=n.period + 1)[-(n.period + 1)]), length=n.frames)
    lon[lon >= 360] <- lon[lon >= 360] - 360
  }
  if(length(lat)==1){
    if(lat < -90 || lat > 90) stop("lat invalid")
    lat <- rep(lat, n.frames)
  }
  list(lon=lon, lat=lat)
}

do_projection <- function(x, lon=0, lat=0, n.period=360, n.frames=n.period){
  i <- x$frameID[1]
  lonlat <- get_lonlat_seq(lon, lat, n.period, n.frames)
  left_join(x, project_to_hemisphere(x$lat, x$long, lonlat$lat[i], lonlat$lon[i])) %>%
    filter(inview) %>% dplyr::select(-inview)
}

theme_blank <- function(){
  eb <- element_blank()
  theme(axis.line=eb, axis.text.x=eb, axis.text.y=eb,
    axis.ticks=eb, axis.title.x=eb, axis.title.y=eb, legend.position="none",
    panel.background=eb, panel.border=eb, panel.grid.major=eb, panel.grid.minor=eb,
    plot.background=element_rect(colour="transparent", fill="transparent"))
}

theme_axes_only <- function(col){
  eb <- element_blank()
  el <- element_line(colour=col)
  theme(axis.line=el, axis.line.x=el, axis.line.y=el, axis.ticks=el,
        axis.text=element_text(colour=col, size=18), legend.position="none",
        panel.background=eb, panel.border=eb, panel.grid.major=eb, panel.grid.minor=eb,
        plot.background=element_rect(colour="transparent", fill="transparent"))
}

save_maps <- function(x, lon=0, lat=0, n.period=360, n.frames=n.period, col=NULL, type="network", suffix=NULL, z.range=NULL){
  if(is.null(col)) col <- switch(type,
    network=c("#FFFFFF25", "#1E90FF25", "#FFFFFF", "#1E90FF50"),
    maptiles=c("black", "steelblue4"),
    maplines="white")
  i <- x$frameID[1]
  lonlat <- get_lonlat_seq(lon, lat, n.period, n.frames)
  if(type=="network") x.lead <- group_by(x, group) %>% slice(n())
  g <- ggplot(x, aes(long, lat))
  if(type=="maptiles"){
    if(is.null(z.range)) z.range <- range(x$z, na.rm=TRUE)
    g <- ggplot(x, aes(long, lat, fill=z)) + geom_tile() +
      scale_fill_gradientn(colors=col, limits=z.range)
  } else {
    g <- ggplot(x, aes(long, lat, group=group))
    if(type=="maplines") g <- g + geom_path(colour=col)
    if(type=="network") g <- g + geom_path(colour=col[2]) + geom_path(colour=col[1]) +
        geom_point(data=x.lead, colour=col[3], size=0.6) +
        geom_point(data=x.lead, colour=col[4], size=0.3)
  }

  g <- g + theme_blank() + coord_map("ortho", orientation=c(lonlat$lat[i], lonlat$lon[i], 23.4))
  if(is.character(suffix)) type <- paste(type, suffix, sep="_")
  dir.create(outDir <- file.path("frames", type), recursive=TRUE, showWarnings=FALSE)
  png(sprintf(paste0(outDir, "/", type, "_%04d.png"), i),
      width=4*1920, height=4*1080, res=300, bg="transparent")
  print(g)
  dev.off()
  NULL
}

save_ts <- function(i, x, label, col, xlm, ylm, axes_only=FALSE){
  x <- filter(x, frameID <= i)
  g <- ggplot(x, aes(Year, Mean))
  if(axes_only){
    if(i!=1) return()
    g <- g + scale_x_continuous(name="", breaks=seq(xlm[1], xlm[2], by=10), limits=xlm) +
      scale_y_continuous(name="", limits=ylm) + theme_axes_only(col)
    dir.create(outDir <- "frames", showWarnings=FALSE)
    png(paste0(outDir, "/ts_axes_fixed_bkgd.png"), width=4*1920, height=4*1080, res=300, bg="transparent")
    print(g)
    dev.off()
    return()
  }
  g <- g + geom_line(colour=col, size=1) + xlim(xlm) + ylim(ylm) + theme_blank()
  dir.create(outDir <- file.path("frames", label), recursive=TRUE, showWarnings=FALSE)
  png(sprintf(paste0(outDir, "/", label, "_%04d.png"), i),
      width=4*1920, height=4*1080, res=300, bg="transparent")
  print(g)
  dev.off()
  NULL
}
