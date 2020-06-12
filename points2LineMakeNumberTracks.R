library(sp)
library(maptools)

points_to_line <- function(data, long, lat, id_field = NULL, sort_field = NULL) {
  
  # Convert to SpatialPointsDataFrame
  coordinates(data) <- c(long, lat)
  
  # If there is a sort field...
  if (!is.null(sort_field)) {
    if (!is.null(id_field)) {
      data <- data[order(data[[id_field]], data[[sort_field]]), ]
    } else {
      data <- data[order(data[[sort_field]]), ]
    }
  }
  
  # If there is only one path...
  if (is.null(id_field)) {
    
    lines <- SpatialLines(list(Lines(list(Line(data)), "id")))
    
    return(lines)
    
    # Now, if we have multiple lines...
  } else if (!is.null(id_field)) {  
    
    # Split into a list by ID field
    paths <- sp::split(data, data[[id_field]])
    
    sp_lines <- SpatialLines(list(Lines(list(Line(paths[[1]])), "line1")))
    
    # I like for loops, what can I say...
    for (p in 2:length(paths)) {
      id <- paste0("line", as.character(p))
      l <- SpatialLines(list(Lines(list(Line(paths[[p]])), id)))
      sp_lines <- spRbind(sp_lines, l)
    }
    
    return(sp_lines)
  }
}
# This function requires a data.table, a field with the difference
# in time between two consecutive points and a threshold to split the 
# tracks. The datatable has to be sorted by time.
# It will create additional field with track number and trackpoint number.
numberTracks <- function(DT,dt,cutStr) {
  # DT[, dtime := diff(utc),]
  DT[, trd := ifelse(dt > as.integer(cutStr), 1, 0), ]
  lenTr <- DT[trd > 0, which = T]
  if (length(lenTr) != 0) {
    difflenTr <- diff(lenTr)
    lenTr <- c(lenTr[1], difflenTr)
    trKs <-
      sapply(1:length(lenTr), function(i) {
        return(seq(1:lenTr[i]))
      })
    lenTrKs <- length(unlist(trKs))
    lenDT <- nrow(DT)
    if (lenTrKs == lenDT) {
      DT[1:lenTrKs, trSeq := unlist(trKs),]
      DT[trSeq == 1, trID := .I, ]
      DT[, trID := na.locf(trID), ]
    } else {
      DT[1:lenTrKs, trSeq := unlist(trKs), ]
      DT[(lenTrKs + 1):lenDT,
         trSeq := seq(1:length((lenTrKs + 1):lenDT)), ]
      DT[trSeq == 1, trID := .I, ]
      DT[, trID := na.locf(trID), ]
    }
  } else {
    DT[, trSeq := seq(1:nrow(DT)), ]
    DT[, trID := 1, ]
    DT$trd <- NULL
    gc()
  }
  DT[, mmsi.trID := paste0(mmsi, '|', trID), ]
  DTTrkslngt <- DT[, nrow(.SD), by = mmsi.trID]
  DTTrkslngt1 <- DTTrkslngt[V1 == 1, ]
  DT <- DT[!mmsi.trID %in% DTTrkslngt1$mmsi.trID, ]
  DT$trd <- NULL
  DTTrkslngt <- NULL
  DTTrkslngt1 <- NULL
  lenTr <- NULL
  lenTrKs <- NULL
  gc()
  return(DT)
}

moreThan <- function(dt,thR){
  # return(dt > quantile(dt, c(0.01,0.99), na.rm = T)[2])
  return(dt > thR)
}
cutStr=3600
# clipping the points to the extent
makeTracks <- function(lrit09f,curStr) {
  # we will have to identify the tracks
  lrit09fT <- as.ltraj(xy = lrit09f[, c("lon", "lat")],
                       date = lrit09f$datetime,
                       id = lrit09f$mmsi)
  lrit09fT <- cutltraj(lrit09fT, paste0('moreThan(dt,',as.character(cutStr),')'), nextr = T)
  # lrit09fTDT <- as.data.table(ld(lrit09fT))
  lrit09fSpl <- ltraj2sldf(lrit09fT, byid = T)
  proj4string(lrit09fSpl) <- CRS("+proj=longlat +datum=WGS84")
  lrit09fSpl <- spTransform(lrit09fSpl, CRS("+init=epsg:3035"))
  setwd(paste0(dataV, 'lrit09/'))
  st_write(st_as_sf(lrit09fSpl),
           paste0("lrit09_", unique(lrit09f[, shiptype]), "_trks.shp"),
           delete_layer = T)
  SpLines <- SpatialLines(lrit09fSpl@lines)
  pspSl  <- as.psp(SpLines)
  # Pixellate with resolution of 1000 m
  px <- pixellate(pspSl, eps = 1000)
  # This can be converted to raster as desired
  rLength <- raster(px)
  gc()
  NAvalue(rLength) <- 0
  crs(rLength) <- CRS("+init=epsg:3035")
  setwd(paste0(outPath, 'raster/', 'lrit09'))
  writeRaster(
    rLength,
    filename = tolower(paste0('lrit09_', unique(lrit09f[, shiptype]), '.tif')),
    format = 'GTiff',
    NAflag = 0,
    overwrite = T
  )
}
