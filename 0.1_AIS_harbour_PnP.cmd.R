library(data.table)
library(parallel)
library(sf)
library(lubridate)
# BoilerPlate
gc()
#- Clear workspace
rm(list=ls())
# Settings paths
# setwd('~/work/north.sea/')
gc()
# Loding the ports
load('harbours.rda')
harbours          <- st_as_sf(harbours,coords = c('lon','lat'))
row.names(harbours) <- NULL
harbours$harbour <- tolower(iconv(harbours$harbour, "UTF-8", "UTF-8",sub='_'))
harbours001      <- st_buffer(harbours,0.01)
harbours         <- NULL

# List csv files
csvL <- list.files(path = '.',pattern ='output*')

# Function to apply to the single csv files
csvPnP <- function(csvFileName) {
  csvF <- fread(csvFileName)
  gc()
  names(csvF) <-
    c(
      "mmsi",
      "source",
      "locdate",
      "lon",
      "lat",
      "sog",
      "cog",
      "trueheading",
      "maneuverindicator",
      "navigationstatus",
      "rot",
      "positionaccuracy",
      'imono',
      'callsign',
      'shipname',
      "aisshiptype",
      "shiplength",
      "shipwidth",
      "flagisocode2",
      "utime"
    )
  # Point in polygon with ports ----
  # Now I will do the point in polygon with the points having sog == 0
  gc()
  csvF[, utc := ymd_hms(utime),]
  csvF[, utime := NULL, ]
  csvF <- csvF[, setorder(.SD, utc), by = mmsi]
  csvF[, id := paste0(mmsi, '/', .I), by = mmsi]
  gc()
  csvF.rest <- csvF
  csvF      <- csvF[, .(id, lon, lat)]
  chunk     <- ceiling(nrow(csvF) / (detectCores() - 1))
  nrowSplit <- nrow(csvF)
  repSplit  <- rep(1:(detectCores() - 1), each = chunk)[1:nrowSplit]
  ## Pre-split the data into m/n chunks
  dataSplit1b <- split(csvF, repSplit)
  repSplit    <- NULL 
  nrowSplit   <- NULL
  gc()
  # Function for the mc split and PnP ----
  pNp <- function(pts) {
    lbfleetMapsSog1          <- st_as_sf(pts, coords = c('lon', 'lat'))
    lbfleetMapsSog1          <-
      st_join(lbfleetMapsSog1, harbours001[, ('harbour')], join = st_intersects)
    lbfleetMapsSog1$geometry <- NULL
    lbfleetMapsSog1          <-
      as.data.table(lbfleetMapsSog1[, c("id", "harbour")])
    names(lbfleetMapsSog1)   <- tolower(names(lbfleetMapsSog1))
    lbfleetMapsSog1          <- as.data.table(lbfleetMapsSog1)
  }
  ## Approach 1b
  res1b <- mclapply(dataSplit1b, pNp, mc.cores = (detectCores() - 1))
  dataSplit1b <- NULL
  gc()
  out   <- rbindlist(res1b)
  res1b <- NULL
  out   <- out[!duplicated(id), ]
  gc()
  csvF <- merge(out, csvF.rest, by = 'id')
  out <- NULL
  csvF.rest <- NULL
  gc()
  # We could remove all the ports in harbour
  csvF.remove <- csvF[(!is.na(harbour) & sog == 0), ]
  csvF.ok     <- csvF[!id %in% csvF.remove$id, ]
  csvF       <- NULL
  csvF.remove <- NULL
  gc()
  fwrite(csvF.ok, paste0('clean/', csvFileName, '.clean.csv'))
  csvF.ok <- NULL
}
lapply(csvL,csvPnP)
