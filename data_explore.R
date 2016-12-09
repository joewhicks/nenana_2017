#-------------------------------------------------------------
# title: Nenana ice classic 2017
# about: data aggregation for exploration. early stages.
# author: Joe Hicks
#-------------------------------------------------------------

library(rnoaa)
library(dplyr)
library(ggplot2)
library(data.table)
library(readr)

api_key <- "vTByNQGaRrHhbkZlgDSPSJXOqzNnvtpE"

GHCND_raw <- read_lines(file="~/Desktop/ghcnd-stations.txt")
site_id <- c()
lng     <- c()
lat     <- c()
misc    <- c()
state   <- c()
city    <- c()
misc2   <- c()
zip     <- c()

for(i in 1:length(GHCND_raw)){
#for(i in 1:10){
  site_id[i] = substr(GHCND_sites[i], 1,11)   %>% str_trim()
  lng[i]     = substr(GHCND_sites[i], 12,20)  %>% str_trim()
  lat[i]     = substr(GHCND_sites[i], 21,30)  %>% str_trim()
  misc[i]    = substr(GHCND_sites[i], 31,37)  %>% str_trim()
  state[i]   = substr(GHCND_sites[i], 38,40)  %>% str_trim()
  city[i]    = substr(GHCND_sites[i], 41,72)  %>% str_trim()
  misc2[i]   = substr(GHCND_sites[i], 72,75)  %>% str_trim()
  zip[i]     = substr(GHCND_sites[i], 76,86)  %>% str_trim()
  print(paste0(i, "/84086"))
}

GHCND_sites <- data.table(site_id, lng, lat, state, city, zip)

GHCND_sites_AK <- GHCND_sites[state=="AK"]

sites <- GHCND_sites_AK %>% select(site_id) %>% unlist %>% unname
sites <- paste0("GHCND:", sites)
out <- ncdc(
  datasetid='GHCND', 
#  datatypeid = 'PRCP', 
  stationid='GHCND:USW00026435', 
  startdate = "2013-09-03", 
  enddate = "2013-09-30", 
  limit=30,
  token = api_key
  )
head(out$data)
