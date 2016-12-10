#-------------------------------------------------------------
# title: Nenana ice classic 2017
# about: data aggregation for exploration. early stages.
# author: Joe Hicks
#-------------------------------------------------------------

setwd("/Users/jwhicks/nenana/nenana_2017/")

library(rnoaa)
library(dplyr)
library(ggplot2)
library(data.table)
library(readr)

# set params
run_parse=FALSE

# set up api key (from email)
noaakey <- "vTByNQGaRrHhbkZlgDSPSJXOqzNnvtpE"

# import raw GHCND site meta data
GHCND_raw <- read_lines(file="~/Desktop/ghcnd-stations.txt")

# initialize variables, names from ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/readme.txt
site_id      <- c()
lat          <- c()
lon          <- c()
elev         <- c()
state        <- c()
name         <- c()
gsn_flag     <- c()
hcn_crn_flag <- c()
wmo_id       <- c()

# parse the text file
if(run_parse==TRUE){
  for(i in 1:length(GHCND_raw)){
    #for(i in 1:1000){
    site_id[i]      = substr(GHCND_raw[i], 1,11)  %>% str_trim()    #ID           1-11   Character
    lat[i]          = substr(GHCND_raw[i], 13,20) %>% as.numeric()  #LATITUDE     13-20   Real
    lon[i]          = substr(GHCND_raw[i], 22,30) %>% as.numeric()  #LONGITUDE    22-30   Real
    elev[i]         = substr(GHCND_raw[i], 32,37) %>% as.numeric()  #ELEVATION    32-37   Real
    state[i]        = substr(GHCND_raw[i], 39,40) %>% str_trim()    #STATE        39-40   Character
    name[i]         = substr(GHCND_raw[i], 42,71) %>% str_trim()    #NAME         42-71   Character
    gsn_flag[i]     = substr(GHCND_raw[i], 73,75) %>% str_trim()    #GSN FLAG     73-75   Character
    hcn_crn_flag[i] = substr(GHCND_raw[i], 77,79) %>% str_trim()    #HCN/CRN FLAG 77-79   Character
    wmo_id[i]       = substr(GHCND_raw[i], 81,85) %>% str_trim()    #WMO ID       81-85   Character
    
    print(paste0(i, "/84086"))
  }
}


# slam them all into a table and write to csv
GHCND_sites <- data.table(site_id, lat, lon, elev, state, name, gsn_flag, hcn_crn_flag, wmo_id)
write.csv(GHCND_sites, file="/Users/jwhicks/nenana/nenana_2017/GHCND_sites.csv", row.names = NULL)
if(run_parse==FALSE){ GHCND_sites <- read_csv("~/nenana/nenana_2017/GHCND_sites.csv") %>% data.table }

# create Alaska specific sites
GHCND_sites_AK <- GHCND_sites[state=="AK"]
GHCND_sites_ANCH <- GHCND_sites_AK[name %like% "ANCHORAGE"]

#Convert to format consumable by ncdc()
sites <- GHCND_sites_ANCH %>% select(site_id) 
ANCH_sites <- lapply(sites, function(x) paste0("GHCND:", x) )
  
out <- ncdc(
  datasetid='GHCND', 
  stationid= ANCH_sites$site_id, 
  startdate = "2015-01-01", 
  enddate = "2015-12-31", 
  limit=500,
  token = noaakey
)
#head(out$data)

ncdc_locs(locationcategoryid='CITY', sortfield='name', sortorder='desc', token = noaakey)





