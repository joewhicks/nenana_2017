#-------------------------------------------------------------
# title: Nenana ice classic 2017
# about: data aggregation for exploration. early stages.
# author: Joe Hicks
#-------------------------------------------------------------

setwd("/Users/jwhicks/nenana/nenana_2017/")

library(dplyr)
library(ggplot2)
library(data.table)
library(lubridate)
library(readr)
library(reshape2)
library(stringr)


# set params
run_parse=TRUE

# set up api key (from email)
noaakey <- "vTByNQGaRrHhbkZlgDSPSJXOqzNnvtpE"

# import raw GHCND site meta data
GHCND_raw <- read_lines(file="/Users/jwhicks/nenana/nenana_2017/ghcnd-stations.txt")

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
#write.csv(GHCND_sites, file="/Users/jwhicks/nenana/nenana_2017/GHCND_sites.csv", row.names = NULL)
#if(run_parse==FALSE){ GHCND_sites <- read_csv("~/nenana/nenana_2017/GHCND_sites.csv") %>% data.table }

# create Alaska specific sites
GHCND_sites_AK <- GHCND_sites[state=="AK"]
GHCND_sites_AK <- GHCND_sites_AK[lat >= 60 & lat <= 70 & lon < -155]

# Get a list of datasets and data availible
# setup the vector of site ids
sta <- GHCND_sites_AK$site_id
for(i in 1:nrow(GHCND_sites_AK)){
#for(i in 1:20){
  input <- paste0("ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/all/",sta[i],".dly")

  output <- read.fwf(input, n = -1,
                     widths = c(11, 4, 2, 4, rep(c(5, 1, 1, 1),31)), 
                     col.names = c("ID", "YEAR", "MONTH", "ELEMENT", 
                                   "VALUE1", "MFLAG1", "QFLAG1", "SFLAG1",
                                   "VALUE2", "MFLAG2", "QFLAG2", "SFLAG2",
                                   "VALUE3", "MFLAG3", "QFLAG3", "SFLAG3",
                                   "VALUE4", "MFLAG4", "QFLAG4", "SFLAG4",
                                   "VALUE5", "MFLAG5", "QFLAG5", "SFLAG5",
                                   "VALUE6", "MFLAG6", "QFLAG6", "SFLAG6",
                                   "VALUE7", "MFLAG7", "QFLAG7", "SFLAG7",
                                   "VALUE8", "MFLAG8", "QFLAG8", "SFLAG8",
                                   "VALUE9", "MFLAG9", "QFLAG9", "SFLAG9",
                                   "VALUE10", "MFLAG10", "QFLAG10", "SFLAG10",
                                   "VALUE11", "MFLAG11", "QFLAG11", "SFLAG11",
                                   "VALUE12", "MFLAG12", "QFLAG12", "SFLAG12",
                                   "VALUE13", "MFLAG13", "QFLAG13", "SFLAG13",
                                   "VALUE14", "MFLAG14", "QFLAG14", "SFLAG14",
                                   "VALUE15", "MFLAG15", "QFLAG15", "SFLAG15",
                                   "VALUE16", "MFLAG16", "QFLAG16", "SFLAG16",
                                   "VALUE17", "MFLAG17", "QFLAG17", "SFLAG17",
                                   "VALUE18", "MFLAG18", "QFLAG18", "SFLAG18",
                                   "VALUE19", "MFLAG19", "QFLAG19", "SFLAG19",
                                   "VALUE20", "MFLAG20", "QFLAG20", "SFLAG20",
                                   "VALUE21", "MFLAG21", "QFLAG21", "SFLAG21",
                                   "VALUE22", "MFLAG22", "QFLAG22", "SFLAG22",
                                   "VALUE23", "MFLAG23", "QFLAG23", "SFLAG23",
                                   "VALUE24", "MFLAG24", "QFLAG24", "SFLAG24",
                                   "VALUE25", "MFLAG25", "QFLAG25", "SFLAG25",
                                   "VALUE26", "MFLAG26", "QFLAG26", "SFLAG26",
                                   "VALUE27", "MFLAG27", "QFLAG27", "SFLAG27",
                                   "VALUE28", "MFLAG28", "QFLAG28", "SFLAG28",
                                   "VALUE29", "MFLAG29", "QFLAG29", "SFLAG29",
                                   "VALUE30", "MFLAG30", "QFLAG30", "SFLAG30",
                                   "VALUE31", "MFLAG31", "QFLAG31", "SFLAG31")
                     ) 
  if(i==1){ AK_site_data <- output } else{ AK_site_data <- rbind(AK_site_data,output) }
  print(paste0(i,"/832"))
}

temp_1 <- AK_site_data %>% select(ID, YEAR, MONTH, ELEMENT)
temp_2 <- AK_site_data[,grepl("VALUE",names(AK_site_data))]
AK_data <- cbind(temp_1,temp_2)

AK_data <- data.table(AK_data)
AK_data <- melt(AK_data, id.vars = c("ID","ELEMENT"))
AK_data <- AK_data[value != -9999]

temp <- AK_data[variable == "YEAR" & value == 2015]
