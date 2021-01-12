library(nhdplusTools)
library(dataRetrieval)
library(lubridate)
library(rgdal) #Geospatial library for open source spatial analyses
library(rgeos) #Database of projeciton transformations
library(raster) #Library for importing and analyzing rasters
library(tidyverse) #Metalibrary that pulls in key data manipulation libraries
library(reshape2) #Data manipulation library
library(leaflet) #Data visualization of spatial data
library(lubridate) #Easy way to assign time stamps
library(dataRetrieval) #Package that direclty taps into the USGS water database
library(xts) #Time series analysis package
library(reshape2)
library(sf)

# first read in data from storet for epa sites A68 and A72
# A68 is just US of spill
# A72 is just DS of spill
chem_us <- read.csv("C:/Users/gubbi/Dropbox/animas_spill/Animas_Spill/data/a68_storet/result.csv") %>%
              filter(ActivityMediaName == "Water") %>%
              mutate(Datetime = ymd_hms(paste(ActivityStartDate, ActivityStartTime.Time), tz = "MST")) %>%
              mutate(ActivityStartDate = ymd(ActivityStartDate)) %>%
              mutate(Units = ResultMeasure.MeasureUnitCode)
              

chem_ds <- read.csv("C:/Users/gubbi/Dropbox/animas_spill/Animas_Spill/data/a72_storet/result.csv") %>%
  filter(ActivityMediaName == "Water") %>%
  mutate(Datetime = ymd_hms(paste(ActivityStartDate, ActivityStartTime.Time), tz = "MST")) %>%
  mutate(ActivityStartDate = ymd(ActivityStartDate)) %>%
  mutate(Units = ResultMeasure.MeasureUnitCode)

# read in nwis q data to match
q_us <- readNWISdv(siteNumbers = "09358550", 
                   parameterCd = "00060", 
                   startDate = min(chem_us$ActivityStartDate), 
                   endDate = max(chem_us$ActivityStartDate))

q_ds <- readNWISdv(siteNumbers = "09359020", 
                   parameterCd = "00060", 
                   startDate = min(chem_ds$ActivityStartDate), 
                   endDate = max(chem_ds$ActivityStartDate))

# save data
save(chem_ds, chem_us, q_ds, q_us, file='data/UnmergedData.RData')

# Move on to cleaning
# Subset data by a few key characteristics and convert all data to mg/l 
# Note that pH has no units and SC is in uS/cm
key.params <- c('Arsenic', 'Turbidity', 'Dissolved oxygen (DO)', 'Mercury', 'Lead', 'Aluminum') 

chem_us_sub <- chem_us %>%
  filter(CharacteristicName %in% key.params) %>%
  mutate(Units=ifelse(grepl('mg/l',Units),'mg/l',Units)) %>%
  mutate(Units=ifelse(grepl('ug/l',Units),'ug/l',Units)) %>%
  mutate(ResultMeasureValue=ifelse(Units=='ug/l',ResultMeasureValue/1000,ResultMeasureValue)) %>%
  mutate(Units=gsub('ug/l','mg/l',Units)) %>%
  mutate(Units=ifelse(grepl('None',Units),'None',Units)) %>% 
  mutate(Analyte = CharacteristicName)

chem_ds_sub <- chem_ds %>%
  filter(CharacteristicName %in% key.params) %>%
  mutate(Units=ifelse(grepl('mg/l',Units),'mg/l',Units)) %>%
  mutate(Units=ifelse(grepl('ug/l',Units),'ug/l',Units)) %>%
  mutate(ResultMeasureValue=ifelse(Units=='ug/l',ResultMeasureValue/1000,ResultMeasureValue)) %>%
  mutate(Units=gsub('ug/l','mg/l',Units)) %>%
  mutate(Units=ifelse(grepl('None',Units),'None',Units)) %>% 
  mutate(Analyte = CharacteristicName)

# reshape to make easier to work with
# cast data then melt to long
chem_us_cast <- dcast(chem_us_sub,Datetime+MonitoringLocationIdentifier ~ Analyte, value.var='ResultMeasureValue',mean)
chem_ds_cast <- dcast(chem_ds_sub,Datetime+MonitoringLocationIdentifier ~ Analyte, value.var='ResultMeasureValue',mean)

chem_us_melt <- melt(chem_us_cast,id.vars=c('Datetime','MonitoringLocationIdentifier'),measure.vars=key.params) %>% 
                  filter(!is.na(value)) %>%
                  mutate(Date = as.Date(Datetime))

chem_ds_melt <- melt(chem_ds_cast,id.vars=c('Datetime','MonitoringLocationIdentifier'),measure.vars=key.params) %>% 
                  filter(!is.na(value)) %>%
                  mutate(Date = as.Date(Datetime))

# join chem data to full q datasets
# first have to convert posix to date object
q_us$Date <- as.Date(q_us$Date)
q_chem_us <- left_join(q_us,chem_us_melt, by= 'Date')

q_ds$Date <- as.Date(q_ds$Date)
q_chem_ds <- left_join(q_ds,chem_ds_melt, by= 'Date')

# next up, get spatial data from nhdplus
nldi_nwis_us <- list(featureSource = "nwissite", featureID = "USGS-09358550")
nldi_nwis_ds <- list(featureSource = "nwissite", featureID = "USGS-09359020")

nhd_id_us <- discover_nhdplus_id(nldi_feature = nldi_nwis_us)
nhd_id_ds <- discover_nhdplus_id(nldi_feature = nldi_nwis_ds)

flowline_us <- navigate_nldi(list(featureSource = "comid", featureID = nhd_id_us))
subset_file_us <- tempfile(fileext = ".gpkg")
subset_us <- subset_nhdplus(comids = flowline_us$nhdplus_comid,
                         output_file = subset_file_us,
                         nhdplus_data = "download", 
                         flowline_only = FALSE,
                         return_data = TRUE)
catchment_us <- subset_us$CatchmentSP

flowline_ds <- navigate_nldi(list(featureSource = "comid", featureID = nhd_id_ds))
subset_file_ds <- tempfile(fileext = ".gpkg")
subset_ds <- subset_nhdplus(comids = flowline_ds$nhdplus_comid,
                            output_file = subset_file_ds,
                            nhdplus_data = "download", 
                            flowline_only = FALSE,
                            return_data = TRUE)
catchment_ds <- subset_ds$CatchmentSP %>%
                  filter(areasqkm == max(areasqkm))

# set colors and names for spatial data
catchment_us$color <- 'blue'
catchment_us$id <- 'Cement Creek Watershed'

catchment_ds$color <- 'red'
catchment_ds$id <- 'Animas River Watershed'

# get station spatial data together
usgs_id <- c(attr(q_chem_us, "siteInfo")$site_no[1], attr(q_chem_ds, "siteInfo")$site_no[1])
prettyname <- c("Cement Creek", "Animas River")
lon <- c(attr(q_chem_us, "siteInfo")$dec_lon_va[1], attr(q_chem_ds, "siteInfo")$dec_lon_va[1])
lat <- c(attr(q_chem_us, "siteInfo")$dec_lat_va[1], attr(q_chem_ds, "siteInfo")$dec_lat_va[1])
color <- c("blue", "red")

stations <- tibble(usgs_id, prettyname, color, lon,lat) %>%
              st_as_sf(., coords = c("lon", "lat"), crs = 4326)

# get spill location

name <- "King Mine Spill Site"
lon <- -107.638333
lat <- 37.894444

spill <- tibble(name, lon, lat) %>%
            st_as_sf(., coords = c("lon", "lat"), crs = 4326)

# Name discharge column by units (cubic feet per second)
names(q_chem_us)[4] <- 'cfs'
names(q_chem_ds)[4] <- 'cfs'

# Convert to lps and m3s
q_chem_us$Lps <- q_chem_us$cfs*28.3168
q_chem_us$m3s <- q_chem_us$Lps/1000
q_chem_us$variable <- as.character(q_chem_us$variable)

q_chem_ds$Lps <- q_chem_ds$cfs*28.3168
q_chem_ds$m3s <- q_chem_ds$Lps/1000
q_chem_ds$variable <- as.character(q_chem_ds$variable)

# create single q chem dataset at the end instead of the beginning because you are trying to reuse matt's code
q_chem_us$prettyname <- "Cement Creek"
q_chem_ds$prettyname <- "Animas River"
q_chem <- rbind(q_chem_ds, q_chem_us)


# create q time series object
q_us <- tibble(q_chem_us$Date, q_chem_us$m3s)
names(q_us) <- c("Date", "Cement Creek")
q_ds <- tibble(q_chem_ds$Date, q_chem_ds$m3s)
names(q_ds) <- c("Date", "Animas River")

q <- unique(right_join(q_us, q_ds, by = "Date"))
q_xts <- as.xts(q[-1], order.by=q$Date)


# save data
save(q_xts, q_chem, q_chem_ds, q_chem_us, catchment_ds, catchment_us, stations, spill, file='data/ShinyFullDataset.RData')

