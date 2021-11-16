## Load libraries
library(bestNormalize)
library(caret)
library(sf)
library(plot3D)
library(zoo)
library(tidyr)
library(data.table)
library(dplyr)
library(lubridate)

##Load data
DF_mosquito_capture <- read.csv("Biocomplexity_MosquitoCaptures/BiocomplexityMosquitoCaptures.csv")
head(DF_mosquito_capture,2)

names(DF_mosquito_capture)
DF_mosquito_capture$Date <- as.Date(DF_mosquito_capture$Date, tryFormats="%m/%d/%Y")
head(DF_mosquito_capture)
DF_mosquito_capture$combined_loc <- paste0(DF_mosquito_capture$Location,"_T", DF_mosquito_capture$Transect, "_S",DF_mosquito_capture$Station)
head(DF_mosquito_capture)

#first merge coordinate and climate station info
station_info_DF <- read.csv("biocomplexity_stns.csv")
head(station_info_DF)
#names(station_info_DF)
#df_projection=CRS("+proj=utm +zone=4 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
spatial_station_info_DF <-st_as_sf(station_info_DF, coords = c( "X_COORD", "Y_COORD"), crs = "+proj=utm +zone=5 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
spatial_station_info_DF <- st_transform(spatial_station_info_DF, crs="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
wgs84_coords <- as.data.frame(st_coordinates(spatial_station_info_DF))
names(wgs84_coords) <- c("lon_wgs84", "lat_wgs84")
station_info_DF <- cbind(station_info_DF, wgs84_coords)
head(station_info_DF)
#View(station_info_DF)
station_info_DF$combined_loc <- paste0(station_info_DF$site,"_T", station_info_DF$TRANSECT, "_S",station_info_DF$STATION)
station_info_DF_to_merge <- station_info_DF[, c("combined_loc","clim_station", "lon_wgs84", "lat_wgs84")]

all_data <- merge(DF_mosquito_capture, station_info_DF_to_merge, by="combined_loc")
head(all_data,10)  
dim(all_data)
dput(names(station_info_DF))


########################
#process climate data
########################
cooper_clim_df<- read.csv("BiocomplexityClimateData/20060410_Mrg_scr.213.csv")
solomon_clim_df<- read.csv("BiocomplexityClimateData/20070516_Mrg_scr.217.csv")
mamalaki_clim_df<- read.csv("BiocomplexityClimateData/20070323_Mrg_scr.211.csv")

head(cooper_clim_df)
head(solomon_clim_df)
head(mamalaki_clim_df)

cooper_clim_df[cooper_clim_df==-9999]=NA
solomon_clim_df[solomon_clim_df==-9999]=NA
mamalaki_clim_df[mamalaki_clim_df==-9999]=NA


head(cooper_clim_df)
head(solomon_clim_df)
head(mamalaki_clim_df)

cooper_clim_df$yyyy.mm.dd.hh.mm <- as.POSIXct(cooper_clim_df$yyyy.mm.dd.hh.mm, format="%m/%d/%Y %H:%M")
solomon_clim_df$yyyy.mm.dd.hh.mm <- as.POSIXct(solomon_clim_df$yyyy.mm.dd.hh.mm, format="%m/%d/%Y %H:%M")
mamalaki_clim_df$yyyy.mm.dd.hh.mm <- as.POSIXct(mamalaki_clim_df$yyyy.mm.dd.hh.mm, format="%m/%d/%Y %H:%M")

names(cooper_clim_df)[1]="datetime"
names(solomon_clim_df)[1]="datetime"
names(mamalaki_clim_df)[1]="datetime"

head(cooper_clim_df)
head(solomon_clim_df)
head(mamalaki_clim_df)

#only use full days!
first_row <- min(grep(cooper_clim_df$datetime, pattern = "01:00:00"))
last_row <- max(grep(cooper_clim_df$datetime, pattern = "00:00:00"))
cooper_clim_df <- cooper_clim_df[c(first_row:last_row),]

first_row <- min(grep(as.factor(solomon_clim_df$datetime), pattern = "01:00:00"))
last_row <- max(grep(solomon_clim_df$datetime, pattern = "00:00:00"))
solomon_clim_df <- solomon_clim_df[c(first_row:last_row),]

first_row <- min(grep(mamalaki_clim_df$datetime, pattern = "01:00:00"))
last_row <- max(grep(mamalaki_clim_df$datetime, pattern = "00:00:00"))
mamalaki_clim_df <- mamalaki_clim_df[c(first_row:last_row),]

head(cooper_clim_df)
head(solomon_clim_df)
head(mamalaki_clim_df)


cooper_clim_dt <- tidyr::separate(cooper_clim_df, datetime, c("date", "time"), sep = " ")
solomon_clim_dt <- tidyr::separate(solomon_clim_df, datetime, c("date", "time"), sep = " ")
mamalaki_clim_dt <- tidyr::separate(mamalaki_clim_df, datetime, c("date", "time"), sep = " ")


## split date in year, month, day, week.
cooper_climsep <- cooper_clim_dt %>% 
  dplyr::mutate(year = lubridate::year(date),
                month = lubridate::month(date),
                day = lubridate::day(date),
                week(date))


solomon_climsep <- solomon_clim_dt %>% 
  dplyr::mutate(year = lubridate::year(date),
                month = lubridate::month(date),
                day = lubridate::day(date),
                week(date))

mamalaki_climsep <- mamalaki_clim_dt %>% 
  dplyr::mutate(year = lubridate::year(date),
                month = lubridate::month(date),
                day = lubridate::day(date),
                week(date))


## built a function to remove NA values 
delete.na <- function(DF, n=0) {
  DF[rowSums(is.na(DF)) <= n,]
}

##Removed rows when more than 5 columns (covariates) have NA values 
cooper_climna <- delete.na(cooper_climsep,5)

solomon_climna <- delete.na(solomon_climsep,5)

mamalaki_climna <- delete.na(mamalaki_climsep,5)


## agregate data by Day

cooper_climdata <- cooper_climna %>% 
  group_by(day, month, year) %>%                            
  summarise(tcmean = mean(TC, na.rm = T), tamean = mean(Ta, na.rm = T), rhmean = mean(RH, na.rm = T), wsmean = mean(WS, na.rm = T), 
            wdmean = mean(WD, na.rm = T), rfmean = mean(RF, na.rm = T), rfsum = sum(RF, na.rm = T), tcmin = min(TC, na.rm = T), 
            tamin = min(Ta, na.rm = T), rhmin = min(RH, na.rm = T), wsmin = min(WS, na.rm = T), wdmin = min(WD, na.rm = T),
            rfmin = min(RF, na.rm = T), tcmax = max(TC, na.rm = T), tamax = max(Ta, na.rm = T), rhmax = max(RH, na.rm = T), wsmax = max(WS, na.rm = T), 
            wdmax = max(WD, na.rm = T), rfmax = max(RF, na.rm = T))


solomon_climdata <- solomon_climna %>% 
  group_by(day, month, year) %>%                            
  summarise(tcmean = mean(TC, na.rm = T), tamean = mean(Ta, na.rm = T), rhmean = mean(RH, na.rm = T), wsmean = mean(WS, na.rm = T), 
            wdmean = mean(WD, na.rm = T), rfmean = mean(RF, na.rm = T), rfsum = sum(RF, na.rm = T), tcmin = min(TC, na.rm = T), 
            tamin = min(Ta, na.rm = T), rhmin = min(RH, na.rm = T), wsmin = min(WS, na.rm = T), wdmin = min(WD, na.rm = T),
            rfmin = min(RF, na.rm = T), tcmax = max(TC, na.rm = T), tamax = max(Ta, na.rm = T), rhmax = max(RH, na.rm = T), wsmax = max(WS, na.rm = T), 
            wdmax = max(WD, na.rm = T), rfmax = max(RF, na.rm = T))

mamalaki_climdata <- mamalaki_climna %>% 
  group_by(day, month, year) %>%                            
  summarise(tcmean = mean(TC, na.rm = T), tamean = mean(Ta, na.rm = T), rhmean = mean(RH, na.rm = T), wsmean = mean(WS, na.rm = T), 
            wdmean = mean(WD, na.rm = T), rfmean = mean(RF, na.rm = T), rfsum = sum(RF, na.rm = T), tcmin = min(TC, na.rm = T), 
            tamin = min(Ta, na.rm = T), rhmin = min(RH, na.rm = T), wsmin = min(WS, na.rm = T), wdmin = min(WD, na.rm = T),
            rfmin = min(RF, na.rm = T), tcmax = max(TC, na.rm = T), tamax = max(Ta, na.rm = T), rhmax = max(RH, na.rm = T), wsmax = max(WS, na.rm = T), 
            wdmax = max(WD, na.rm = T), rfmax = max(RF, na.rm = T))

## save the files for future analysis

write.csv(cooper_climdata,"dataR\\cooper_clim_day.csv", 
          row.names = FALSE)
write.csv(solomon_climdata,"dataR\\solomon_clim_day.csv", 
          row.names = FALSE)
write.csv(mamalaki_climdata,"dataR\\mamalaki_clim_day.csv", 
          row.names = FALSE)

## Agregate data by week

cooper_climdatawk <- cooper_climna %>% 
  group_by(week(date), month, year) %>%                            
  summarise(tcmean = mean(TC, na.rm = T), tamean = mean(Ta, na.rm = T), rhmean = mean(RH, na.rm = T), wsmean = mean(WS, na.rm = T), 
            wdmean = mean(WD, na.rm = T), rfmean = mean(RF, na.rm = T), rfsum = sum(RF, na.rm = T), tcmin = min(TC, na.rm = T), 
            tamin = min(Ta, na.rm = T), rhmin = min(RH, na.rm = T), wsmin = min(WS, na.rm = T), wdmin = min(WD, na.rm = T),
            rfmin = min(RF, na.rm = T), tcmax = max(TC, na.rm = T), tamax = max(Ta, na.rm = T), rhmax = max(RH, na.rm = T), wsmax = max(WS, na.rm = T), 
            wdmax = max(WD, na.rm = T), rfmax = max(RF, na.rm = T))


solomon_climdatawk <- solomon_climna %>% 
  group_by(week(date), month, year) %>%                            
  summarise(tcmean = mean(TC, na.rm = T), tamean = mean(Ta, na.rm = T), rhmean = mean(RH, na.rm = T), wsmean = mean(WS, na.rm = T), 
            wdmean = mean(WD, na.rm = T), rfmean = mean(RF, na.rm = T), rfsum = sum(RF, na.rm = T), tcmin = min(TC, na.rm = T), 
            tamin = min(Ta, na.rm = T), rhmin = min(RH, na.rm = T), wsmin = min(WS, na.rm = T), wdmin = min(WD, na.rm = T),
            rfmin = min(RF, na.rm = T), tcmax = max(TC, na.rm = T), tamax = max(Ta, na.rm = T), rhmax = max(RH, na.rm = T), wsmax = max(WS, na.rm = T), 
            wdmax = max(WD, na.rm = T), rfmax = max(RF, na.rm = T))

mamalaki_climdatawk <- mamalaki_climna %>% 
  group_by(week(date), month, year) %>%                            
  summarise(tcmean = mean(TC, na.rm = T), tamean = mean(Ta, na.rm = T), rhmean = mean(RH, na.rm = T), wsmean = mean(WS, na.rm = T), 
            wdmean = mean(WD, na.rm = T), rfmean = mean(RF, na.rm = T), rfsum = sum(RF, na.rm = T), tcmin = min(TC, na.rm = T), 
            tamin = min(Ta, na.rm = T), rhmin = min(RH, na.rm = T), wsmin = min(WS, na.rm = T), wdmin = min(WD, na.rm = T),
            rfmin = min(RF, na.rm = T), tcmax = max(TC, na.rm = T), tamax = max(Ta, na.rm = T), rhmax = max(RH, na.rm = T), wsmax = max(WS, na.rm = T), 
            wdmax = max(WD, na.rm = T), rfmax = max(RF, na.rm = T))




write.csv(cooper_climdatawk,"dataR\\cooper_clim_week.csv", 
          row.names = FALSE)
write.csv(solomon_climdatawk,"dataR\\solomon_clim_week.csv", 
          row.names = FALSE)
write.csv(mamalaki_climdatawk,"dataR\\mamalaki_clim_week.csv", 
          row.names = FALSE)


