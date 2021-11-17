#this code simply merges the mosquito capture data with the site/point data coordinate information for
#subsequent candidate predictor data extraction 


#Specify where project directory is by adding it to this vector
#BTW we really need to standardize our folder structure to ensure our code is easily run across our machines
project_dirs=c("D:/projects/2020_hakalau_line/biocomplexity_mosquito_analysis/") 
wd=project_dirs[min(which(dir.exists(project_dirs)))]
setwd(wd)

################################
#load mosquito capture data
DF_mosquito_capture=read.csv("data/BiocomplexityMosquitoCaptures.csv")

names(DF_mosquito_capture)
DF_mosquito_capture$Date=as.Date(DF_mosquito_capture$Date, tryFormats="%m/%d/%Y")
DF_mosquito_capture$combined_loc=paste0(DF_mosquito_capture$Location,"_T", DF_mosquito_capture$Transect, "_S",DF_mosquito_capture$Station)

################################
#load coordinate and climate station info
library(sf)
station_info_DF=read.csv("data/biocomplexity_stns.csv")
#names(station_info_DF)
spatial_station_info_DF=st_as_sf(station_info_DF, coords = c( "X_COORD", "Y_COORD"), crs = "+proj=utm +zone=5 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
spatial_station_info_DF=st_transform(spatial_station_info_DF, crs="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
wgs84_coords=as.data.frame(st_coordinates(spatial_station_info_DF))
names(wgs84_coords)=c("lon_wgs84", "lat_wgs84")
station_info_DF=cbind(station_info_DF, wgs84_coords)
#View(station_info_DF)
station_info_DF$combined_loc=paste0(station_info_DF$site,"_T", station_info_DF$TRANSECT, "_S",station_info_DF$STATION)
station_info_DF_to_merge=station_info_DF[, c("combined_loc","clim_station", "lon_wgs84", "lat_wgs84")]

################################
#merge capture data with coordinate and climate station info
all_data=merge(DF_mosquito_capture, station_info_DF_to_merge, by="combined_loc")
View(all_data)  
dput(names(station_info_DF))

write.csv(all_data, "data/all_biocomplexity_data_merged_coords_only.csv", row.names = F)
save(all_data, file = "data/all_biocomplexity_data_merged_coords_only.Rdata")
