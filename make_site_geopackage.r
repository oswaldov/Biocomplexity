#this code creates the geopackage with weather station coordinates and point coordinates merged

#Specify where project directory is by adding it to this vector
project_dirs=c("D:/projects/2020_hakalau_line/biocomplexity_mosquito_analysis/") #add your local paths to this vector
wd=project_dirs[min(which(dir.exists(project_dirs)))]
setwd(wd)

######################################
station_data_df=read.csv("biocomplexity_stns.csv")

#View(station_data_df)
#dput(names(station_data_df))

xy <- station_data_df[,c("X_COORD", "Y_COORD")]

library(sp)
spdf <- SpatialPointsDataFrame(coords = xy, data = station_data_df,
                               proj4string = CRS(SRS_string = "EPSG:26905")) #https://spatialreference.org/


library(rgdal)
site_GIS_data_file=paste0("site_coordinates.gpkg") #paste0(wd, shp_outdir, sp_name, img_name, ".gpkg")
if (file.exists(site_GIS_data_file)){file.remove(site_GIS_data_file)}

writeOGR(obj=spdf, dsn=site_GIS_data_file, layer="site_coordinates", driver="GPKG")

plot(spdf)

###############
#weather station
station_data_df=read.csv("biocomplexity_weather_stations.csv")

#View(station_data_df)
#dput(names(station_data_df))

xy <- station_data_df[,c("Longitude", "Latitude")]

library(sp)
spdf_weather_stn <- SpatialPointsDataFrame(coords = xy, data = station_data_df,
                               proj4string = CRS(SRS_string = "EPSG:4326")) #https://spatialreference.org/
library(raster)
spdf_weather_stn=spTransform(spdf_weather_stn, crs(spdf))

library(rgdal)
site_GIS_data_file=paste0("weather_station_coordinates.gpkg") #paste0(wd, shp_outdir, sp_name, img_name, ".gpkg")
if (file.exists(site_GIS_data_file)){file.remove(site_GIS_data_file)}


writeOGR(obj=spdf_weather_stn, dsn=site_GIS_data_file, layer="site_coordinates", driver="GPKG")

plot(spdf_weather_stn)

##########################
#combine
spdf@data$climate_station=NA
station_names=spdf_weather_stn@data$Name
station_info_df=read.csv("biocomplexity_weather_stations_info.csv")
spdf_weather_stn@data=station_info_df

merged_spdf=rbind(spdf, spdf_weather_stn)

site_GIS_data_file=paste0("merged_plot_and_station_coordinates.gpkg") #paste0(wd, shp_outdir, sp_name, img_name, ".gpkg")
if (file.exists(site_GIS_data_file)){file.remove(site_GIS_data_file)}
writeOGR(obj=merged_spdf, dsn=site_GIS_data_file, layer="site_coordinates", driver="GPKG")

