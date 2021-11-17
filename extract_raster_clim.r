#this script simply extracts predictor data for the available time series gridded environmental data
#including monthly rainfall, and temperature
#the datasets are rather large, so it may be hard to share it with everyone, but am willing if anyone is interested

project_dirs=c("D:/projects/2020_hakalau_line/biocomplexity_mosquito_analysis/data/") #add your local paths to this vector
wd=project_dirs[min(which(dir.exists(project_dirs)))]
setwd(wd)
years=c(1999:2005)
months=c(1:12)

library(stringr)
library(rgdal)
library(terra)
months=str_pad(months, 2, "left", "0")


merged_spdf=readOGR("D:/projects/2020_hakalau_line/biocomplexity_mosquito_analysis/data/merged_plot_and_station_coordinates.gpkg", layer="site_coordinates")
merged_spdf=spTransform(merged_spdf, CRS(SRS_string = "EPSG:4326"))
merged_spdf_xy=terra::geom(merged_spdf)[,-1]

merged_spdf_coords=cbind(merged_spdf_xy, merged_spdf@data)
write.csv(merged_spdf_coords, "plot_and_station_coords.csv", row.names = F)

month_years=apply(expand.grid(years, months), 1, paste, collapse="_") #paste0(years, months)

#############################
#do precip
clim_directory="D:/data/climate_data/2019_UH_clim_data/P_obs_month_1920_2012_mm/"

#allocate output df to memory
ppt_extracted_val_df=data.frame(matrix(NA, nrow = nrow(merged_spdf_xy), ncol = length(month_years)))
names(ppt_extracted_val_df)=month_years
#View(ppt_extracted_val_df)

year="2001"
month="10"
for (year in years){
  for (month in months){
    cat("doing ", year, " ", month, "\n")
    month_ppt_raster=rast(paste0(clim_directory, year, "_", month, "_int.tif"))
    extracted_vector=terra::extract(month_ppt_raster, merged_spdf_xy)
    ppt_extracted_val_df[,paste0(year, "_", month)]=extracted_vector
  }
}

names(ppt_extracted_val_df)=paste0("ppt_", names(ppt_extracted_val_df))
merged_spdf@data=cbind(merged_spdf@data, ppt_extracted_val_df)
#View(merged_spdf@data)

######################################
#now temperature
#tmax
tmax_clim_directory="D:/data/climate_data/2019_UH_clim_data/Tmax_obs_daily_state_1990_2014/"
tmax_extracted_val_df=data.frame(matrix(NA, nrow = nrow(merged_spdf_xy), ncol = length(month_years))) #allocate output df to memory
names(tmax_extracted_val_df)=month_years

year="2001"
month="10"
for (year in years){
  for (month in months){
    cat("doing ", year, " ", month, "\n")
    raster_files=list.files(path = tmax_clim_directory, pattern = paste0("All_Islands_", year, "_", month), full.names = T)
    raster_files=grep(pattern = ".tif$", raster_files, value = T)
    month_clim_stack=rast(raster_files)
    extracted_vector=terra::extract(month_clim_stack, merged_spdf_xy)
    extracted_vector=apply(extracted_vector, 1, max, na.rm=T)/100
    tmax_extracted_val_df[,paste0(year, "_", month)]=extracted_vector
  }
}

names(tmax_extracted_val_df)=paste0("tmax_", names(tmax_extracted_val_df))
merged_spdf@data=cbind(merged_spdf@data, tmax_extracted_val_df)


######################################
#tmin
tmin_clim_directory="D:/data/climate_data/2019_UH_clim_data/Tmin_obs_daily_state_1990_2014/"
tmin_extracted_val_df=data.frame(matrix(NA, nrow = nrow(merged_spdf_xy), ncol = length(month_years))) #allocate output df to memory
names(tmin_extracted_val_df)=month_years

year="2001"
month="10"
for (year in years){
  for (month in months){
    cat("doing ", year, " ", month, "\n")
    raster_files=list.files(path = tmin_clim_directory, pattern = paste0("All_Islands_", year, "_", month), full.names = T)
    raster_files=grep(pattern = ".tif$", raster_files, value = T)
    month_clim_stack=rast(raster_files)
    extracted_vector=terra::extract(month_clim_stack, merged_spdf_xy)
    extracted_vector=apply(extracted_vector, 1, min, na.rm=T)/100
    tmin_extracted_val_df[,paste0(year, "_", month)]=extracted_vector
  }
}

names(tmin_extracted_val_df)=paste0("tmin_", names(tmin_extracted_val_df))
merged_spdf@data=cbind(merged_spdf@data, tmin_extracted_val_df)

names(merged_spdf@data)
View(merged_spdf@data)

site_GIS_data_file=paste0("merged_plot_and_station_raster_clim_info.gpkg") #paste0(wd, shp_outdir, sp_name, img_name, ".gpkg")
if (file.exists(site_GIS_data_file)){file.remove(site_GIS_data_file)}
writeOGR(obj=merged_spdf, dsn=site_GIS_data_file, layer="climate_info", driver="GPKG")

write.csv(merged_spdf@data, "merged_plot_and_station_raster_clim_info.csv", row.names = F)

