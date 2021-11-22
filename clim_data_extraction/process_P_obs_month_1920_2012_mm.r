wd="D:/data/climate_data/2019_frazier_data/P_obs_month_1920_2012_mm/" #location where all precip folders are located
#script will save all geotifs at this wd root directory
max_n_cores = 8 #number of cores to use in parallel processing (dont use all available, or else computer will be unusable)
# available cores found at as.integer(Sys.getenv('NUMBER_OF_PROCESSORS'))

################
#under the hood
setwd(wd)
library(raster)
library(stringr)
wd_subfolders=list.dirs(wd, recursive = T)
wd_subfolders=grep(wd_subfolders, pattern = "_mm$", value = T)
month_str=tolower(month.abb)

#split rasters into lists to be farmed out to cores
all_raster_list=split(wd_subfolders, sort(c(1:length(wd_subfolders))%%max_n_cores))

#wd_subfolder=wd_subfolders[200]
process_esri_grids=function(wd_subfolder){
  raster_name=basename(wd_subfolder)
  raster_name=sub(raster_name, pattern = "st", replacement = "")
  cat("doing ", raster_name, "\n")
  raster_period=substr(raster_name, start = 1, stop=3)
  if (raster_period=="ann"){
    raster_year=substr(raster_name, start = 5, stop=8)
    
  }else{
    raster_year=substr(raster_name, start = 4, stop=7)
  }
  
  if (raster_period =="ann"){
    raster_period="yr"
  }else{
    raster_period=str_pad(as.character(which(raster_period==month_str)), width = 2, side = "left", pad = "0") 
  }
  new_raster_name=paste0(raster_year, "_", raster_period)
  new_raster_name
  cat("saving as ", new_raster_name, "\n")
  
  #raster(wd_subfolder)
  img_filename=wd_subfolder
  cat("doing ",img_filename, "\n")
  img=raster(img_filename)
  #writeRaster(img, filename = paste0(file_path_sans_ext(basename(i)), ".tif)"), compress="LZW", overwrite=T, format="GTiff")
  #projection(img)="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  img_int=round(img)
  #plot(img_int)
  writeRaster(img_int, filename = paste0(new_raster_name, "_int.tif)"), compress="LZW", overwrite=T, format="GTiff", datatype="INT2S")
}

#stop sinks
sink.reset <- function(){
  for(i in seq_len(sink.number())){
    sink(NULL)
  }
}

raster_list=all_raster_list[[1]]
sp_parallel_run=function(raster_list){
  library(raster)
  library(tools)
  library(stringr)
  worker=Sys.getpid()
  file_nm=paste0(wd,"log_",format(Sys.time(), "%a %b %d %H%M%S"),"_worker",worker, ".txt")
  con=file(file_nm, open="wt")
  sink(con)
  cat('\n', 'Started on ', date(), '\n') 
  ptm0 <- proc.time()
  
  cat("will do rasters: \n")
  print(raster_list)
  
  img_filename = raster_list[1]
  for (img_filename in raster_list){
    process_esri_grids(img_filename)
  }
  
  #end code
  ptm1=proc.time() - ptm0
  jnk=as.numeric(ptm1[3])
  cat('\n','It took ', jnk, "seconds to process rasters")
  
  on.exit(sink.reset())
  on.exit(close(con), add=T)
}

library(snowfall)
cpucores=max_n_cores #min(c(2, as.integer(Sys.getenv('NUMBER_OF_PROCESSORS')))) 
sfInit(parallel=TRUE, cpus=cpucores) # 
sfExportAll()
sfLapply(x=all_raster_list, fun=sp_parallel_run)

sfRemoveAll()
sfStop()

