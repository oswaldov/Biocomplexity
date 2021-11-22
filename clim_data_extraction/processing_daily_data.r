#config params
wd="E:/Tmax_All_Islands/" #where are the original rasters?
setwd(wd)
output_dir="E:/Tmax_state/" #where to save rasters?
overwrite_results=F #redo process/ overwrite rasters?

#under the hood
all_images=list.files(wd, pattern = ".asc$")
dir.create(output_dir, showWarnings = F)
library(raster)
library(tools)
#img_filename = all_images[10]

add_projections_save_as_geotif=function(img_filename){
  output_filename=paste0(output_dir, file_path_sans_ext(basename(img_filename)), "_int.tif")
  if (file.exists(output_filename) & overwrite_results==F){
    cat("already done with ",img_filename, "\n")
  }else{
    cat("doing ",img_filename, "\n")
    img=raster(img_filename)
    projection(img)="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
    img_int=round(img*100)
    writeRaster(img_int, filename = output_filename, compress="LZW", overwrite=T, format="GTiff", datatype="INT2S")
  }
}  

#parallelizing script
#split rasters into lists to be farmed out to cores
n_cores = 8 #number of cores to try use 
all_raster_list=split(all_images, sort(c(1:length(all_images))%%n))
raster_list=all_raster_list[[3]]
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
  worker=Sys.getpid()
  file_nm=paste0(output_dir,"log_",format(Sys.time(), "%a %b %d %H%M%S"),"_worker",worker, ".txt")
  con=file(file_nm, open="wt")
  sink(con)
  cat('\n', 'Started on ', date(), '\n') 
  ptm0 <- proc.time()
  
  #cat("will do rasters: \n")
  #print(raster_list)

  img_filename = raster_list[1]
  for (img_filename in raster_list){
    add_projections_save_as_geotif(img_filename)
  }
  
  #end code
  ptm1=proc.time() - ptm0
  jnk=as.numeric(ptm1[3])
  cat('\n','It took ', jnk, "seconds to process rasters")
  
  on.exit(sink.reset())
  on.exit(close(con), add=T)
}

library(snowfall)
cpucores=min(c(n_cores, as.integer(Sys.getenv('NUMBER_OF_PROCESSORS')))) 
sfInit(parallel=TRUE, cpus=cpucores) # 
sfExportAll()
sfLapply(x=all_raster_list, fun=sp_parallel_run)

sfRemoveAll()
sfStop()
