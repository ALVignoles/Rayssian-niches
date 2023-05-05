#' Clip all rasters that are in the same directory by the same extent.
#' 
#' @description auto_crop will crop multiple rasters with the desired extent and then save the output in a new folder 
#' named after the mask.
#' 
#' @param mask (SpatVect object) the mask that will be used for clipping the layers.
#' @param mask.name (character) the name of the mask.
#' @param raster.dir (character) the directory in which are the rasters to be clipped. 
#' 
#' @return An end message announcing that the script was succesfully run. In addition, output rasters will be saved in 
#' a directory named after the mask (format: GeoTiff files).
#' 
#' @author Anaïs L. Vignoles.
#' Last modified: 29/04/2023.

auto_crop <- function(mask, mask.name, raster.dir) {
  cat(paste("\n processing",  mask.name, "data... \n", 
            "\n directory:", raster.dir, "\n"))
  
  require(terra)
  
  # first, importing raster layers
  env <- list.files(raster.dir, pattern = "*.asc$")
  
  env.raster <- terra::rast(paste0(raster.dir, "/", env))
  terra::crs(env.raster) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  
  nam <- strsplit(names(env.raster), "_")
  names <- nam[[1]][1]
  
  for(i in 2:length(nam)) {
    x <- nam[[i]][1]
    names <- c(names, x)
  }
  
  
  # now, cropping the env var by the extent of the mask 
  remask <- resample(env.raster, mask)
  env.clipped <- terra::mask(remask, mask)
  
  # writing the files in a new folder
    dir.create(paste0(raster.dir, "/", mask.name, "_clipped"))
  
  for(i in 1:length(names)) {
    terra::writeRaster(env.clipped[[i]], 
                       filename = paste0(raster.dir, "/", mask.name, "_clipped/", 
                                         names[i], "_", mask.name, "_clipped.tif"),
                       filetype = "GTiff")
  }
  
  end.msg <- cat(paste("\n check your working directory ! \n"))
  
  return(end.msg)
}
