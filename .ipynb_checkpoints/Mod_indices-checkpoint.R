#Indices modification script

library(RStoolbox)
library(tidyverse)
library(rgeos)
library(sp)
library(caret)
library(rgdal)
library(raster)
library(rasterVis)
library(viridis)
library(cluster)

setwd("C:/Users/s1526/Dropbox/MB3_Scripts")

Data_path <- "C:/Users/s1526/Dropbox/MB3_Scripts/Data"
list.files(Data_path)

#LOAD bands for indices and RGB
setwd(Data_path)
list.files()

b11_MIR <- raster(list.files()[1])
b8_NIR <- raster(list.files()[6])
b4_RED <- raster(list.files()[5])
b3_GREEN <- raster(list.files()[4])
b2_BLUE <- raster(list.files()[3])


IBI <- function(MIR, NIR, GREEN, RED){
  ibi <- ((2*MIR)/(MIR+NIR)-(NIR/(NIR+RED)+GREEN/(GREEN+MIR)))/
    ((2*MIR/(MIR+NIR))+(NIR/(NIR+RED)+GREEN/(GREEN+MIR)))
  return(ibi)
}

DissAgg.b11_MIR <- disaggregate(b11_MIR, fact=2)

all_band <- stack(b2_BLUE, b3_GREEN, b4_RED, b8_NIR, DissAgg.b11_MIR)

Wue_IBI <- IBI(DissAgg.b11_MIR, b8_NIR, b3_GREEN, b4_RED)
plot(Wue_IBI)

Wue_NDVI <- spectralIndices(all_band, blue=1, green=2, red=3, nir=4, swir2=5, scaleFactor = 1, index="NDVI")
plot(Wue_NDVI)

Wue_NDWI <- spectralIndices(all_band, blue=1, green=2, red=3, nir=4, swir2=5, scaleFactor = 1, index="NDWI")
plot(Wue_NDWI)

stack_indices <- stack(Wue_IBI, Wue_NDVI, Wue_NDWI)

setwd("C:/Users/s1526/Dropbox/EAGLE_Assessments/MB3_Geoinfo/MB3_FINAL/Data")
writeRaster(stack_indices, "stack_Wue_indices.tif", format="GTiff", overwrite=TRUE)