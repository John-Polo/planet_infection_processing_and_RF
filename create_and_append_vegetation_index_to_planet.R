library(terra)
library(tidyverse)

# List of the planet files. 
p22lis <- list.files("Z:/Late_blight/MRCfieldtest/mrcfield2022_psscene_analytic_8b_sr_udm2/PSScene",
                     full.names = T, pattern = "harmonized.*tif$")

# Planet bands
# 1 coastal blue
# 2 blue
# 3 green i 513:549
# 4 green   547:583 #This is almost equivalent to Landsat 8
# 5 yellow
# 6 red
# 7 red-edge
# 8 NIR

# NDVI  NIR-red / NIR+red
'ndvi <- ([8]-[6]) / ([8]+[6])'

# normalized difference red edge index NDRE  (NIR - RE) / (NIR + RE) 
'ndre <- ([8] - [7]) / ([8] + [7])'

# EVI (2.5 * (NIR - RED)) / (NIR + 6 * RED - 7.5 * BLUE + 1)
'evi_i <- (2.5 * ([8] - [6])) / ([8] + 6 * [6] - 7.5 * [2] + 1)'

# MSAVI ((2 * NIR + 1 - sqrt((2 * NIR + 1) ^ 2 - 8 * (NIR - RED))) / 2)
'msavi <- (((2 * [8] + 1) - sqrt((2 * [8] + 1)^2 - 8 * ([8] - [6]))) / 2)'

# GRVI (GREEN - RED) / (GREEN + RED)
'grvi <- ([4] - [6]) / ([4] + [6])'

# canopy chlorophyll content index CCCI. described N content  ((NIR - RE) / (NIR + RE) ) / ((NIR - red) / (NIR + red))
'ccci <- ( ([7]-[6]) / ([7]+[6])) / (([8] - [6]) / ([8] + [6]))'

# enhanced vegetation index is for high biomass. not crops... NO EVI
# evi <- 2.4 * ( ([[8]] - [[6]]) / ([[8]] + [[6]] + 1)  ) 

creatIdx <- function(x){
  ndvi = (x[[8]] - x[[6]]) / (x[[8]] + x[[6]])
  x = c(x, ndvi)
  ndre = (x[[8]] - x[[7]]) / (x[[8]] + x[[7]])
  x = c(x, ndre)
  evi_i = (2.5 * (x[[8]] - x[[6]])) / (x[[8]] + 6 * x[[6]] - 7.5 * x[[2]] + 1)
  x = c(x, evi_i)
  msavi = (((2 * x[[8]] + 1) - sqrt((2 * x[[8]] +1)^2 - 8 * (x[[8]] - x[[6]]))) / 2)
  x = c(x, msavi)
  grvi = (x[[4]] - x[[6]]) / (x[[4]] + x[[6]])
  x = c(x, grvi)
  ccci = ((x[[8]] - x[[7]]) / (x[[7]] + x[[8]])) / ((x[[8]] - x[[6]]) / (x[[8]] + x[[6]] ))
  x = c(x, ccci)
  cci2 = ((x[[3]] - x[[6]]) / (x[[3]] + x[[6]]))
  x = c(x, cci2)
  names(x)[c(9, 10, 11, 12, 13, 14, 15)] <- c("NDVI", "NDRE", "EVI", "MSAVI", "GRVI",
                                                  "CCCI", "CCI")
  return(x)
}

# For loop to read in one raster at a time, calculate the vegetation index, and append the index to the raster.
# Use creatIdx with all of 2022
for(i in 1:length(p22lis)){
  r1 = rast(p22lis[i])
  r2 = creatIdx(r1)
  # The has to be adjusted to the file name, which will depend on the directory path
  nm1 = str_sub(p22lis[i], 78, 92)
  # This probably has to be adjusted. It's just supposed to returned the 8 digits in the date part of the name, e.g. 20220824
  dat1 = as_date(str_sub(nm1, 1, 8))
  # Revise names
  nm2 = names(r2)
  nm2 = paste0(nm2, "_", str_sub(nm1, 1, 15))
  names(r2) = nm2
  # Add date to raster
  time(r2) = rep(dat1, nlyr(r2))
  # Save the raster
  writeRaster(r2, paste0("Z:/Late_blight/MRCfieldtest/planet/planet_plusVI_REV_",
              nm1,".tif"), overwrite=T, wopt=list(datatype="FLT8S"))
}

