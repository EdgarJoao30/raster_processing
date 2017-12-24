# ------------------------------------------------------------------------------
# Libraries
# ------------------------------------------------------------------------------
require(raster)
require(maptools)
require(rgdal)
require(dplyr)
options(digits=11)
# ------------------------------------------------------------------------------
# Directory
# ------------------------------------------------------------------------------
rasterdir1 <- "C:/Users/CORE_DM/Desktop/edgar/rasters/all_in_one/F6/"
setwd(rasterdir1)
# ------------------------------------------------------------------------------
# list of files
# ------------------------------------------------------------------------------
temp = list.files(pattern="*.tif")
# ------------------------------------------------------------------------------
# creates a raster for each band (9) 
#band 1 red
#band 2 green
#band 3 blue
#band 4 red m
#band 5 green m
#band 6 edge red
#band 7 nir
#band 8 ndvi
#band 9 classification
# ------------------------------------------------------------------------------
for (i in 1:9) {
  if (length(temp) < 2) {
    myfile = raster(temp[[1]], band = i)
    assign(paste0('multi_0_band_',i),myfile)
    } else {myfiles = lapply(temp, function(x) raster(x, band = i, stringsAsFactors = FALSE))
    myfiles$tolerance <- 1
    myfile = do.call(raster::merge, myfiles)
    assign(paste0('multi_0_band_',i),myfile)
  }
  
}

# ------------------------------------------------------------------------------
# creates a list of the rasters
# ------------------------------------------------------------------------------
rasters_multi <- mget(ls(pattern = "multi_0_band.*"))
# ------------------------------------------------------------------------------
# cleaning
# ------------------------------------------------------------------------------
rm(list = ls(pattern ="multi_0_band.*" ))
rm(myfile)
rm(myfiles)
# ------------------------------------------------------------------------------
# creates a data frame for each raster
# ------------------------------------------------------------------------------
matrix0 <- lapply(rasters_multi, function (y) rasterToPoints(y, function(x) x == x ))
df0 <- lapply(matrix0, function (x) data.frame(x))
# ------------------------------------------------------------------------------
# more cleaning
# ------------------------------------------------------------------------------
rm(matrix0)
rm(rasters_multi)
# ------------------------------------------------------------------------------
# cuts the dataframes to the min number of obs
# ------------------------------------------------------------------------------
min_value <- min(c(length(df0$multi_0_band_1$x), 
      length(df0$multi_0_band_2$x), 
      length(df0$multi_0_band_3$x),
      length(df0$multi_0_band_4$x),
      length(df0$multi_0_band_5$x),
      length(df0$multi_0_band_6$x),
      length(df0$multi_0_band_7$x),
      length(df0$multi_0_band_8$x),
      length(df0$multi_0_band_9$x)))

df0_2 <- lapply(df0, function(x) x[1:min_value,]) 
# ------------------------------------------------------------------------------
# combines the dfs into a single df
# ------------------------------------------------------------------------------
df_multi <-do.call(cbind, df0_2)
df_multi2 <- df_multi[,c(1,2,3,6,9,12,15,18,21,24,27)]
colnames(df_multi2) <- c('x','y','red','green','blue','red_m','green_m','edge_red','nir','ndvi','classification')
# ------------------------------------------------------------------------------
# saving the file
# ------------------------------------------------------------------------------
setwd("C:/Users/CORE_DM/Desktop/")
write.csv(df_multi2, file = 'F6.csv')




require(parallel)
# ------------------------------------------------------------------------------
# Directory
# ------------------------------------------------------------------------------
direct <- "C:/Users/CORE_DM/Desktop/edgar/rasters/all_in_one/csv/"
setwd(direct)
# ------------------------------------------------------------------------------
# list of files
# ------------------------------------------------------------------------------
tempf = list.files(pattern="*.csv")

# Calculate the number of cores
no_cores <- detectCores() - 1

# Initiate cluster
cl <- makeCluster(no_cores)

files <- parLapply(cl, tempf, function(x) read.csv(x, stringsAsFactors = FALSE))
stopCluster(cl)
file <- do.call(rbind, files)

file2<- file[((length(file$x)/3)+1):(2*(length(file$x)/3)),]
file3<- file[((2*(length(file$x)/3))+1):length(file$x),]
#this is a test
#test number 3
