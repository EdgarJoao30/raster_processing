#loading packages
require(rgdal)
require(maptools)
require(sp)
require(raster)
require(ggplot2)
require(plyr)
require(rgeos)
require(Hmisc)
require(dplyr)
require(gridsample)
#reading files
loreto.shp <- shapefile('~/Dropbox/upch/Shapefiles_Project/dist_loreto/loreto_rpj.shp')
loreto.pop <- read.csv('~/Dropbox/upch/Shapefiles_Project/loreto_inei.csv')
eess.shp <- shapefile('~/Dropbox/upch/Shapefiles_Project/est_salud/eess.shp')
loreto.raster <-raster("~/Dropbox/upch/worldPop/PER_ppp_v2b_2015_UNadj.tif")
#getting files ready
loreto.raster <- crop(loreto.raster, extent(loreto.shp)) 
loreto.raster <- raster::mask(x=loreto.raster, mask=loreto.shp) 
loreto.wpop <- raster::extract(loreto.raster, loreto.shp, small=T, fun=sum, na.rm=T,layer=1, df=TRUE)
loreto.wpop$District <- capitalize(tolower(loreto.shp@data$NOMBDIST))
loreto.pop$District <- capitalize(tolower(loreto.pop$District))
loreto.pop <- merge(loreto.pop, loreto.wpop, by = 'District')
loreto.pop$ID<-NULL
colnames(loreto.pop)[5] <- 'wpop'
rm(loreto.wpop)
loreto.pop <- mutate(loreto.pop, weigth = pop2015/wpop)
loreto.pop <- mutate(loreto.pop, urb.ratio = urb2007/pop2007)
eess.shp <- gBuffer(eess.shp, width=0.1)
loreto.raster <- crop(loreto.raster, extent(eess.shp)) 
loreto.raster <- raster::mask(x=loreto.raster, mask=eess.shp)
#running gridsample
distList<-loreto.shp@data$NOMBDIST
distList <- sort(distList)
for (i in 1:51) {
  print(paste0("iteration number: " ,i))
  print("getting district pop raster")
  #getting district pop raster
  clip_dist <- crop(loreto.raster, extent(loreto.shp[loreto.shp@data$NOMBDIST == as.character(distList[i]),])) 
  clip_dist <- raster::mask(x=clip_dist, mask=loreto.shp[loreto.shp@data$NOMBDIST == as.character(distList[i]),])
  clip_dist <- calc(clip_dist, function(x) x*as.numeric(loreto.pop$weigth[i]))
  print("getting urban raster")
  #getting urban raster
  total_pop=cellStats(clip_dist,stat="sum")
  pop_urban = total_pop* as.numeric(loreto.pop$urb.ratio[i])
  pop_df = data.frame(index = 1:length(clip_dist[]),pop = clip_dist[])
  pop_df = pop_df[!is.na(pop_df$pop),]
  pop_df = pop_df[order(pop_df$pop,decreasing = T),]
  pop_df$cumulative_pop = cumsum(pop_df$pop)
  pop_df$urban = 0
  pop_df$urban[which(pop_df$cumulative_pop<=pop_urban)] = 1
  raster_urban <- clip_dist >= min(subset(pop_df,urban == 1)$pop)
  print("getting strata raster")
  #getting strata raster
  strata_shape <- loreto.shp[loreto.shp@data$NOMBDIST == as.character(distList[i]),]
  strata_raster<-rasterize(strata_shape,clip_dist)
  rm(strata_shape)
  print("grdsample")
  #grdsample
  psu_loreto<-gs_sample(population_raster = clip_dist, 
                        strata_raster = strata_raster,
                        urban_raster = raster_urban,
                        cfg_desired_cell_size = NA,
                        cfg_hh_per_stratum = 40,
                        cfg_hh_per_urban = 10,
                        cfg_hh_per_rural = 10,
                        cfg_min_pop_per_cell = clip_dist@data@min,
                        cfg_max_psu_size = 10, 
                        cfg_pop_per_psu = 500,
                        cfg_psu_growth = TRUE,
                        cfg_sample_rururb = TRUE,
                        cfg_sample_spatial = FALSE,
                        cfg_sample_spatial_scale = 0.1,
                        output_path=tempdir(),
                        sample_name="loreto_psu")
  
  psu_loreto@data$stratum <- as.character(distList[i])
  assign(paste0('psu_',tolower(gsub(' ','',as.character(distList[i])))),psu_loreto)
  rm(psu_loreto)
  rm(list = c('raster_urban', 'strata_raster', 'clip_dist', 'pop_urban', 'total_pop', 'i','pop_df'))
  
}

#plot(loreto.shp)
all_my_shapes <- mget(ls(pattern = "psu_.*"))
rm(list = ls(pattern = "psu_.*"))
#lapply(all_my_shapes,plot, add=T, col='red')
psu <- all_my_shapes[[1]]
for(i in 2:length(all_my_shapes)){
  print(paste0('iteration number: ', i-1))
  psu <- union(psu, all_my_shapes[[i]])
  
}
rm(all_my_shapes)
setwd('~/Dropbox/upch/Shapefiles_Project/psu_10km_2/')
writeSpatialShape(psu_2, 'psu_10km')

#dont run, this converts sp into spdf to write a shapefile of the buffer
eess.id <- sapply(slot(eess.shp, "polygons"), function(x) slot(x, "ID"))
eess.df <- data.frame( ID=1:length(eess.shp), row.names = eess.id)
eess.spdf <- SpatialPolygonsDataFrame(eess.shp, eess.df) 
writeSpatialShape(eess.spdf, 'eess_5km_r')