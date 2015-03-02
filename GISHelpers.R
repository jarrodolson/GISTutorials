require(raster)
require(rgdal)
require(sp)
require(ggplot2)

makeLatLong <- function(dir,fi){
  ##Reads in a shape, then transforms to a common projection
  layer <- readOGR(dir, layer=fi)
  layer <- spTransform(layer,CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  return(layer)
}

getRasterAsDF <- function(fiName,layername){
  ba <- raster(fiName)
  ##Then extract data with an x and a y column
  ba <- as.data.frame(ba,xy=TRUE)
  ba <- getLatLong(ba,layername)
}

createPtsFromRaster_maskShape <- function(name,shp=NA,minValue=0){
  ##Takes a shapefile (ogr format) and a path to a raster
  print("Reading in raster")
  txRaster <- raster(name)
  if(is.na(shp)==FALSE){
    txRaster <- clipRasterWithShape(txRaster,shp)
  }
  print("Converting raster to points dataframe")
  txRaster <- rasterToPoints(txRaster,fun=function(x){x>minValue})
  txRaster <- data.frame(txRaster)
  colnames(txRaster) <- c('x','y','dataIn')
  return(txRaster)
}

clipRasterWithShape <- function(raster,shp){
  myraster.crop <- crop(myraster,extent(myshp),snap='out')##shrinking raster with a rough estimate
  crop <- setValues(myraster.crop,NA)##Create a new shapefile with values all NA
  myshp.r <- rasterize(myshp,crop)##Rasterize new shapefile
  myraster.masked <- mask(myraster.crop,myshp.r)##Use crop.raster to mask raster
  return(myraster.masked)
}

shapeToDataFrame <- function(shape){
  ##Now we want to extract the data for ggplot plotting
  shape@data$id = rownames(shape@data)
  shape.points = fortify(shape,region='id')
  test2 <- join(shape.points,shape@data,by='id')
  return(test2)
}

dissolveShape <- function(shapeData,byName){
  #This function dissolves a shapefile by a column in the shapefile data structure (byName)
  ##First we dissolve
  cmd <- paste0('unionSpatialPolygons(shapeData,ID=shape@data$',byName,')')
  reg4 <- eval(parse(text=cmd))
  #reg4 <- unionSpatialPolygons(shape,ID=shape@data$NEMS)
  ##Then we need to create a dataframe with the same name as the polygons in the dissolved object (byName)
  cmd2 <- paste0('as.data.frame(unique(shapeData@data$',byName,'),stringsAsFactors=FALSE)')
  reg4.data <- eval(parse(text=cmd2))
  #reg4.data <- as.data.frame(unique(shape@data$NEMS),stringsAsFactors=FALSE)
  ##Then we want to assign a name to the dissolved by value
  colnames(reg4.data) <- byName
  ##THen we have to set the rownames for the new dataframe
  rownames(reg4.data) <- eval(parse(text=paste0('reg4.data$',byName)))
  ##Then we create the SpatialPolygonsDataFrame (same object as original "shapeData")
  test <- SpatialPolygonsDataFrame(reg4,reg4.data)
  return(test)
}

readShapeForDF <- function(path,name){
  ##Reads in a shape file, then converts to points dataframe
  val <- readOGR(path,name)
  val@data$id = rownames(val@data)
  val.points = fortify(val,region='id')
  val.df = join(val.points,val@data,by='id')
  return(val.df)
}

rgb2 <- function(red,green,blue){
  red <- red/255
  green <- green/255
  blue <- blue/255
  return(rgb(red,green,blue))
}

mapTheme <- theme_bw()+
  theme(panel.border=element_blank(),
        panel.grid=element_blank(),
        axis.text=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank(),
        legend.key=element_blank())

mapTheme2 <- theme_bw()+
  theme(panel.border=element_blank(),
        panel.grid=element_blank(),
        axis.text=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank())
