<<<<<<< HEAD
########################################################################################################
### Set up 1x1km spatial grid across Germany (i.e., derive predictor values for each grid cell center
########################################################################################################
=======
#########################################################################
### Set up 1x1km spatial grid for prediction across Germany,
### i.e. derive values of predictor variables for each grid cell center
#########################################################################
>>>>>>> 10b46e97d58dcbd1354e02f86ecf27239a3a0e6e





rm(list = ls())



library(rgdal)
library(xlsx)
library(raster)
library(rgeos)
library(sp)





# Read data ----

<<<<<<< HEAD
sPdf.Boundaries.DE <- readOGR(dsn = "data/Data_GADM",
                              layer = "DEU_adm0",
                              encoding = "UTF-8",
                              use_iconv = TRUE)
sGdf.CLC12 <- readGDAL(fname = "data/Data_CLC12/g100_clc12_V18_5a/g100_clc12_V18_5.tif",
=======
sPdf.Boundaries.DE <- readOGR(dsn = "DataFull/Data_GADM",
                              layer = "DEU_adm0",
                              encoding = "UTF-8",
                              use_iconv = TRUE)
sGdf.CLC12 <- readGDAL("DataFull/Data_CLC12/g100_clc12_V18_5a/g100_clc12_V18_5.tif",
>>>>>>> 10b46e97d58dcbd1354e02f86ecf27239a3a0e6e
                       offset = c(19480,67313),
                       region.dim = c(8700,6500))
sPdf.Boundaries.DE <- spTransform(sPdf.Boundaries.DE,
                                  CRS = proj4string(sGdf.CLC12))



# Construct 1 x 1 km^2 grid ----
# over Germany on the basis of sGdf.CLC12
# sGdf.CLC12@grid@cells.dim
# [1] 6500 8700
# -> sGdf.CLC12 contains 6500 * 8700 =  56550000 cells, each of size 100 x 100 m^2
# -> for a resolution of 1 x 1 km^2 only 650 * 870 = 565500 cells, each of size 1 x 1 km^2, are required
grid.topo <- GridTopology(cellcentre.offset = sGdf.CLC12@grid@cellcentre.offset,
                          cellsize = c(1000, 1000),
                          cells.dim = sGdf.CLC12@grid@cells.dim/10)

grid.DE.overlapping <- SpatialGridDataFrame(grid.topo,
                                            data = data.frame(seq(from = 1, to = grid.topo@cells.dim[1]*grid.topo@cells.dim[2], by = 1)),
                                            proj4string = proj4string(sGdf.CLC12))

rgrid.DE.overlapping <- raster(extent(grid.DE.overlapping), res = c(1000, 1000), proj4string(sGdf.CLC12))
rgrid.DE.overlapping[] <- seq(from = 1, to = grid.topo@cells.dim[1]*grid.topo@cells.dim[2], by = 1)
rgrid.DE.msk <- mask(rgrid.DE.overlapping, sPdf.Boundaries.DE)


spdf.grid <- as(rgrid.DE.msk, "SpatialPointsDataFrame")
proj4string(spdf.grid) <- proj4string(grid.DE.overlapping)

spdf.grid$Lon.GK3 <- coordinates(spdf.grid)[,1]
spdf.grid$Lat.GK3 <- coordinates(spdf.grid)[,2]

WGS84 <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

spdf.grid.WGS84 <- spTransform(spdf.grid, WGS84)
spdf.grid$Lon.WGS84 <- coordinates(spdf.grid.WGS84)[,1]
spdf.grid$Lat.WGS84 <- coordinates(spdf.grid.WGS84)[,2]



# Data on land use ----
# Assign to each grid cell center shares of grouped land use classes that cover buffer
# with radius 1km around grid cell center

if(FALSE){
  # Transform SpatialGridDataFrame to raster
  raster.CLC12 <- raster(sGdf.CLC12)

  CLC.Grid <- data.frame(matrix(data = 0,
                                nrow = nrow(spdf.grid),
                                ncol = 11))

  names(CLC.Grid) <- c("ID",
                       "HighDens", "LowDens", "Ind", "Transp", "Seap",
                       "Airp", "Constr", "UrbGreen", "Agri", "Forest")

  CLC.Grid$ID <- spdf.grid$layer


  for(i in 1:nrow(spdf.grid)){
    list  <- extract(raster.CLC12,
                     coordinates(spdf.grid[i,]),
                     buffer = 1000)
    nr.cells <- length(list[[1]])
    CLC.Grid[i, -1] <- c(sum(list[[1]] == 1), sum(list[[1]] == 2), sum(list[[1]] == 3),
                         sum(list[[1]] == 4), sum(list[[1]] == 5), sum(list[[1]] == 6),
                         sum(list[[1]] %in% c(7:9)), sum(list[[1]] %in% c(10:11)),
                         sum(list[[1]] %in% c(12:22)), sum(list[[1]] %in% c(23:25))) / nr.cells
  }


<<<<<<< HEAD
  write.csv(CLC.Grid, "data/Data_built/Attribute_CLC.csv", row.names = FALSE)
}

CLC.Grid <- read.csv("data/Data_built/Attribute_CLC.csv")
=======
  write.csv(CLC.Grid, "Data_built/Attribute_CLC.csv")
}

CLC.Grid <- read.csv("Data_built/Attribute_CLC.csv")[,-1]
>>>>>>> 10b46e97d58dcbd1354e02f86ecf27239a3a0e6e

table(spdf.grid@data$layer == CLC.Grid$ID)
spdf.grid.tmp <- spdf.grid
spdf.grid@data <- cbind(spdf.grid.tmp@data, CLC.Grid)




# Data on population density ----

<<<<<<< HEAD
sPdf.Municipalities <- readOGR(dsn = "data/Data_BKG/vg250-ew_ebenen",
=======
sPdf.Municipalities <- readOGR(dsn = "DataFull/Data_BKG/vg250-ew_ebenen",
>>>>>>> 10b46e97d58dcbd1354e02f86ecf27239a3a0e6e
                               layer = "VG250_GEM",
                               encoding = "UTF-8",
                               use_iconv = TRUE)
proj4string(sPdf.Municipalities)
sPdf.Municipalities <- spTransform(sPdf.Municipalities, CRS = proj4string(sGdf.CLC12))

sPdf.Municipalities <- sPdf.Municipalities[,c("AGS", "EWZ")]
sPdf.Municipalities$Area_km2 <- gArea(sPdf.Municipalities, byid = TRUE) / 1000000
sPdf.Municipalities$PopDens <- as.numeric(sPdf.Municipalities$EWZ) / sPdf.Municipalities$Area_km2


proj4string(sPdf.Municipalities)
proj4string(spdf.grid)

spdf.grid@data <- cbind(spdf.grid@data, over(spdf.grid, sPdf.Municipalities))

table(is.na(spdf.grid$AGS))
# -> 757 NA's
#View(spdf.grid@data[is.na(spdf.grid$AGS),])

plot(sPdf.Boundaries.DE, col = "white", lty = 2)
points(spdf.grid[is.na(spdf.grid$AGS),"layer"], pch = 4, col = "blue")
# -> all NA's lie on the German boundary -> remove the respective cells from the prediction grid

spdf.grid2  <- subset(spdf.grid, !is.na(spdf.grid$AGS))



# Data on traffic ----

if(FALSE){
<<<<<<< HEAD
  sLdf.Roads <- readOGR(dsn = "data/Data_EuroGeographics/EGM_10-1-0SHP_20171110/DATA/Countries/DE", layer = "RoadL",
=======
  sLdf.Roads <- readOGR(dsn = "DataFull/Data_EuroGeographics/EGM_10-1-0SHP_20171110/DATA/Countries/DE", layer = "RoadL",
>>>>>>> 10b46e97d58dcbd1354e02f86ecf27239a3a0e6e
                        encoding = "UTF-8",use_iconv = TRUE)
  sLdf.Roads <- spTransform(sLdf.Roads, CRS = proj4string(sGdf.CLC12))

    spdf.spdf.grid2 <- as(spdf.grid2, "SpatialPointsDataFrame")
  BufferCellcentres <- gBuffer(spdf.spdf.grid2,
                               byid = TRUE,
                               id = as.character(spdf.grid2$layer),
                               width = 1000)

  Roads.in.Buffers <- gIntersects(BufferCellcentres[280001:300000,], sLdf.Roads, byid = TRUE)

  spdf.spdf.grid2$PriRoad  <- 0
  spdf.spdf.grid2$SecRoad  <- 0
  spdf.spdf.grid2$FedAuto  <- 0
  spdf.spdf.grid2$Locroute <- 0

  for(i in 1:10000){
    sLdf.tmp <- subset(sLdf.Roads,row.names(sLdf.Roads)%in%row.names(sLdf.Roads)[Roads.in.Buffers[,i]])
    if(nrow(sLdf.tmp)>0){
      sLdf.tmp2 <- crop(sLdf.tmp,BufferCellcentres[280000+i,])
      sLdf.tmp2.14 <- subset(sLdf.tmp2, sLdf.tmp2$RTT==14)
      sLdf.tmp2.15 <- subset(sLdf.tmp2, sLdf.tmp2$RTT==15)
      sLdf.tmp2.16 <- subset(sLdf.tmp2, sLdf.tmp2$RTT==16)
      sLdf.tmp2.984 <- subset(sLdf.tmp2, sLdf.tmp2$RTT==984)
      if(nrow(sLdf.tmp2.14)>0){
        spdf.spdf.grid2$PriRoad[280000+i]  <- sum(SpatialLinesLengths(sLdf.tmp2.14))
        }
      if(nrow(sLdf.tmp2.15)>0){
        spdf.spdf.grid2$SecRoad[280000+i]  <- sum(SpatialLinesLengths(sLdf.tmp2.15))
        }
      if(nrow(sLdf.tmp2.16)>0){
        spdf.spdf.grid2$FedAuto[280000+i]  <- sum(SpatialLinesLengths(sLdf.tmp2.16))
        }
      if(nrow(sLdf.tmp2.984)>0){
        spdf.spdf.grid2$LocRoute[280000+i] <- sum(SpatialLinesLengths(sLdf.tmp2.984))
      }
    }
  }

  write.csv(spdf.spdf.grid2@data[280001:300000, ], file = "Attribute_Road_280001to300000.csv")
}

<<<<<<< HEAD
Roads1  <- read.csv("data/Data_built/Attribute_Road_1to20000.csv")[,-1]
Roads2  <- read.csv("data/Data_built/Attribute_Road_20001to40000.csv")[,-1]
Roads3  <- read.csv("data/Data_built/Attribute_Road_40001to60000.csv")[,-1]
Roads4  <- read.csv("data/Data_built/Attribute_Road_60001to80000.csv")[,-1]
Roads5  <- read.csv("data/Data_built/Attribute_Road_80001to100000.csv")[,-1]
Roads6  <- read.csv("data/Data_built/Attribute_Road_100001to120000.csv")[,-1]
Roads7  <- read.csv("data/Data_built/Attribute_Road_120001to140000.csv")[,-1]
Roads8  <- read.csv("data/Data_built/Attribute_Road_140001to160000.csv")[,-1]
Roads9  <- read.csv("data/Data_built/Attribute_Road_160001to180000.csv")[,-1]
Roads10 <- read.csv("data/Data_built/Attribute_Road_180001to200000.csv")[,-1]
Roads11 <- read.csv("data/Data_built/Attribute_Road_200001to220000.csv")[,-1]
Roads12 <- read.csv("data/Data_built/Attribute_Road_220001to240000.csv")[,-1]
Roads13 <- read.csv("data/Data_built/Attribute_Road_240001to260000.csv")[,-1]
Roads14 <- read.csv("data/Data_built/Attribute_Road_260001to280000.csv")[,-1]
Roads15 <- read.csv("data/Data_built/Attribute_Road_280001to300000.csv")[,-1]
Roads16 <- read.csv("data/Data_built/Attribute_Road_300001to320000.csv")[,-1]
Roads17 <- read.csv("data/Data_built/Attribute_Road_320001to340000.csv")[,-1]
Roads18 <- read.csv("data/Data_built/Attribute_Road_340001to356793.csv")[,-1]
=======
Roads1  <- read.csv("Data_built/Attribute_Road_1to20000.csv")[,-1]
Roads2  <- read.csv("Data_built/Attribute_Road_20001to40000.csv")[,-1]
Roads3  <- read.csv("Data_built/Attribute_Road_40001to60000.csv")[,-1]
Roads4  <- read.csv("Data_built/Attribute_Road_60001to80000.csv")[,-1]
Roads5  <- read.csv("Data_built/Attribute_Road_80001to100000.csv")[,-1]
Roads6  <- read.csv("Data_built/Attribute_Road_100001to120000.csv")[,-1]
Roads7  <- read.csv("Data_built/Attribute_Road_120001to140000.csv")[,-1]
Roads8  <- read.csv("Data_built/Attribute_Road_140001to160000.csv")[,-1]
Roads9  <- read.csv("Data_built/Attribute_Road_160001to180000.csv")[,-1]
Roads10 <- read.csv("Data_built/Attribute_Road_180001to200000.csv")[,-1]
Roads11 <- read.csv("Data_built/Attribute_Road_200001to220000.csv")[,-1]
Roads12 <- read.csv("Data_built/Attribute_Road_220001to240000.csv")[,-1]
Roads13 <- read.csv("Data_built/Attribute_Road_240001to260000.csv")[,-1]
Roads14 <- read.csv("Data_built/Attribute_Road_260001to280000.csv")[,-1]
Roads15 <- read.csv("Data_built/Attribute_Road_280001to300000.csv")[,-1]
Roads16 <- read.csv("Data_built/Attribute_Road_300001to320000.csv")[,-1]
Roads17 <- read.csv("Data_built/Attribute_Road_320001to340000.csv")[,-1]
Roads18 <- read.csv("Data_built/Attribute_Road_340001to356793.csv")[,-1]
>>>>>>> 10b46e97d58dcbd1354e02f86ecf27239a3a0e6e


RoadsComplete <- rbind(Roads1, Roads2, Roads3, Roads4, Roads5, Roads6,
                       Roads7, Roads8, Roads9, Roads10, Roads11, Roads12,
                       Roads13, Roads14, Roads15, Roads16, Roads17, Roads18)

spdf.grid3 <- spdf.grid2
spdf.grid3 <- merge(spdf.grid3, RoadsComplete[,c(2,21:24)],
                              by = "layer", all.x = TRUE)



# Data on altitude ----
<<<<<<< HEAD
sGdf.Alt <- readGDAL("data/Data_BKG/DGM/dgm200.gk3.gridascii/dgm200/dgm200_gk3.asc")
=======
sGdf.Alt <- readGDAL("DataFull/Data_BKG/DGM/dgm200.gk3.gridascii/dgm200/dgm200_gk3.asc")
>>>>>>> 10b46e97d58dcbd1354e02f86ecf27239a3a0e6e
proj4string(sGdf.Alt) <- "+proj=tmerc +lat_0=0 +lon_0=9 +k=1 +x_0=3500000 +y_0=0 +datum=potsdam +units=m +no_defs +ellps=bessel +towgs84=598.1,73.7,418.2,0.202,0.045,-2.455,6.7"

raster.Alt <- raster(sGdf.Alt)

spdf.grid3 <- spTransform(spdf.grid3, proj4string(raster.Alt))

spdf.grid3$Altitude <- extract(raster.Alt, spdf.grid3)



# Define final prediction grid ----

spdf.grid.final <- spdf.grid3

names(spdf.grid.final)
df.grid.final <- spdf.grid.final@data[ , c(6,2:5,25,7:16,17,20:24)]
names(df.grid.final) <- c("ID", "Lon.GK3", "Lat.GK3", "Lon.WGS84", "Lat.WGS84", "Alt",
                          "HighDens", "LowDens", "Ind", "Transp", "Seap",
                          "Airp", "Constr", "UrbGreen", "Agri", "Forest",
                          "AGS", "PopDens", "PriRoad", "SecRoad", "FedAuto", "LocRoute")

# Add column of type factor indicating the municipality
df.grid.final$indRegions <- NA
df.grid.final$indRegions[nchar(df.grid.final$AGS) == 7] <- substr(df.grid.final$AGS[nchar(df.grid.final$AGS) == 7], 1, 1)
df.grid.final$indRegions[nchar(df.grid.final$AGS) != 7] <- substr(df.grid.final$AGS[nchar(df.grid.final$AGS) != 7], 1, 2)


# df.grid.DE <- df.grid.final
# rm(list=(ls()[ls()!="df.grid.DE"]))
<<<<<<< HEAD
# save.image("data/Data_built/grid.DE.RData")
=======
# save.image("Data_built/grid.DE.RData")
>>>>>>> 10b46e97d58dcbd1354e02f86ecf27239a3a0e6e
