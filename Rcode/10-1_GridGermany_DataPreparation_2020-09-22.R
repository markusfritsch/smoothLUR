#########################################################################
### Set up 1x1km spatial grid for prediction across Germany,
### i.e. derive values of predictor variables for each grid cellcentre
#########################################################################





rm(list = ls())


#	setwd("D:/Work/20_Projekte/570_Behm-and-Fritsch/smoothLUR")


library(rgdal)
library(xlsx)
library(raster)
library(rgeos)
library(sp)





# Read data ----

sPdf.boundaries.DE <- readOGR(dsn = "DataFull/Data_GADM",
                              layer = "DEU_adm0",
                              encoding = "UTF-8",
                              use_iconv = TRUE)
sGdf.CLC12 <- readGDAL("DataFull/Data_CLC12/g100_clc12_V18_5a/g100_clc12_V18_5.tif",
                       offset = c(19480,67313),
                       region.dim = c(8700,6500))
sPdf.boundaries.DE <- spTransform(sPdf.boundaries.DE,
                                  CRS = proj4string(sGdf.CLC12))


admin.regions.2015 <- readOGR(dsn = "DataFull/Data_BKG/vg250-ew_ebenen",
                              layer = "VG250_GEM",
                              encoding = "UTF-8",
                              use_iconv = TRUE)
proj4string(admin.regions.2015)
admin.regions.2015 <- spTransform(admin.regions.2015, CRS = proj4string(sGdf.CLC12))


df.area2015 <- read.xlsx("DataFull/Data_BBSR/Gem15-Raumtyp.xlsx",
                         header = TRUE,
                         sheetIndex = 1,
                         startRow = 3,
                         endRow = 11169,
                         colIndex = seq(1,7,1),
                         encoding = "UTF-8")

df.popDens2015 <- read.xlsx("DataFull/Data_BBSR/Gem15-Raumtyp.xlsx",
                            header = TRUE,
                            sheetIndex = 2,
                            startRow = 3,
                            endRow = 4541,
                            colIndex = seq(1,6,1),
                            encoding = "UTF-8")

sLdf.roads.DE <- readOGR(dsn = "DataFull/Data_EuroGeographics/EGM_10-1-0SHP_20171110/DATA/Countries/DE", layer = "RoadL",
                         encoding = "UTF-8",use_iconv = TRUE)
sLdf.roads.DE <- spTransform(sLdf.roads.DE, CRS = proj4string(sGdf.CLC12))





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
# spplot(grid.DE.overlapping)
# -> first grid cell is top left, last grid cell is bottom right

rgrid.DE.overlapping <- raster(extent(grid.DE.overlapping), res = c(1000, 1000), proj4string(sGdf.CLC12))
rgrid.DE.overlapping[] <- seq(from = 1, to = grid.topo@cells.dim[1]*grid.topo@cells.dim[2], by = 1)

# plot(rgrid.DE.overlapping)

rgrid.DE.msk <- mask(rgrid.DE.overlapping, sPdf.boundaries.DE)


cellcentres.DE <- as(rgrid.DE.msk, "SpatialPointsDataFrame")
proj4string(cellcentres.DE) <- proj4string(grid.DE.overlapping)

cellcentres.DE$lon.GK3 <- coordinates(cellcentres.DE)[,1]
cellcentres.DE$lat.GK3 <- coordinates(cellcentres.DE)[,2]

WGS84 <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

spdf.cellcentres.WGS84 <- spTransform(cellcentres.DE, WGS84)
cellcentres.DE$lon.WGS84 <- coordinates(spdf.cellcentres.WGS84)[,1]
cellcentres.DE$lat.WGS84 <- coordinates(spdf.cellcentres.WGS84)[,2]



# Assign data from BBSR and BKG ----


# Exploring and adjusting data - for details have a look in '01-MonitoringSites_DE_DataPreparation.R'
names(df.area2015)
names(df.popDens2015)
names(admin.regions.2015)


admin.regions.2015 <- admin.regions.2015[,c("RS", "AGS", "GEN", "NUTS", "EWZ")]

# Calculate area of each polygon in 'admin.regions.2015' and add column to 'admin.regions.2015@data'
proj4string(admin.regions.2015) # unit=m
# gArea() returns area in m^2, divide by 1000*1000 to get area in km^2
admin.regions.2015$Area_km2 <- gArea(admin.regions.2015, byid = TRUE) / 1000000

admin.regions.2015$popDens <- as.numeric(admin.regions.2015$EWZ) / admin.regions.2015$Area_km2
summary(admin.regions.2015$popDens)


# Note that calculating area of municipalities based on the respective polygon is just an approximation as
# a polygon is built upon connecting points and resembles the shape of the respective municipality not exactly.
# More exact values for the population density can be retrieved using data from BBSR...



dummy.tmp <- nchar(df.area2015$gem15) == 7
df.area2015$gem15_full[dummy.tmp] <- paste(0,df.area2015$gem15[dummy.tmp],sep="")
df.area2015$gem15_full[!dummy.tmp] <- df.area2015$gem15[!dummy.tmp]
all(admin.regions.2015$AGS %in% df.area2015$gem15_full)
table(admin.regions.2015$AGS %in% df.area2015$gem15_full)
# there are 185 municipalities for which no entry is available in `df.area`
#View(admin.regions.2015[!(admin.regions.2015$AGS %in% df.area2015$gem15_full),]@data)

# Data\Data_BKG\dokumentation\vg250.pdf
# search for 'gemeindefrei' -> Regionalschlüssel (RS) 6.Stelle
admin.regions.2015$digit6 <- sapply(as.vector(admin.regions.2015$RS), function(x) substring(x,6,6))


regions.2015 <- merge(admin.regions.2015, df.area2015[,c("vbgem15", "gem15_full")],
                      by.y = "gem15_full", by.x = "AGS", all.x = TRUE)

# Add manually the information for 'Berlin' and 'Hamburg' contained in in df.area.2015
df.area2015[df.area2015$name15%in%c("Berlin, Stadt", "Hamburg, Freie und Hansestadt"),]
#View(regions.2015@data[is.na(regions.2015$vbgem15),]) # filter for  digit == 0
regions.2015@data[is.na(regions.2015$vbgem15) & regions.2015$digit6==0, "vbgem15"] <- df.area2015[c(1113,1113,8497),"vbgem15"]

table(is.na(regions.2015$vbgem15))
# now there are 182 entries in 'regions.2015' without a value for 'vbgem15'
# these entries refer to regions as forests where in general no people live

proj4string(regions.2015)
proj4string(cellcentres.DE)

cellcentres.DE@data <- cbind(cellcentres.DE@data, over(cellcentres.DE, regions.2015))

table(is.na(cellcentres.DE$AGS))
# -> 757 NA's
#View(cellcentres.DE@data[is.na(cellcentres.DE$AGS),])

plot(sPdf.boundaries.DE, col = "white", lty = 2)
points(cellcentres.DE[is.na(cellcentres.DE$AGS),"layer"], pch = 4, col = "blue")
# -> all NA's lie on the German boundary -> remove the respective cells from the prediction grid


# See further below
# cellcentres.DE2 <- subset(cellcentres.DE, !is.na(cellcentres.DE$AGS))
#
# summary(duplicated(df.popDens2015$vbgem15)) # unambiguously identified
# table(is.na(cellcentres.DE2$vbgem15))
# # 2288 NA's
#
# cellcentres.DE2  <- merge(cellcentres.DE2, df.popDens2015[,c(1,3,4)],
#                           by = "vbgem15",
#                           all.x = TRUE)
#
# cellcentres.DE2$popDens15 <- cellcentres.DE2$z_bev15/cellcentres.DE2$fl15_sum
#
# summary(cellcentres.DE2$popDens15)
# 2288 NA's
# View(cellcentres.DE2@data[is.na(cellcentres.DE2$popDens15),])
# Mainly forest -> either building arithmetic means over cells attributed to the same NUTS level or setting 0


if(FALSE){
# Assign CLC ----
# Assign to each cellcentre the shares of the grouped land use classes that cover the buffer with radius 1km around the cellcentre
## Write data frame that relates the 44 CLC classes to the 10 grouped classes in Beelen et al. (2009), p.1854-1855
df.CLC.grouped <- data.frame(matrix(data=0,nrow=10,ncol=3))
names(df.CLC.grouped) <- c("GroupedClass", "Description", "CLCclasses")
df.CLC.grouped$GroupedClass <- 1:10
df.CLC.grouped$Description <- c("High density residential","Low density residential", "Industry", "Transport",
                                "Seaports", "Airports", "Construction","Urban Greenery", "Agriculture", "Forest")
df.CLC.grouped$CLCclasses <- c(as.character(1:6),"7-9","10-11","12-22","23-25")


# Transform SpatialGridDataFrame to raster
raster.CLC12 <- raster(sGdf.CLC12)


CLC.Grid <- data.frame(matrix(data = 0,
                              nrow = nrow(cellcentres.DE),
                              ncol = 11))

names(CLC.Grid) <- c("ID",
                     "HighDens", "LowDens", "Ind", "Transp", "Seap",
                     "Airp", "Constr", "UrbGreen", "Agri", "Forest")

CLC.Grid$ID <- cellcentres.DE$layer


for(i in 1:nrow(cellcentres.DE)){
  list  <- extract(raster.CLC12,
                   coordinates(cellcentres.DE[i,]),
                   buffer = 1000)
  nr.cells <- length(list[[1]])
  CLC.Grid[i, -1] <- c(sum(list[[1]] == 1), sum(list[[1]] == 2), sum(list[[1]] == 3),
                       sum(list[[1]] == 4), sum(list[[1]] == 5), sum(list[[1]] == 6),
                       sum(list[[1]] %in% c(7:9)), sum(list[[1]] %in% c(10:11)),
                       sum(list[[1]] %in% c(12:22)), sum(list[[1]] %in% c(23:25))) / nr.cells
}


write.csv(CLC.Grid, "DataFull/Data_built/Attribute_CLC.csv")
}


CLC.Grid <- read.csv("DataFull/Data_built/Attribute_CLC.csv")[,-1]


table(cellcentres.DE@data$layer == CLC.Grid$ID)

cellcentres.DE.tmp <- cellcentres.DE
cellcentres.DE@data <- cbind(cellcentres.DE.tmp@data, CLC.Grid)

cellcentres.DE2  <- subset(cellcentres.DE, !is.na(cellcentres.DE$AGS))
cellcentres.DE2  <- merge(cellcentres.DE2, df.popDens2015[,c(1,3,4)],
                          by = "vbgem15",
                          all.x = TRUE)
cellcentres.DE2$popDens15 <- cellcentres.DE2$z_bev15/cellcentres.DE2$fl15_sum



# Assign traffic information from EuroGeographics ----
if(FALSE){
spdf.cellcentres.DE2 <- as(cellcentres.DE2, "SpatialPointsDataFrame")
BufferCellcentres <- gBuffer(spdf.cellcentres.DE2,
                             byid = TRUE,
                             id = as.character(cellcentres.DE2$layer),
                             width = 1000)
plot(BufferCellcentres[1,])
points(spdf.cellcentres.DE2[1,])

Roads.in.Buffers <- gIntersects(BufferCellcentres[280001:300000,], sLdf.roads.DE, byid = TRUE)

spdf.cellcentres.DE2$Road14 <- 0
spdf.cellcentres.DE2$Road15 <- 0
spdf.cellcentres.DE2$Road16 <- 0
spdf.cellcentres.DE2$Road984 <- 0

for(i in 1:10000){
  sLdf.tmp <- subset(sLdf.roads.DE,row.names(sLdf.roads.DE)%in%row.names(sLdf.roads.DE)[Roads.in.Buffers[,i]])
  if(nrow(sLdf.tmp)>0){
    sLdf.tmp2 <- crop(sLdf.tmp,BufferCellcentres[280000+i,])
    sLdf.tmp2.14 <- subset(sLdf.tmp2, sLdf.tmp2$RTT==14)
    sLdf.tmp2.15 <- subset(sLdf.tmp2, sLdf.tmp2$RTT==15)
    sLdf.tmp2.16 <- subset(sLdf.tmp2, sLdf.tmp2$RTT==16)
    sLdf.tmp2.984 <- subset(sLdf.tmp2, sLdf.tmp2$RTT==984)
    if(nrow(sLdf.tmp2.14)>0){
      spdf.cellcentres.DE2$Road14[280000+i] <- sum(SpatialLinesLengths(sLdf.tmp2.14))
    }
    if(nrow(sLdf.tmp2.15)>0){
      spdf.cellcentres.DE2$Road15[280000+i] <- sum(SpatialLinesLengths(sLdf.tmp2.15))
    }
    if(nrow(sLdf.tmp2.16)>0){
      spdf.cellcentres.DE2$Road16[280000+i] <- sum(SpatialLinesLengths(sLdf.tmp2.16))
    }
    if(nrow(sLdf.tmp2.984)>0){
      spdf.cellcentres.DE2$Road984[280000+i] <- sum(SpatialLinesLengths(sLdf.tmp2.984))
    }
  }
}

write.csv(spdf.cellcentres.DE2@data[280001:300000, ], file = "Attribute_Road_280001to300000.csv")
}

Roads1  <- read.csv("DataFull/Data_built/Attribute_Road_1to20000.csv")[,-1]
Roads2  <- read.csv("DataFull/Data_built/Attribute_Road_20001to40000.csv")[,-1]
Roads3  <- read.csv("DataFull/Data_built/Attribute_Road_40001to60000.csv")[,-1]
Roads4  <- read.csv("DataFull/Data_built/Attribute_Road_60001to80000.csv")[,-1]
Roads5  <- read.csv("DataFull/Data_built/Attribute_Road_80001to100000.csv")[,-1]
Roads6  <- read.csv("DataFull/Data_built/Attribute_Road_100001to120000.csv")[,-1]
Roads7  <- read.csv("DataFull/Data_built/Attribute_Road_120001to140000.csv")[,-1]
Roads8  <- read.csv("DataFull/Data_built/Attribute_Road_140001to160000.csv")[,-1]
Roads9  <- read.csv("DataFull/Data_built/Attribute_Road_160001to180000.csv")[,-1]
Roads10 <- read.csv("DataFull/Data_built/Attribute_Road_180001to200000.csv")[,-1]
Roads11 <- read.csv("DataFull/Data_built/Attribute_Road_200001to220000.csv")[,-1]
Roads12 <- read.csv("DataFull/Data_built/Attribute_Road_220001to240000.csv")[,-1]
Roads13 <- read.csv("DataFull/Data_built/Attribute_Road_240001to260000.csv")[,-1]
Roads14 <- read.csv("DataFull/Data_built/Attribute_Road_260001to280000.csv")[,-1]
Roads15 <- read.csv("DataFull/Data_built/Attribute_Road_280001to300000.csv")[,-1]
Roads16 <- read.csv("DataFull/Data_built/Attribute_Road_300001to320000.csv")[,-1]
Roads17 <- read.csv("DataFull/Data_built/Attribute_Road_320001to340000.csv")[,-1]
Roads18 <- read.csv("DataFull/Data_built/Attribute_Road_340001to356793.csv")[,-1]


RoadsComplete <- rbind(Roads1, Roads2, Roads3, Roads4, Roads5, Roads6,
                       Roads7, Roads8, Roads9, Roads10, Roads11, Roads12,
                       Roads13, Roads14, Roads15, Roads16, Roads17, Roads18)

cellcentres.DE3 <- cellcentres.DE2
cellcentres.DE3 <- merge(cellcentres.DE3, RoadsComplete[,c(2,21:24)],
                              by = "layer", all.x = TRUE)



# Assign Altitude ----
DGM <- readGDAL("DataFull/Data_BKG/DGM/dgm200.gk3.gridascii/dgm200/dgm200_gk3.asc")
proj4string(DGM) <- "+proj=tmerc +lat_0=0 +lon_0=9 +k=1 +x_0=3500000 +y_0=0 +datum=potsdam +units=m +no_defs +ellps=bessel +towgs84=598.1,73.7,418.2,0.202,0.045,-2.455,6.7"

raster.DGM <- raster(DGM)

cellcentres.DE3 <- spTransform(cellcentres.DE3, proj4string(raster.DGM))

cellcentres.DE3$Altitude <- extract(raster.DGM, cellcentres.DE3)



# Define final prediction grid ----

cellcentres.final <- cellcentres.DE3

df.grid.final <- cellcentres.final@data[ , c(1, 3:6, 33, 16:25, 7, 9:10, 13, 28:32)]
names(df.grid.final) <- c("layer", "lon.GK3", "lat.GK3", "lon.WGS84", "lat.WGS84", "Alt",
                          "HighDens", "LowDens", "Ind", "Transp", "Seap",
                          "Airp", "Constr", "UrbGreen", "Agri", "Forest",
                          "AGS", "GEN", "NUTS", "PopDens", "BBSRpopDens",
                          "PriRoad", "SecRoad", "FedAuto", "LocRoute")

# Add column of type factor indicating the municipality
df.grid.final$indRegions <- NA
df.grid.final$indRegions[nchar(df.grid.final$AGS) == 7] <- substr(df.grid.final$AGS[nchar(df.grid.final$AGS) == 7], 1, 1)
df.grid.final$indRegions[nchar(df.grid.final$AGS) != 7] <- substr(df.grid.final$AGS[nchar(df.grid.final$AGS) != 7], 1, 2)


# df.grid.DE <- df.grid.final
# rm(list=(ls()[ls()!="df.grid.DE"]))
# save.image("DataFull/Data_built/grid.DE.RData")
