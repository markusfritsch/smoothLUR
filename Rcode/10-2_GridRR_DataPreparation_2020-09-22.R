#########################################################################
### Set up 1x1km spatial grid for prediction across Rhine-Ruhr region,
### i.e. derive values of predictor variables for each grid cellcentre
#########################################################################





rm(list = ls())


#setwd("E:/Work/20_Projekte/570_Behm-and-Haupt/R/10_data")
#	setwd("D:/Work/20_Projekte/570_Behm-and-Fritsch/smoothLUR")






#install.packages("rgdal")
library(rgdal)
#install.packages("xlsx")
library(xlsx)
#install.packages("raster")
library(raster)
#install.packages("gstat")
library(gstat)
#install.packages("GISTools")
library(GISTools)




# Read data ----

load("DataFull/Data_built/grid.DE.NEW.RData")
load("DataFull/Data_built/r.popDens.NRW.RData")

# Indicator vector containing AGS (Amtliche Gemeindeschlüssel) that refer to Rhine-Ruhr region
ind.RR <- readRDS("DataFull/Data_built/indRhineRuhr.rds")


WGS84 <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

# Transform sPdf.popDens.NRW to raster object
GK3 <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"


# Filter grid for Rhine-Ruhr region
df.grid.RR <- df.grid.DE[df.grid.DE$AGS %in% ind.RR, ]

popDens2 <- rep(NA, nrow(df.grid.RR))

spdf.grid.RR <- SpatialPoints(coords = cbind(df.grid.RR$Lon.WGS84, df.grid.RR$Lat.WGS84),
                              proj4string = CRS(WGS84))

spdf.grid.RR.GK3 <- spTransform(spdf.grid.RR, GK3)

radius <- 3000 # Choose 1000, 2000, or 3000

for(i in 1:nrow(df.grid.RR)){
  list  <- extract(r.popDens.NRW,
                   coordinates(spdf.grid.RR.GK3[i,]),
                   buffer = radius)
  nr.cells <- length(list[[1]][list[[1]]>0])
  if(nr.cells > 0){ 
    popDens2[i] <- sum(list[[1]][list[[1]]>0])/nr.cells
  }
}



# Add column with population density based on Zensus Atlas data where buffer radius 3km is chosen
df.grid.RR$popDens_Atlas_1kmBuffer <- popDens2
df.grid.RR$popDens_Atlas_2kmBuffer <- popDens2
df.grid.RR$popDens_Atlas_3kmBuffer <- popDens2


# rm(list=(ls()[ls()!="df.grid.RR"]))
# save.image("DataFull/Data_built/grid.RR.RData")