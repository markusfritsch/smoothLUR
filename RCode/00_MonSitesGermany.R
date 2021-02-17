##########################################################################
### Derive values of predictor variables for monitoring sites in Germany
##########################################################################


rm(list = ls())


# Load packages ----
library(rgdal)
library(xlsx)
library(raster)
library(rgeos)
library(sp)


# EEA: AQeReporting data ----
df.meta <- read.csv("data/DE_AQeReporting_2013-2015/DE_2013-2015_metadata.csv",
                    sep = "\t", encoding = "UTF-8")
df.NO2 <- read.csv("data/DE_AQeReporting_2013-2015/DE_8_2013-2015_aggregated_timeseries.csv",
                   sep = "\t", encoding = "UTF-8")


# Choose Air Pollutant of interest
df.meta <- df.meta[df.meta$AirPollutant=="NO2",]

# Subset sites contained in df.NO2
df.meta <- df.meta[df.meta$AirQualityStationEoICode%in%unique(df.NO2$AirQualityStationEoICode),]

# Choose Aggregation Process
df.NO2 <- df.NO2[df.NO2$DataAggregationProcess=="P1Y",]   # annual mean

# Merge the two data frames
names(df.meta)[which(names(df.meta)%in%names(df.NO2))]
which(names(df.meta)%in%names(df.NO2))
df.NO2.meta <- merge(df.NO2,df.meta[,-c(1,3,4,5,8,11,12)],by="AirQualityStationEoICode")
# 53 passive sampler extra -> number of observations rises from 1022 to 1075

table(df.NO2.meta$MeasurementType)
# automatic   passive
#      10022       53
#
# Remove duplicates
df.NO2.meta <- df.NO2.meta[df.NO2.meta$MeasurementType=="automatic",]

# Focus on one year only
df.NO2.meta <- df.NO2.meta[df.NO2.meta$DatetimeBegin=="2015-01-01 00:00:00",]

#For ease of clarity filter required columns..
names(df.NO2.meta)
df.NO2.meta <- df.NO2.meta[,c(1,10,15,25,26,27,28,30,31)]
#...and rename them
names(df.NO2.meta) <- c("AQeCode","Y","Year","Projection","Lon","Lat","Alt","AQeType","AQeArea")
df.NO2.meta$Year <- 2015


# Data on land use; EEA:CLC2012 layer ----
GDALinfo("data/Data_CLC12/g100_clc12_V18_5a/g100_clc12_V18_5.tif")
sGdf.CLC12 <- readGDAL("data/Data_CLC12/g100_clc12_V18_5a/g100_clc12_V18_5.tif",
                       offset = c(19480,67313),
                       region.dim = c(8700,6500))
# CRS Name: EUR_ETRS89/LAEA1052 corresponds to EPSG:3035

proj4string(sGdf.CLC12)
WKT <- wkt(sGdf.CLC12)

# Calculate shares of grouped land use classes within buffer of radius r

# Building buffers around monitoring sites with 1km and calculate shares of 10
# grouped land use classes relying on Beelen et al.(2009)
df.NO2.meta <- cbind(df.NO2.meta,data.frame(matrix(data = 0, nrow = nrow(df.NO2.meta), ncol = 10)))
names(df.NO2.meta)[10:19] <- c("HighDens", "LowDens", "Ind", "Transp", "Seap",
                               "Airp", "Constr", "UrbGreen", "Agri", "Forest")

# Transform SpatialGridDataFrame to raster
raster.CLC12 <- raster(sGdf.CLC12)

unique(df.NO2.meta$Projection)
# [1] EPSG:4979
# +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs

sp.sites <- SpatialPoints(coords = df.NO2.meta[,c("Lon","Lat")],
                          proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
sp.sites <- spTransform(sp.sites,proj4string(raster.CLC12))

radius <- 1000

for(i in 1:nrow(df.NO2.meta)){
  list  <- extract(raster.CLC12,
                   coordinates(sp.sites[i,]),
                   buffer=radius)

  nr.cells <- length(list[[1]])

  df.NO2.meta$HighDens[i] <- sum(list[[1]]==1)/nr.cells
  df.NO2.meta$LowDens[i]  <- sum(list[[1]]==2)/nr.cells
  df.NO2.meta$Ind[i]      <- sum(list[[1]]==3)/nr.cells
  df.NO2.meta$Transp[i]   <- sum(list[[1]]==4)/nr.cells
  df.NO2.meta$Seap[i]     <- sum(list[[1]]==5)/nr.cells
  df.NO2.meta$Airp[i]     <- sum(list[[1]]==6)/nr.cells
  df.NO2.meta$Constr[i]   <- sum(list[[1]]%in%c(7:9))/nr.cells
  df.NO2.meta$UrbGreen[i] <- sum(list[[1]]%in%c(10:11))/nr.cells
  df.NO2.meta$Agri[i]     <- sum(list[[1]]%in%c(12:22))/nr.cells
  df.NO2.meta$Forest[i]   <- sum(list[[1]]%in%c(23:25))/nr.cells
}




# Data on population density; BKG (Bundesamt für Kartographie und Geodäsie) ----
# German administrative regions at municipality level
sPdf.Municipalities <- readOGR(dsn = "data/Data_BKG/vg250-ew_ebenen",
                               layer = "VG250_GEM",
                               encoding = "UTF-8",
                               use_iconv = TRUE)
names(sPdf.Municipalities)
# for our analysis the following variables are relevant
# AGS: official municipality key (Amtlicher Gemeindeschlüssel)
# -> 1.-2. digit: identification number of federal state
# -> 3. digit: identification number of administrative district (Regierungsbezirk),
# -> 4.-5. digit: identification number of district or county (Kreis)
# -> 6.-8. digit: community identification number (Gemeinde)
# EWZ: number of inhabitants
sPdf.Municipalities <- sPdf.Municipalities[,c("AGS", "EWZ")]

# Calculate area of each polygon in 'sPdf.Municipalities' and add column to 'sPdf.Municipalities@data'
proj4string(sPdf.Municipalities) # units=m
# gArea() returns area in m^2, divide by 1000*1000 to get area in km^2
sPdf.Municipalities$Area_km2 <- gArea(sPdf.Municipalities, byid = TRUE) / 1000000

sPdf.Municipalities$PopDens <- as.numeric(sPdf.Municipalities$EWZ) / sPdf.Municipalities$Area_km2
summary(sPdf.Municipalities$PopDens)

proj4string(sPdf.Municipalities)
proj4string(sp.sites)

df.NO2.meta2 <- cbind(df.NO2.meta, over(spTransform(x = sp.sites,CRSobj = CRS(proj4string(sPdf.Municipalities))),
                                       spTransform(x = sPdf.Municipalities, CRSobj = CRS(proj4string(sPdf.Municipalities)))))
summary(df.NO2.meta2$PopDens)
df.NO2.meta2[is.na(df.NO2.meta2$PopDens),]
# Search on the internet for monitoring sites "DESN024" and "DEMV031" -> they are located in Klingenthal and Rostock, respectively
# Search on the internet for the AGS of Klingenthal and Rostock -> attributed to "14523160" and "13003000", respectively
# Why are they not attributed to a polygon in "regions.2015"?
plot(sPdf.Municipalities[sPdf.Municipalities$AGS=="13003000",])
which(is.na(df.NO2.meta2$AGS))
# [1] 200 340
plot(spTransform(sp.sites[200,],proj4string(sPdf.Municipalities)),col="red",pch=16,cex=1,add=TRUE)
# Because it is on the border of the respective polygon...
# Add the information manually
names(df.NO2.meta2)
names(sPdf.Municipalities)
df.NO2.meta2[df.NO2.meta2$AQeCode=="DESN024",20:23] <- sPdf.Municipalities@data[sPdf.Municipalities$AGS=="14523160",]
df.NO2.meta2[df.NO2.meta2$AQeCode=="DEMV031",20:23] <- sPdf.Municipalities@data[sPdf.Municipalities$AGS=="13003000",][-1,]

# Drop columns no longer required...
df.NO2.meta2 <- df.NO2.meta2[,-c(21,22)]



# Create indicator vector for filtering AGS referring to Rhine-Ruhr region
names(sPdf.Municipalities)
unique(sPdf.Municipalities$AGS)

regions.2015.NRW <- subset(sPdf.Municipalities, substr(sPdf.Municipalities$AGS, 1,2 ) == "05")
k.ME <- as.character(regions.2015.NRW$AGS[substr(regions.2015.NRW$AGS, 3, 5) == "158"])
k.UN <- as.character(regions.2015.NRW$AGS[substr(regions.2015.NRW$AGS, 3, 5) == "978"])
k.EN <- as.character(regions.2015.NRW$AGS[substr(regions.2015.NRW$AGS, 3, 5) == "954"])[-4]
k.MK <- as.character(regions.2015.NRW$AGS[substr(regions.2015.NRW$AGS, 3, 5) == "962"])[c(4,8,13)]
k.RE <- as.character(regions.2015.NRW$AGS[substr(regions.2015.NRW$AGS, 3, 5) == "562"])[-3]
k.BM <- as.character(regions.2015.NRW$AGS[substr(regions.2015.NRW$AGS, 3, 5) == "362"])[c(2,3,6,7)]
k.NE <- as.character(regions.2015.NRW$AGS[substr(regions.2015.NRW$AGS, 3, 5) == "162"])[-c(1,6)]
k.SU <- as.character(regions.2015.NRW$AGS[substr(regions.2015.NRW$AGS, 3, 5) == "382"])[c(6,7,11,12, 15,19)]
k.GL <- as.character(regions.2015.NRW$AGS[substr(regions.2015.NRW$AGS, 3, 5) == "378"])[c(1,2,4)]
k.VIE <- as.character(regions.2015.NRW$AGS[substr(regions.2015.NRW$AGS, 3, 5) == "166"])[c(1,5,7,8)]
k.WES <- as.character(regions.2015.NRW$AGS[substr(regions.2015.NRW$AGS, 3, 5) == "170"])[c(2, 4:6, 8:10)]

cities <- as.character(regions.2015.NRW$AGS[substr(regions.2015.NRW$AGS, 6, 8) %in% c("000")])[-c(3,16)]

ind.RR <- c(k.ME, k.UN, k.EN, k.MK, k.RE, k.BM, k.NE, k.SU, k.GL, k.VIE, k.WES, cities)
#saveRDS(object = ind.RR, file = "indRhineRuhr.rds")




# Data on traffic; EuroGeographics ----
sLdf.Roads.DE <- readOGR(dsn = "data/Data_EuroGeographics/EGM_10-1-0SHP_20171110/DATA/Countries/DE", layer = "RoadL",
                         encoding = "UTF-8",use_iconv = TRUE)
class(sLdf.Roads.DE)
proj4string(sLdf.Roads.DE)
names(sLdf.Roads.DE)
# RTT: route intended use -> 0: unknown; 14: primary route; 15: secondary route; 16: national route; 984: local route

spdf.sites2 <- SpatialPointsDataFrame(coords = df.NO2.meta2[,c("Lon","Lat")],
                                      proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"),
                                      data=df.NO2.meta2)
spdf.sites2 <- spTransform(spdf.sites2,proj4string(raster.CLC12))
BufferSites <- gBuffer(spdf.sites2, byid = TRUE,
                       id = spdf.sites2$AirQualityStationEoICode,
                       width = radius)


# Note difference between gIntersects and gContains
# gIntersects: buffer and line segment have at least one point in common
# gContains: buffer contains line segment

Test.RoadSubset <- gIntersects(spTransform(x = BufferSites,CRSobj = CRS(proj4string(sLdf.Roads.DE))),
                               spTransform(x = sLdf.Roads.DE, CRSobj = CRS(proj4string(sLdf.Roads.DE))),byid = TRUE)
dim(Test.RoadSubset)
# [1] 62448   403
# -> columns are related to buffers around monitoring sites
# -> rows are related to elements in sLdf.Roads.DE


# Sum up the line segments intersecting with the buffer for each monitoring site distinguishing between the road types
df.NO2.meta2$PriRoad <- 0
df.NO2.meta2$SecRoad <- 0
df.NO2.meta2$FedAuto <- 0
df.NO2.meta2$LocRoute <- 0


for(i in 1:nrow(df.NO2.meta2)){
  sLdf.tmp <- subset(sLdf.Roads.DE,
                     row.names(sLdf.Roads.DE)%in%row.names(sLdf.Roads.DE)[Test.RoadSubset[,i]])
  if(nrow(sLdf.tmp)>0){
    sLdf.tmp2 <- crop(spTransform(sLdf.tmp, proj4string(BufferSites)), BufferSites[i,])
    sLdf.tmp2.14 <- subset(sLdf.tmp2, sLdf.tmp2$RTT == 14)
    sLdf.tmp2.15 <- subset(sLdf.tmp2, sLdf.tmp2$RTT == 15)
    sLdf.tmp2.16 <- subset(sLdf.tmp2, sLdf.tmp2$RTT == 16)
    sLdf.tmp2.984 <- subset(sLdf.tmp2, sLdf.tmp2$RTT == 984)
    if(nrow(sLdf.tmp2.14)>0){
      df.NO2.meta2$PriRoad[i] <- sum(SpatialLinesLengths(sLdf.tmp2.14))
    }
    if(nrow(sLdf.tmp2.15)>0){
      df.NO2.meta2$SecRoad[i] <- sum(SpatialLinesLengths(sLdf.tmp2.15))
    }
    if(nrow(sLdf.tmp2.16)>0){
      df.NO2.meta2$FedAuto[i] <- sum(SpatialLinesLengths(sLdf.tmp2.16))
    }
    if(nrow(sLdf.tmp2.984)>0){
      df.NO2.meta2$LocRoute[i] <- sum(SpatialLinesLengths(sLdf.tmp2.984))
    }
  }
}



# Add column of type factor indicating the municipality; this is required for stratified sampling in validation
df.NO2.meta2$IndRegions <- NA
df.NO2.meta2$IndRegions[nchar(df.NO2.meta2$AGS) == 7] <- substr(df.NO2.meta2$AGS[nchar(df.NO2.meta2$AGS) == 7], 1, 1)
df.NO2.meta2$IndRegions[nchar(df.NO2.meta2$AGS) != 7] <- substr(df.NO2.meta2$AGS[nchar(df.NO2.meta2$AGS) != 7], 1, 2)

write.csv(df.NO2.meta2, file = "DATA_MonitoringSites_DE.csv", row.names = FALSE)
