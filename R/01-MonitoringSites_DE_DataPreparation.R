##########################################################################
### Derive values of predictor variables for background sites in Germany
##########################################################################


rm(list = ls())


#setwd("E:/Work/20_Projekte/570_Behm-and-Haupt/R/10_data")
#setwd("D:/Markus/Work/20_Projekte/570_Behm-and-Haupt/R/10_data")



# Load packages ----
library(rgdal)
library(xlsx)
library(raster)
library(rgeos)
library(sp)


# EEA: AQeReporting data ----
df.meta <- read.csv("../DATA/DE_AQeReporting_2013-2015/DE_2013-2015_metadata.csv",
                    sep = "\t", encoding = "UTF-8")
df.NO2 <- read.csv("../DATA/DE_AQeReporting_2013-2015/DE_8_2013-2015_aggregated_timeseries.csv",
                   sep = "\t", encoding = "UTF-8")

# Choose Air Pollutant of interest
pollutant <- "NO2"
df.meta <- df.meta[df.meta$AirPollutant==pollutant,]

# Subset to sites contained in df.NO2
df.meta <- df.meta[df.meta$AirQualityStationEoICode%in%unique(df.NO2$AirQualityStationEoICode),]

# Choose Aggregation Process
AggProcess <- "P1Y" # annual mean
df.NO2 <- df.NO2[df.NO2$DataAggregationProcess==AggProcess,]

# Merge the two data frames
names(df.meta)[which(names(df.meta)%in%names(df.NO2))]
which(names(df.meta)%in%names(df.NO2))
df.NO2.meta <- merge(df.NO2,df.meta[,-c(1,3,4,5,8,11,12)],by="AirQualityStationEoICode")
# 53 passive sampler extra -> number of observations rises from 1022 to 1075

# Focus on background sites only
#df.Background <- df.NO2.meta[df.NO2.meta$AirQualityStationType=="background",]

unique(droplevels(df.NO2.meta$DatetimeBegin))
unique(droplevels(df.NO2.meta$DatetimeEnd))

# Focus on one year only
df.NO2.meta <- df.NO2.meta[df.NO2.meta$DatetimeBegin=="2015-01-01 00:00:00",]


# Identify duplicates
df.NO2.meta$AirQualityStationEoICode[duplicated(df.NO2.meta$AirQualityStationEoICode)]

summary(df.NO2.meta$MeasurementType)
#  active automatic   passive
#       0       403       18
#
# Remove duplicates
df.NO2.meta <- df.NO2.meta[df.NO2.meta$MeasurementType=="automatic",]



# GADM: Boundaries of Germany and its federal states ----
sPdf.boundaries.DE <- readOGR(dsn = "../DATA/Data_GADM", layer = "DEU_adm1")


# EEA:CLC2012 layer ----
first10.CLC12 <- readGDAL("../DATA/Data_CLC12/g100_clc12_V18_5a/g100_clc12_V18_5.tif",
                          region.dim = c(10,10))

sPdf.boundaries.DE <- spTransform(sPdf.boundaries.DE, CRS = proj4string(first10.CLC12))


bbox(sPdf.boundaries.DE)
#       min     max
# x 4031313 4672526
# y 2684076 3551246
bbox(first10.CLC12)
#        min      max
# x -2700000 -2699000
# y  5499000  5500000


sGdf.CLC12 <- readGDAL("../DATA/Data_CLC12/g100_clc12_V18_5a/g100_clc12_V18_5.tif",
                       offset = c(19480,67313),
                       region.dim = c(8700,6500))
# offset: Number of rows and columns from the origin (usually the upper left corner) to begin reading from; presently ordered (y,x) - this may change
# region.dim: The number of rows and columns to read from the dataset; presently ordered (y,x) - this may change
# 100x100m cells
# build difference between x.min of bbox(spdf.boundaries.DE) and x.min of sGdf.CLC12
# divide the difference by 100 (units=m)
# do the same with y.max
# this gives the offset
# for region.dim build difference x.min and x.max of bbox(spdf.AirBase.stations.DE.new)
# divide the difference by 100
# do the same with y.min and y.max of bbox(spdf.AirBase.stations.DE.new)
# this gives region.dim
# note that the order in offset and region.dim is always (y,x)
# and that the first cell in sGdf.CLC06 is the upper left corner corresponding to
# (y.max, x.min) of bbox(sGdf.CLC06)
# unfortunately sGdf.CLC06 cannot be read into R as it is too large
# some trial and error is required!



# Write function to calculate shares of grouped land use classes within buffer of radius r

# Building buffers around monitoring sites with 1km, 5km and 21km and calculate shares of 10
# grouped land use classes relying on Beelen et al.(2009)
df.NO2.meta <- cbind(df.NO2.meta,data.frame(matrix(data = 0, nrow = nrow(df.NO2.meta), ncol = 10)))
names(df.NO2.meta)[37:46] <- paste(rep("Class", n = 10), seq(from = 1, to = 10, by = 1), sep = "")



## Write data frame that relates the 44 CLC classes to the 10 grouped classes in Beelen et al. (2009), p.1854-1855
df.CLC.grouped <- data.frame(matrix(data = 0, nrow = 10, ncol = 3))
names(df.CLC.grouped) <- c("GroupedClass", "Description", "CLCclasses")
df.CLC.grouped$GroupedClass <- 1:10
df.CLC.grouped$Description <- c("High density residential","Low density residential", "Industry", "Transport",
                                "Seaports", "Airports", "Construction","Urban Greenery", "Agriculture", "Forest")
df.CLC.grouped$CLCclasses <- c(as.character(1:6),"7-9","10-11","12-22","23-25")


# Transform SpatialGridDataFrame to raster
raster.CLC12 <- raster(sGdf.CLC12)


unique(df.NO2.meta$Projection)
# [1] EPSG:4979
# +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs

sp.sites <- SpatialPoints(coords = df.NO2.meta[,c("Longitude","Latitude")],
                               proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
sp.sites <- spTransform(sp.sites,proj4string(raster.CLC12))

radius <- 1000

for(i in 1:nrow(df.NO2.meta)){
  list  <- extract(raster.CLC12,
                   coordinates(sp.sites[i,]),
                   buffer=radius)

  nr.cells <- length(list[[1]])

  df.NO2.meta$Class1[i] <- sum(list[[1]]==1)/nr.cells
  df.NO2.meta$Class2[i] <- sum(list[[1]]==2)/nr.cells
  df.NO2.meta$Class3[i] <- sum(list[[1]]==3)/nr.cells
  df.NO2.meta$Class4[i] <- sum(list[[1]]==4)/nr.cells
  df.NO2.meta$Class5[i] <- sum(list[[1]]==5)/nr.cells
  df.NO2.meta$Class6[i] <- sum(list[[1]]==6)/nr.cells
  df.NO2.meta$Class7[i] <- sum(list[[1]]%in%c(7:9))/nr.cells
  df.NO2.meta$Class8[i] <- sum(list[[1]]%in%c(10:11))/nr.cells
  df.NO2.meta$Class9[i] <- sum(list[[1]]%in%c(12:22))/nr.cells
  df.NO2.meta$Class10[i] <- sum(list[[1]]%in%c(23:25))/nr.cells
}




# BKG (Bundesamt für Kartographie und Geodäsie) ----
# German administrative regions
admin.regions.2015 <- readOGR(dsn = "../DATA/Data_BKG/vg250-ew_ebenen",
                              layer = "VG250_GEM",
                              encoding = "UTF-8",
                              use_iconv = TRUE)


# BBSR (Bundesinstitut für Bau-, Stadt- und Raumforschung) ----
# Urban-rural typology of the German administrative regions
# data 'Gem15-Raumtyp.xlsx' received via E-Mail from Petra Kuhlmann on 19th July 2017
# Raumtypen beziehen sich auf die Raumordnung von 2010
# Die Zuordnung zu den Gemeinden bezieht sich auf den Gemeindestand 2015-12-31
df.area2015 <- read.xlsx("../DATA/Data_BBSR/Gem15-Raumtyp.xlsx",
                         header = TRUE,
                         sheetIndex = 1,
                         startRow = 3,
                         endRow = 11169,
                         colIndex = seq(1,7,1),
                         encoding = "UTF-8")

df.popDens2015 <- read.xlsx("../DATA/Data_BBSR/Gem15-Raumtyp.xlsx",
                            header = TRUE,
                            sheetIndex = 2,
                            startRow = 3,
                            endRow = 4541,
                            colIndex = seq(1,6,1),
                            encoding = "UTF-8")


# Exploring and adjusting data
names(df.area2015)
names(df.popDens2015)

names(admin.regions.2015)
# an overview over the attributes is given in the Pdf "Produktdokumentation VG250"
# for our analysis the following variables are relevant
# RS:
# Regionalschlüssel (RS) gliedert sich wie folgt:
# -> 1.-2. digit = identification number of the federal state
# -> 3.digit = identification number of the administrative district (Regierungsbezirk)
# -> 4.–5.digit = identification number of the district or county (Kreis)
# -> 6.–9.digit = identification number of the administrative association (Verwaltungsgemeinschaft)
# -> 10.–12.digit = community identification number (Gemeinde)
# Note: In case the 6th digit is '9' it refers to a municipality not adhering to an administrative association
# Then the municipality is associated with the degree of urbanisation 'rural'

# AGS: official municipality key (Amtlicher Gemeindeschlüssel)
# -> 1.-2. digit: identification number of the federal state
# -> 3. digit: identification number of the administrative district (Regierungsbezirk),
# -> 4.-5. digit: identification number of the district or county (Kreis)
# -> 6.-8. digit: community identification number (Gemeinde)

# GEN: Geografischer Name
# NUTS: Europäischer Statistikschlüssel

admin.regions.2015.sub <- admin.regions.2015[,names(admin.regions.2015) %in% c("RS", "AGS", "GEN", "NUTS")]

dummy.tmp <- nchar(df.area2015$gem15) == 7
df.area2015$gem15_full[dummy.tmp] <- paste(0,df.area2015$gem15[dummy.tmp],sep="")
df.area2015$gem15_full[!dummy.tmp] <- df.area2015$gem15[!dummy.tmp]
all(admin.regions.2015.sub$AGS %in% df.area2015$gem15_full)
test2 <- admin.regions.2015.sub$AGS %in% df.area2015$gem15_full
table(test2)
# there are 185 municipalities for which no entry is available in `df.area`
x1 <- admin.regions.2015.sub[!test2,]@data
#View(x1)


# Data\Data_BKG\dokumentation\vg250.pdf
# search for 'gemeindefrei' -> Regionalschlüssel (RS) 6.Stelle
x1$check9 <- sapply(as.vector(x1[,1]), function(x) substring(x, 6,6))

# sort for 'check9'
# add to all municipalities except 'Berlin' and 'Hamburg' attribute 'rural'
admin.regions.2015.sub$digit6 <- sapply(as.vector(admin.regions.2015.sub$RS), function(x) substring(x,6,6))


regions.2015.complete <- merge(admin.regions.2015.sub,df.area2015,
                               by.y = "gem15_full", by.x = "AGS", all.x = TRUE)



regions.2015.complete$raumt2010besiedlung[regions.2015.complete$digit6==9] <- "überwiegend ländlich"

# Add manually the information for 'Berlin' and 'Hamburg' contained in in dfarea.2015
df.area2015[df.area2015$name15%in%c("Berlin, Stadt", "Hamburg, Freie und Hansestadt"),1:7]
View(regions.2015.complete@data[is.na(regions.2015.complete$raumt2010besiedlung),])

regions.2015.complete@data[is.na(regions.2015.complete$raumt2010besiedlung),6:12] <- df.area2015[c(1113,1113,8497),1:7]

# Are there any more NA's?
check.na <- is.na(regions.2015.complete$raumt2010besiedlung)
table(check.na)
# No! -> to every polygon in 'admin.regions.2015.sub' the corresponding degree of urbanisation can be attributed

levels(regions.2015.complete$raumt2010besiedlung)
levels(regions.2015.complete$raumt2010besiedlung) <- c("suburban", "rural", "urban")


proj4string(regions.2015.complete)
proj4string(sp.sites)


df.NO2.meta2 <- cbind(df.NO2.meta,over(spTransform(sp.sites,proj4string(regions.2015.complete)),regions.2015.complete))
summary(df.NO2.meta2$AGS)
df.NO2.meta2[is.na(df.NO2.meta2$AGS),]
# Search on the internet for monitoring sites "DESN024" and "DEMV031" -> they are located in Klingenthal and Rostock, respectively
# Search on the internet for the AGS of Klingenthal and Rostock -> attributed to "14523160" and "13003000", respectively
# Why are they not attributed to a polygon in "regions.2015.complete"?
plot(regions.2015.complete[regions.2015.complete$AGS=="14523160",])
plot(regions.2015.complete[regions.2015.complete$AGS=="13003000",])
which(is.na(df.NO2.meta2$AGS))
# [1] 200 340
plot(spTransform(sp.sites[200,],proj4string(regions.2015.complete)),col="red",pch=16,cex=1,add=TRUE)
# Because it is on the border of the respective polygon...
# Add the information manually
names(df.NO2.meta2)
names(regions.2015.complete)
df.NO2.meta2[df.NO2.meta2$AirQualityStationEoICode=="DESN024",47:58] <- regions.2015.complete@data[regions.2015.complete$AGS=="14523160",]
df.NO2.meta2[df.NO2.meta2$AirQualityStationEoICode=="DEMV031",47:58] <- regions.2015.complete@data[regions.2015.complete$AGS=="13003000",][-1,]



summary(duplicated(df.popDens2015$vbgem15)) # unambiguously identified
df.NO2.meta3 <- merge(df.NO2.meta2, df.popDens2015[,c(1,3,4)], by="vbgem15",all.x=TRUE)
df.NO2.meta3$popDens15 <- df.NO2.meta3$z_bev15/df.NO2.meta3$fl15_sum





# Create indicator vector for filtering AGS referring to Rhine-Ruhr region
names(regions.2015.complete)
unique(regions.2015.complete$AGS)

regions.2015.NRW <- subset(regions.2015.complete, substr(regions.2015.complete$AGS, 1,2 ) == "05")
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
#saveRDS(object = ind.RR, file = "smoothLUR/indRhineRuhr.rds")




# EuroGeographics: Traffic information ----
# data retrieved from http://www.eurogeographics.org/content/euroglobalmap-opendata on 25th April 2018
sLdf.roads.DE <- readOGR(dsn = "../DATA/Data_EuroGeographics/EGM_10-1-0SHP_20171110/DATA/Countries/DE", layer = "RoadL",
                         encoding = "UTF-8",use_iconv = TRUE)
class(sLdf.roads.DE)

proj4string(sLdf.roads.DE)


# FCsubtype: name of the feature type

# F_CODE: The Feature CODE using the DIGEST coding,
# i.e. "AP030" identifying the road feature.

# ICC: two-character country code
# LLE: location level
# LTN: lane/track number
# RTN: route number (national)
# RTT: route intended use -> 0: unknown; 14: primary route; 15: secondary route; 16: national route; 984: local route
# the latter variable is most important w.r.t. to the NO_2 concentration

sp.sites3 <- SpatialPoints(coords = df.NO2.meta3[,c("Longitude","Latitude")],
                                proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
sp.sites3 <- spTransform(sp.sites3,proj4string(raster.CLC12))

# Test
B279 <- subset(sLdf.roads.DE, RTN == "B279")
par(mar = c(0,0,0,0))
# ID_1==7 for Hessen
plot(spTransform(subset(sPdf.boundaries.DE,ID_1==7),CRS=proj4string(sLdf.roads.DE)), border = "grey", lwd = 1.5)
plot(spTransform(sPdf.boundaries.DE,CRS=proj4string(sLdf.roads.DE)), border = "grey", lwd = 1.5)
plot(B279, col = "blue", add = TRUE)
points(coordinates(spTransform(sp.sites3,CRS=proj4string(sLdf.roads.DE))),pch=16,cex=0.2)
plot(spTransform(gBuffer(sp.sites3[1,],width=5000),CRS=proj4string(sLdf.roads.DE)),add=TRUE,col="red")



spdf.sites3 <- SpatialPointsDataFrame(sp.sites3,data=df.NO2.meta3)
BufferSites <- gBuffer(spdf.sites3, byid = TRUE,
                       id = spdf.sites3$AirQualityStationEoICode,
                       width = radius)


# Note difference between gIntersects and gContains
# gIntersects: buffer and line segment have at least one point in common
# gContains: buffer contains line segment

Test.RoadSubset <- gIntersects(spTransform(BufferSites,proj4string(sLdf.roads.DE)),sLdf.roads.DE,byid = TRUE)
dim(Test.RoadSubset)
# [1] 62448   403
# -> columns are related to buffers around monitoring sites
# -> rows are related to elements in sLdf.roads.DE


# Sum up the line segments intersecting with the buffer for each monitoring site distinguishing between the road types
df.NO2.meta3$Road14 <- 0
df.NO2.meta3$Road15 <- 0
df.NO2.meta3$Road16 <- 0
df.NO2.meta3$Road984 <- 0


for(i in 1:nrow(df.NO2.meta3)){
  sLdf.tmp <- subset(sLdf.roads.DE,
                     row.names(sLdf.roads.DE)%in%row.names(sLdf.roads.DE)[Test.RoadSubset[,i]])
  if(nrow(sLdf.tmp)>0){
    sLdf.tmp2 <- crop(spTransform(sLdf.tmp, proj4string(BufferSites)), BufferSites[i,])
    sLdf.tmp2.14 <- subset(sLdf.tmp2, sLdf.tmp2$RTT == 14)
    sLdf.tmp2.15 <- subset(sLdf.tmp2, sLdf.tmp2$RTT == 15)
    sLdf.tmp2.16 <- subset(sLdf.tmp2, sLdf.tmp2$RTT == 16)
    sLdf.tmp2.984 <- subset(sLdf.tmp2, sLdf.tmp2$RTT == 984)
    if(nrow(sLdf.tmp2.14)>0){
      df.NO2.meta3$Road14[i] <- sum(SpatialLinesLengths(sLdf.tmp2.14))
    }
    if(nrow(sLdf.tmp2.15)>0){
      df.NO2.meta3$Road15[i] <- sum(SpatialLinesLengths(sLdf.tmp2.15))
    }
    if(nrow(sLdf.tmp2.16)>0){
      df.NO2.meta3$Road16[i] <- sum(SpatialLinesLengths(sLdf.tmp2.16))
    }
    if(nrow(sLdf.tmp2.984)>0){
      df.NO2.meta3$Road984[i] <- sum(SpatialLinesLengths(sLdf.tmp2.984))
    }
  }
}


# Choose i and check if for-loop does what it is supposed to do...
i=99

sLdf.tmp <- subset(sLdf.roads.DE,row.names(sLdf.roads.DE)%in%row.names(sLdf.roads.DE)[Test.RoadSubset[,i]])
sLdf.tmp2 <- crop(spTransform(sLdf.tmp,proj4string(BufferSites)),BufferSites[i,])

sLdf.tmp2@data$RTT

sLdf.tmp2.14 <- subset(sLdf.tmp2, sLdf.tmp2$RTT==14)
sLdf.tmp2.15 <- subset(sLdf.tmp2, sLdf.tmp2$RTT==15)
sLdf.tmp2.16 <- subset(sLdf.tmp2, sLdf.tmp2$RTT==16)
sLdf.tmp2.984 <- subset(sLdf.tmp2, sLdf.tmp2$RTT==984)


par(mar=c(4,4,4,4))
plot(BufferSites[i,])
plot(sp.sites3[i,],add=TRUE)
plot(spTransform(sLdf.tmp,proj4string(BufferSites)),col="blue",add=TRUE,lwd=2)
plot(sLdf.tmp2.14,col="red",add=TRUE,lty=2,lwd=3)



LinesLength(sLdf.tmp2@lines[[1]]) + LinesLength(sLdf.tmp2@lines[[2]])
sum(SpatialLinesLengths(sLdf.tmp2.14))
sum(SpatialLinesLengths(sLdf.tmp2))
sum(SpatialLinesLengths(spTransform(sLdf.tmp,proj4string(sLdf.tmp2))))



## Derive distance of each monitroing site to next road
sLdf.roads.DE2 <- spTransform(sLdf.roads.DE, proj4string(sp.sites3))

sLdf.roads.DE2.14 <- subset(sLdf.roads.DE2, sLdf.roads.DE2$RTT == 14)
sLdf.roads.DE2.15 <- subset(sLdf.roads.DE2, sLdf.roads.DE2$RTT == 15)
sLdf.roads.DE2.16 <- subset(sLdf.roads.DE2, sLdf.roads.DE2$RTT == 16)
sLdf.roads.DE2.984 <- subset(sLdf.roads.DE2, sLdf.roads.DE2$RTT == 984)

dat.ProximityToRoads <- data.frame(ID = df.NO2.meta3$AirQualityStationEoICode,
                                   Type = df.NO2.meta3$AirQualityStationType,
                                   Lon.WGS84 = df.NO2.meta3$Longitude,
                                   Lat.WGS84 = df.NO2.meta3$Latitude,
                                   DistPri = rep(NA, nrow(df.NO2.meta3)),
                                   DistSec = rep(NA, nrow(df.NO2.meta3)),
                                   DistMot = rep(NA, nrow(df.NO2.meta3)),
                                   DistLoc = rep(NA, nrow(df.NO2.meta3)))



for(i in 1:nrow(df.NO2.meta3)){
  dat.ProximityToRoads$DistPri[i] <- gDistance(sp.sites3[i,], sLdf.roads.DE2.14)
  dat.ProximityToRoads$DistSec[i] <- gDistance(sp.sites3[i,], sLdf.roads.DE2.15)
  dat.ProximityToRoads$DistMot[i] <- gDistance(sp.sites3[i,], sLdf.roads.DE2.16)
  dat.ProximityToRoads$DistLoc[i] <- gDistance(sp.sites3[i,], sLdf.roads.DE2.984)
}


dat.ProximityToRoads[,5:8] <- round(dat.ProximityToRoads[,5:8], 2)

write.csv(dat.ProximityToRoads, file = "R/DATA/Data_built/dat_ProximityToRoads_background.csv")


# Choose i and check if for-loop does what it is supposed to do...
i <- 33
Buffer.tmp <- gBuffer(sp.sites3[i, ], width = 7000)
sLdf.tmp2 <- crop(sLdf.roads.DE2, Buffer.tmp)
sLdf.tmp2.14 <- subset(sLdf.tmp2, sLdf.tmp2$RTT==14)
sLdf.tmp2.15 <- subset(sLdf.tmp2, sLdf.tmp2$RTT==15)
sLdf.tmp2.16 <- subset(sLdf.tmp2, sLdf.tmp2$RTT==16)
sLdf.tmp2.984 <- subset(sLdf.tmp2, sLdf.tmp2$RTT==984)


par(mar=c(4,4,4,4))
plot(Buffer.tmp)
plot(sp.sites3[i,],add=TRUE)
plot(sLdf.tmp2.14,col="blue",add=TRUE,lwd=2)
plot(sLdf.tmp2.15,col="red",add=TRUE,lty=2,lwd=3)
plot(sLdf.tmp2.16,col="green",add=TRUE,lty=2,lwd=4)
plot(sLdf.tmp2.984,col="orange",add=TRUE,lty=2,lwd=5)
# -> Compare with GoogleMaps





# Select columns required for analysis ----
names(df.NO2.meta3)
df.analysis <- df.NO2.meta3[,c(2,11,24,26:29,31:32,35,38:48,50:51,57:58,61:65)]
df.analysis$ObservationDateBegin = 2015

# EoI: Exchange on Information
names(df.analysis)[c(1:3,5:20,24:30)] <- c("AQeCode", "AQeYMean", "Year", "AQeLon", "AQeLat", "AQeAlt",
                                           "AQeType", "AQeArea", "AQeInletHeight",
                                           "HighDens", "LowDens", "Ind", "Transp", "Seap",
                                           "Airp", "Constr", "UrbGreen", "Agri", "Forest",
                                           "BBSRArea","BBSRArea2","BBSRpopDens",
                                           "PriRoad", "SecRoad", "NatMot", "LocRoute")

write.csv(df.analysis, file = "R/DATA/Data_built/DATA_MonitoringSites_DE.csv")
