#####################################################################################################
### Derive values of predictor variables for traffic and industrical sites in North Rhine-Westphalia
#####################################################################################################





rm(list = ls())


#	setwd("E:/Work/20_Projekte/570_Behm-and-Haupt/R/10_data")
#	setwd("D:/Work/20_Projekte/570_Behm-and-Fritsch/smoothLUR")



# Load packages ----
library(rgdal)
library(xlsx)
library(raster)
library(GISTools)
library(gstat)


# EEA: AQeReporting data ----
df.meta <- read.csv("DataFull/DE_AQeReporting_2013-2015/DE_2013-2015_metadata.csv",
                    sep = "\t", encoding = "UTF-8")
df.NO2 <- read.csv("DataFull/DE_AQeReporting_2013-2015/DE_8_2013-2015_aggregated_timeseries.csv",
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

# Focus on non-background sites only
df.NO2.meta.sub <- df.NO2.meta[df.NO2.meta$AirQualityStationType!="background",]

#unique(droplevels(df.NO2.meta$DatetimeBegin))
#unique(droplevels(df.NO2.meta$DatetimeEnd))
unique(df.NO2.meta$DatetimeBegin)
unique(df.NO2.meta$DatetimeEnd)

# Focus on one year only
df.NO2.meta.sub <- df.NO2.meta.sub[df.NO2.meta.sub$DatetimeBegin=="2015-01-01 00:00:00",]

# Filter monitoring sites located in North Rhine-Westphalia
df.NO2.meta.NRW <- df.NO2.meta.sub[substr(df.NO2.meta.sub$AirQualityStationEoICode, 1, 4) == "DENW", ]

# Identify duplicates
df.NO2.meta.NRW$AirQualityStationEoICode[duplicated(df.NO2.meta.NRW$AirQualityStationEoICode)]
# [1] DENW247 DERP001 DERP017 DERP053
# View(df.Background)
# DENW247: passive and automatic sampling -> same annual mean
# DERP001: passive and automatic sampling -> same annual mean
# DERP017: passive and automatic sampling -> same annual mean
# DERP053: passive and automatic sampling -> same annual mean

summary(df.NO2.meta.NRW$MeasurementType)
#  active automatic   passive
#       0       29          8
#
# Remove duplicates
df.NO2.meta.NRW <- df.NO2.meta.NRW[df.NO2.meta.NRW$MeasurementType=="automatic",]

plot(df.NO2.meta.NRW$Longitude, df.NO2.meta.NRW$Latitude, cex = 0)
text(df.NO2.meta.NRW$Longitude, df.NO2.meta.NRW$Latitude, substr(df.NO2.meta.NRW$AirQualityStationEoICode, 5, 7))
# when focus lies on conurbation areas only, drop sites 181, 200, 207 and 260


rm(list = ls()[!ls() %in% c("df.NO2.meta.NRW")])


# GADM: Boundaries of Germany and its federal states ----
sPdf.boundaries.DE <- readOGR(dsn = "DataFull/Data_GADM",
                              layer = "DEU_adm1")


# boundary for North Rhine-Westphalia
sPdf.bndry.NRW <- sPdf.boundaries.DE[sPdf.boundaries.DE$ID_1 == 10,]


# EEA:CLC2012 layer ----
first10.CLC12 <- readGDAL("DataFull/Data_CLC12/g100_clc12_V18_5a/g100_clc12_V18_5.tif",
                          region.dim = c(10,10))

sPdf.bndry.NRW2 <- spTransform(sPdf.bndry.NRW, CRS = proj4string(first10.CLC12))


bbox(sPdf.bndry.NRW2)
#       min     max
# x 4031313 4283898
# y 3029642 3269981
bbox(first10.CLC12)
#        min      max
# x -2700000 -2699000
# y  5499000  5500000


sGdf.CLC12 <- readGDAL("DataFull/Data_CLC12/g100_clc12_V18_5a/g100_clc12_V18_5.tif",
                       offset = c(22300,67313),
                       region.dim = c(2404, 2526))
# offset: Number of rows and columns from the origin (usually the upper left corner) to begin reading from; presently ordered (y,x) - this may change
# region.dim: The number of rows and columns to read from the dataset; presently ordered (y,x) - this may change
# 100x100m cells
# build difference between x.min of bbox(sPdf.bndry.NRW2) and x.min of sGdf.CLC12
# divide the difference by 100 (units=m)
# do the same with y.max
# this gives the offset
# for region.dim build difference x.min and x.max of bbox(sPdf.bndry.NRW2)
# divide the difference by 100
# do the same with y.min and y.max of bbox(sPdf.bndry.NRW2)
# this gives region.dim
# note that the order in offset and region.dim is always (y,x)
# and that the first cell in sGdf.CLC12 is the upper left corner corresponding to
# (y.max, x.min) of bbox(first10.CLC12)
# unfortunately sGdf.CLC12 cannot be read into R as it is too large
# some trial and error is required!



# Write function to calculate shares of grouped land use classes within buffer of radius r

# Building buffers around monitoring sites with 1km and calculate shares of 10
# grouped land use classes relying on Beelen et al.(2009)
df.NO2.meta.NRW <- cbind(df.NO2.meta.NRW, data.frame(matrix(data = 0, nrow = nrow(df.NO2.meta.NRW),ncol = 10)))
names(df.NO2.meta.NRW)[37:46] <- paste(rep("Class", n = 10), seq(from = 1, to = 10, by = 1), sep = "")



## Write data frame that relates the 44 CLC classes to the 10 grouped classes in Beelen et al. (2009), p.1854-1855
df.CLC.grouped <- data.frame(matrix(data = 0, nrow = 10, ncol = 3))
names(df.CLC.grouped) <- c("GroupedClass", "Description", "CLCclasses")
df.CLC.grouped$GroupedClass <- 1:10
df.CLC.grouped$Description <- c("High density residential","Low density residential", "Industry", "Transport",
                                "Seaports", "Airports", "Construction","Urban Greenery", "Agriculture", "Forest")
df.CLC.grouped$CLCclasses <- c(as.character(1:6),"7-9","10-11","12-22","23-25")


# Transform SpatialGridDataFrame to raster
raster.CLC12 <- raster(sGdf.CLC12)


unique(df.NO2.meta.NRW$Projection)
# [1] EPSG:4979
# +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs

sp.NRW <- SpatialPoints(coords = df.NO2.meta.NRW[,c("Longitude","Latitude")],
                        proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
sp.NRW <- spTransform(sp.NRW, proj4string(raster.CLC12))

radius <- 1000

for(i in 1:nrow(df.NO2.meta.NRW)){
  list  <- extract(raster.CLC12,
                   coordinates(sp.NRW[i,]),
                   buffer = radius)
  
  nr.cells <- length(list[[1]])
  
  df.NO2.meta.NRW$Class1[i] <- sum(list[[1]]==1)/nr.cells
  df.NO2.meta.NRW$Class2[i] <- sum(list[[1]]==2)/nr.cells
  df.NO2.meta.NRW$Class3[i] <- sum(list[[1]]==3)/nr.cells
  df.NO2.meta.NRW$Class4[i] <- sum(list[[1]]==4)/nr.cells
  df.NO2.meta.NRW$Class5[i] <- sum(list[[1]]==5)/nr.cells
  df.NO2.meta.NRW$Class6[i] <- sum(list[[1]]==6)/nr.cells
  df.NO2.meta.NRW$Class7[i] <- sum(list[[1]]%in%c(7:9))/nr.cells
  df.NO2.meta.NRW$Class8[i] <- sum(list[[1]]%in%c(10:11))/nr.cells
  df.NO2.meta.NRW$Class9[i] <- sum(list[[1]]%in%c(12:22))/nr.cells
  df.NO2.meta.NRW$Class10[i] <- sum(list[[1]]%in%c(23:25))/nr.cells
}




# BKG (Bundesamt für Kartographie und Geodäsie) ----
# German administrative regions
admin.regions.2015 <- readOGR(dsn = "DataFull/Data_BKG/vg250-ew_ebenen",
                              layer = "VG250_GEM",
                              encoding = "UTF-8",
                              use_iconv = TRUE)


# BBSR (Bundesinstitut für Bau-, Stadt- und Raumforschung) ----
# Urban-rural typology of the German administrative regions
# data 'Gem15-Raumtyp.xlsx' received via E-Mail from Petra Kuhlmann on 19th July 2017
# Raumtypen beziehen sich auf die Raumordnung von 2010
# Die Zuordnung zu den Gemeinden bezieht sich auf den Gemeindestand 2015-12-31
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

regions.2015.complete@data[is.na(regions.2015.complete$raumt2010besiedlung),6:12] <- df.area2015[c(1113,1113,8497),1:7]

# Are there any more NA's?
check.na <- is.na(regions.2015.complete$raumt2010besiedlung)
table(check.na)
# No! -> to every polygon in 'admin.regions.2015.sub' the corresponding degree of urbanisation can be attributed

levels(regions.2015.complete$raumt2010besiedlung)
levels(regions.2015.complete$raumt2010besiedlung) <- c("suburban", "rural", "urban")


proj4string(regions.2015.complete)
proj4string(sp.NRW)


df.NO2.meta.NRW2 <- cbind(df.NO2.meta.NRW,over(spTransform(sp.NRW,proj4string(regions.2015.complete)),regions.2015.complete))
summary(df.NO2.meta.NRW2$AGS)
str(df.NO2.meta.NRW2$AGS)
df.NO2.meta.NRW2$AGS <- droplevels(df.NO2.meta.NRW2$AGS)
df.NO2.meta.NRW2[is.na(df.NO2.meta.NRW2$AGS),]

summary(duplicated(df.popDens2015$vbgem15)) # unambiguously identified
df.NO2.meta.NRW3 <- merge(df.NO2.meta.NRW2, df.popDens2015[,c(1,3,4)], by="vbgem15",all.x=TRUE)
df.NO2.meta.NRW3$popDens15 <- df.NO2.meta.NRW3$z_bev15/df.NO2.meta.NRW3$fl15_sum





# ZensusAtlas: popDens on 1x1km grid ----

# this command takes some minutes
sPdf.popDens <- readOGR(dsn = "DataFull/Data_GADM/Zensus_Atlas_Deutschland",		# M: This data set is missing!
                        layer = "Zensus_Atlas_Deutschland",
                        encoding = "UTF-8", use_iconv = TRUE)
# see also
# https://opendata-esri-de.opendata.arcgis.com/datasets/esri-de-content::zensus-atlas-deutschland/data

sPdf.bndry.DE <- readOGR(dsn = "DataFull/Data_GADM", 
                         layer = "DEU_adm0", encoding = "UTF-8", use_iconv = TRUE)
sPdf.bndry.NRW <- readOGR(dsn = "DataFull/Data_GADM",
                          layer = "DEU_adm1", encoding = "UTF-8", use_iconv = TRUE)
sPdf.bndry.NRW <- sPdf.bndry.NRW[sPdf.bndry.NRW$ID_1==10,]


proj4string(sPdf.bndry.NRW)
bbox(sPdf.bndry.NRW)

# filter for polygons in NRW
sPdf.popDens.NRW <- crop(sPdf.popDens, bbox(sPdf.bndry.NRW))

# rename the column that refers to population density (number of inhabitans per 1km^2)
names(sPdf.popDens.NRW)[14] <- "popDens"
sPdf.popDens.NRW$popDens <- droplevels(sPdf.popDens.NRW$popDens)
sPdf.popDens.NRW$popDens <- as.numeric(as.character(sPdf.popDens.NRW@data$popDens))

summary(sPdf.popDens.NRW$popDens)


# Transform sPdf.popDens.NRW to raster object
GK3 <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"
sPdf.popDens.NRW.GK3 <- spTransform(sPdf.popDens.NRW, GK3)

# plot(sPdf.popDens.NRW.GK3, col = "darkgrey")
# points(coordinates(sp.NRW)[,1],coordinates(sp.NRW)[,2], col = "yellow", pch = 19)

extent(sPdf.popDens.NRW.GK3)
(extent(sPdf.popDens.NRW.GK3)@xmax - extent(sPdf.popDens.NRW.GK3)@xmin)/1000 # 253 (rounded)
(extent(sPdf.popDens.NRW.GK3)@ymax - extent(sPdf.popDens.NRW.GK3)@ymin)/1000 # 250 (rounded)

# Filter area covering NRW that can be divided into 1x1km cells
sPdf.tmp <- crop(sPdf.popDens.NRW.GK3, 
                 matrix(c(extent(sPdf.popDens.NRW.GK3)@xmin, extent(sPdf.popDens.NRW.GK3)@xmin + 253*1000,
                          extent(sPdf.popDens.NRW.GK3)@ymin, extent(sPdf.popDens.NRW.GK3)@ymin + 250*1000),
                        ncol = 2, byrow = TRUE))

r <- raster(ncols = 253, nrows = 250, ext = extent(sPdf.tmp), crs = GK3, resolution = 1000)
r.popDens.NRW <- rasterize(sPdf.tmp, r, "popDens")


radius <- 1000 # choose 1000, 2000, or 3000
popDens2 <- rep(NA, nrow(df.NO2.meta.NRW3))

for(i in 1:nrow(df.NO2.meta.NRW)){
  list  <- extract(r.popDens.NRW,
                   coordinates(sp.NRW[i,]),
                   buffer = radius)
  nr.cells <- length(list[[1]][list[[1]]>0])
  popDens2[i] <- sum(list[[1]][list[[1]]>0])/nr.cells
}

df.NO2.meta.NRW3$popDens_Atlas_1kmBuffer <- popDens2
df.NO2.meta.NRW3$popDens_Atlas_2kmBuffer <- popDens2
df.NO2.meta.NRW3$popDens_Atlas_3kmBuffer <- popDens2


df.NO2.meta.NRW3$popDens_Atlas_NoBuffer <- extract(r.popDens.NRW, sp.NRW)


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


sp.NRW3 <- SpatialPointsDataFrame(sp.NRW, data = df.NO2.meta.NRW3)

radius <- 1000
BufferSites <- gBuffer(sp.NRW3, byid = TRUE,
                       id = sp.NRW3$AirQualityStationEoICode,
                       width = radius)


# Note difference between gIntersects and gContains
# gIntersects: buffer and line segment have at least one point in common
# gContains: buffer contains line segment

Test.RoadSubset <- gIntersects(spTransform(BufferSites,proj4string(sLdf.roads.DE)),sLdf.roads.DE,byid = TRUE)
dim(Test.RoadSubset)
# [1] 62448   29
# -> columns are related to buffers around monitoring sites
# -> rows are related to elements in sLdf.roads.DE


# Sum up the line segments intersecting with the buffer for each monitoring site distinguishing between the road types
df.NO2.meta.NRW3$Road14 <- 0
df.NO2.meta.NRW3$Road15 <- 0
df.NO2.meta.NRW3$Road16 <- 0
df.NO2.meta.NRW3$Road984 <- 0


for(i in 1:nrow(df.NO2.meta.NRW3)){
  sLdf.tmp <- subset(sLdf.roads.DE,
                     row.names(sLdf.roads.DE)%in%row.names(sLdf.roads.DE)[Test.RoadSubset[,i]])
  if(nrow(sLdf.tmp)>0){
    sLdf.tmp2 <- crop(spTransform(sLdf.tmp, proj4string(BufferSites)), BufferSites[i,])
    sLdf.tmp2.14 <- subset(sLdf.tmp2, sLdf.tmp2$RTT == 14)
    sLdf.tmp2.15 <- subset(sLdf.tmp2, sLdf.tmp2$RTT == 15)
    sLdf.tmp2.16 <- subset(sLdf.tmp2, sLdf.tmp2$RTT == 16)
    sLdf.tmp2.984 <- subset(sLdf.tmp2, sLdf.tmp2$RTT == 984)
    if(nrow(sLdf.tmp2.14)>0){
      df.NO2.meta.NRW3$Road14[i] <- sum(SpatialLinesLengths(sLdf.tmp2.14))
    }
    if(nrow(sLdf.tmp2.15)>0){
      df.NO2.meta.NRW3$Road15[i] <- sum(SpatialLinesLengths(sLdf.tmp2.15))
    }
    if(nrow(sLdf.tmp2.16)>0){
      df.NO2.meta.NRW3$Road16[i] <- sum(SpatialLinesLengths(sLdf.tmp2.16))
    }
    if(nrow(sLdf.tmp2.984)>0){
      df.NO2.meta.NRW3$Road984[i] <- sum(SpatialLinesLengths(sLdf.tmp2.984))
    }
  }
}


# Choose i and check if for-loop does what it is supposed to do...
i=2

sLdf.tmp <- subset(sLdf.roads.DE,row.names(sLdf.roads.DE)%in%row.names(sLdf.roads.DE)[Test.RoadSubset[,i]])
sLdf.tmp2 <- crop(spTransform(sLdf.tmp,proj4string(BufferSites)),BufferSites[i,])

sLdf.tmp2@data$RTT

sLdf.tmp2.14 <- subset(sLdf.tmp2, sLdf.tmp2$RTT==14)
sLdf.tmp2.15 <- subset(sLdf.tmp2, sLdf.tmp2$RTT==15)
sLdf.tmp2.16 <- subset(sLdf.tmp2, sLdf.tmp2$RTT==16)
sLdf.tmp2.984 <- subset(sLdf.tmp2, sLdf.tmp2$RTT==984)


par(mar=c(4,4,4,4))
plot(BufferSites[i,])
plot(sp.NRW3[i,],add=TRUE)
plot(spTransform(sLdf.tmp,proj4string(BufferSites)),col="blue",add=TRUE,lwd=2)
plot(sLdf.tmp2,col="orange",add=TRUE,lty=2,lwd=3)



LinesLength(sLdf.tmp2@lines[[1]]) + LinesLength(sLdf.tmp2@lines[[2]])
sum(SpatialLinesLengths(sLdf.tmp2.15))
sum(SpatialLinesLengths(sLdf.tmp2))
sum(SpatialLinesLengths(spTransform(sLdf.tmp,proj4string(sLdf.tmp2))))



# Select columns required for analysis ----
names(df.NO2.meta.NRW3)
df.analysis <- df.NO2.meta.NRW3[,c(2,11,24,26:29,31:32,35,38:48,50:51,57:58,61:69)]
df.analysis$ObservationDateBegin = 2015

# EoI: Exchange on Information
names(df.analysis)[c(1:3,5:20,24:34)] <- c("AQeCode", "Y", "Year", "Lon", "Lat", "Alt",
                                           "AQeType", "AQeArea", "AQeInletHeight",
                                           "HighDens", "LowDens", "Ind", "Transp", "Seap",
                                           "Airp", "Constr", "UrbGreen", "Agri", "Forest",
                                           "BBSRArea","BBSRArea2","BBSRpopDens", "PopDensAtlas1kmBuffer",
                                           "PopDensAtlas2kmBuffer", "PopDensAtlas3kmBuffer", "PopDensAtlasNoBuffer",
                                           "PriRoad", "SecRoad", "FedAuto", "LocRoute")

# write.csv(df.analysis, file = "R/DATA/Data_built/DATA_NRW_traffic_industrial.csv")


# rm(list = ls()[!ls() %in% "r.popDens.NRW"])
# save.image("R/DATA/Data_built/r.popDens.NRW.RData")
