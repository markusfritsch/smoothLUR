## Load packages ----

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
#install.packages("ggplot2")
library(ggplot2)
#install.packages("ggmap")
library(ggmap)
#install.packages("ggthemes")
library(ggthemes)

## Read data ----

load("R/DATA/Data_built/df.grid.final.RData")

sGdf.CLC12 <- readGDAL("R/DATA/Data_sources/Data_CLC12/g100_clc12_V18_5a/g100_clc12_V18_5.tif",
                       offset = c(19480,67313),
                       region.dim = c(8700,6500))

sPdf.boundaries.DE <- readOGR(dsn = "R/DATA/Data_sources/Data_GADM",
                              layer = "DEU_adm1",
                              encoding = "UTF-8",
                              use_iconv = TRUE)

df.meta <- read.csv("../../DATA/DE_AQeReporting_2013-2015/DE_2013-2015_metadata.csv",
                    sep = "\t", encoding = "UTF-8")

df.NO2 <- read.csv("../../DATA/DE_AQeReporting_2013-2015/DE_8_2013-2015_aggregated_timeseries.csv",
                   sep = "\t", encoding = "UTF-8")


# German administrative regions
admin.regions.2015 <- readOGR(dsn = "../../DATA/Data_BKG/vg250-ew_ebenen",
                              layer = "VG250_GEM",
                              encoding = "UTF-8",
                              use_iconv = TRUE)



# Derive SpatialPointsDataFrame from 'df.grid.final'
spdf.final <- SpatialPointsDataFrame(coords = cbind(df.grid.final$Lon, df.grid.final$Lat),
                                     data = df.grid.final,
                                     proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

sPdf.boundaries.DE <- spTransform(sPdf.boundaries.DE, proj4string(spdf.final))


# Filter data for North Rhine-Westphalia
spdf.NRW <- spdf.final[substring(spdf.final$AGS, 1, 2) == "05", ]
# 05: North Rhine-Westphalia

spdf.NRW.tr <- spTransform(spdf.NRW, CRSobj = proj4string(sGdf.CLC12))


# CLC class for each cellcentre of 1km x 1km cells over NRW
spdf.NRW.tr$CLC12 <- as.numeric(unlist(over(spdf.NRW.tr, sGdf.CLC12)))
spdf.NRW.tr$CLC12_grpd <- ifelse(spdf.NRW.tr$CLC12 %in% c(7:9), 7,
                                 ifelse(spdf.NRW.tr$CLC12 %in% c(10:11), 8,
                                        ifelse(spdf.NRW.tr$CLC12 %in% (12:22), 9,
                                               ifelse(spdf.NRW.tr$CLC12 %in% 23:25, 10,
                                                      ifelse(spdf.NRW.tr$CLC12 < 7, spdf.NRW.tr$CLC12, NA)))))
spdf.NRW.tr$CLC12_grpd <- as.factor(spdf.NRW.tr$CLC12_grpd)
levels(spdf.NRW.tr$CLC12_grpd) <- c("HighDens", "LowDens", "Ind", "Transp", "Seap",
                                    "Airp", "Constr", "UrbGreen", "Agri", "Forest")

sPdf.bndry.NRW <- sPdf.boundaries.DE[sPdf.boundaries.DE$ID_1==10,]
sPdf.bndry.NRW.tr <- spTransform(sPdf.bndry.NRW, CRSobj = proj4string(sGdf.CLC12))


admin.regions.2015.NRW <- admin.regions.2015[substr(admin.regions.2015$AGS, 1, 2) == "05", ]


## Filter monitoring sites located in North Rhine-Westphalia ----

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
# df.Background <- df.NO2.meta[df.NO2.meta$AirQualityStationType=="background",]

unique(droplevels(df.NO2.meta$DatetimeBegin))
unique(droplevels(df.NO2.meta$DatetimeEnd))

# Focus on one year only
df.NO2.meta <- df.NO2.meta[df.NO2.meta$DatetimeBegin=="2015-01-01 00:00:00",]

# Filter monitoring sites located in North Rhine-Westphalia
df.NO2.meta.NRW <- df.NO2.meta[substr(df.NO2.meta$AirQualityStationEoICode, 1, 4) == "DENW", ]

# Identify duplicates
df.NO2.meta.NRW$AirQualityStationEoICode[duplicated(df.NO2.meta.NRW$AirQualityStationEoICode)]

summary(df.NO2.meta.NRW$MeasurementType)
#  active automatic   passive
#       0        56         9
#
# Remove duplicates
df.NO2.meta.NRW <- df.NO2.meta.NRW[df.NO2.meta.NRW$MeasurementType=="automatic",]

table(df.NO2.meta.NRW$AirQualityStationType)

# Build spatial points data frame for monitoring sites in NRW
spdf.NO2.NRW <- SpatialPointsDataFrame(coords = cbind(df.NO2.meta.NRW$Longitude, df.NO2.meta.NRW$Latitude),
                                       data = df.NO2.meta.NRW,
                                       proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

ggplot(data = df.NO2.meta.NRW, aes(Longitude, Latitude, colour = AirQualityStationType)) +
  geom_point()


spplot(spdf.NRW.tr, "CLC12_grpd", key.space = "right", col.regions = c(brewer.pal(3, "Accent"), brewer.pal(7, "Dark2")))
spplot(spdf.NRW, "BBSRArea")


cuts.tmp <- quantile(spdf.NRW$BBSRpopDens, seq(from = 0, to = 1, by = 0.1))
spplot(spdf.NRW, "BBSRpopDens",
       main = "PopDens in North Rhine-Westphalia",
       key.space = "right",
       par.settings=list(fontsize=list(text=10)),
       cuts = cuts.tmp, #c(0, 50, 150, 350, 600, 1000, 1500, 3000, 4500, +Inf),
       col.regions = brewer.pal(9, "Oranges"))+
  latticeExtra::layer(sp.polygons(sPdf.boundaries.DE[sPdf.boundaries.DE$ID_1==10, ],
                                  fill = NA,
                                  col = "grey",
                                  lwd = 2))+
  latticeExtra::layer(sp.points(spdf.NO2.NRW,
                                col = spdf.NO2.NRW$AirQualityStationType,
                                pch = spdf.NO2.NRW$AirQualityStationType))

b <- bbox(sPdf.bndry.NRW)
NRW <- ggmap(get_map(location = b, maptype = "satellite", zoom = 8))


sPdf.bndry.NRW.f <- fortify(sPdf.bndry.NRW, region = "NAME_1")

NRW + geom_path(data = sPdf.bndry.NRW.f, aes(x = long, y = lat), lwd = 1) +
  geom_point(data = df.NO2.meta.NRW, aes(x = Longitude, y = Latitude, col = AirQualityStationType), size = 2, shape = 15)


ggplot(data = sPdf.bndry.NRW.f, aes(x = long, y = lat)) +
  theme_bw() +
  coord_fixed() +
  geom_path(lwd = 1) +
  geom_point(data = df.NO2.meta.NRW, aes(x = Longitude, y = Latitude, col = AirQualityStationType), size = 2, shape = 15)


sPdf.bndry.NRW.tr2 <- spTransform(sPdf.bndry.NRW, proj4string(spdf.NRW.tr))
sPdf.bndry.NRW.tr2.f <- fortify(sPdf.bndry.NRW.tr2, region = "NAME_1")
ggplot(data = sPdf.bndry.NRW.tr2.f, aes(x = long, y = lat)) +
  theme_bw() +
  coord_fixed() +
  geom_point(data = spdf.NRW.tr@data,
            aes(coordinates(spdf.NRW.tr)[,1],
                coordinates(spdf.NRW.tr)[,2],
                col = CLC12_grpd)) +
  scale_color_manual(values = c(brewer.pal(3, "Accent"), brewer.pal(7, "Dark2")), aesthetics = "colour") +
  geom_path(data = sPdf.bndry.NRW.tr2.f, aes(x = long, y = lat), lwd = 1)





admin.regions.NRW.f <- fortify(admin.regions.2015.NRW, region = "AGS")
names(admin.regions.NRW.f)[6] <- "AGS"
admin.regions.NRW.f <- merge(admin.regions.NRW.f, admin.regions.2015.NRW[,c("AGS", "GEN")], by = "AGS", all.x = TRUE)

dat.tmp <- spdf.NRW[!duplicated(spdf.NRW$AGS), c("AGS", "BBSRpopDens")]
admin.regions.NRW.f <- merge(admin.regions.NRW.f, dat.tmp[,c("AGS", "BBSRpopDens")], by = "AGS", all.x = TRUE)

names(df.NO2.meta.NRW)[c(26,27)] <- c("long", "lat")
spdf.NO2.NRW.tr <- spTransform(spdf.NO2.NRW, proj4string(admin.regions.2015.NRW))


(brks.tmp <- c(0, quantile(dat.tmp$BBSRpopDens, seq(from = 0.125, to = 1, by = 0.125))))
admin.regions.NRW.f$brks <- cut(admin.regions.NRW.f$BBSRpopDens,
                                breaks = brks.tmp,
                                labels = c("< 127", "127 - 170", "170 - 219", "219 - 292",
                                           "292 - 415", "415 - 617", "617 - 1082", "1082 - 3031"))


ggplot(admin.regions.NRW.f, aes(x = long, y = lat)) +
  theme_bw() +
  coord_fixed() +
  geom_polygon(aes(group = AGS, fill = brks))+
  geom_path(aes(group = AGS), color = "darkgrey", lwd = 0.01) +
  geom_point(data = spdf.NO2.NRW.tr@data, aes(x = coordinates(spdf.NO2.NRW.tr)[,1],
                                              y = coordinates(spdf.NO2.NRW.tr)[,2],
                                              col = AirQualityStationType),
             size = 2, shape = 15) +
  scale_color_manual(name = "StationType", values = c("orange", "yellow", "purple"), aesthetics = "colour") +
  scale_color_manual(name = "popDens", values = brewer.pal(9, "Blues"), aesthetics = "fill")



spplot(spdf.NRW, "Alt",
       main = "Altitude in North Rhine-Westphalia",
       key.space = "right",
       par.settings=list(fontsize=list(text=10)),
       cuts = c(0, 150, 300, 450, 600, 1000, 1500, 2500, +Inf),
       col.regions = brewer.pal(9, "Greens")[3:9])+
  latticeExtra::layer(sp.polygons(sPdf.boundaries.DE[sPdf.boundaries.DE$ID_1==10, ],
                                  fill = NA,
                                  col = "grey",
                                  lwd = 2))

