#######################################################
### Figures and tables for results section
#######################################################



## Load packages
library(cowplot)     # for plot_grid(), draw_plot()
library(data.table)
library(ggplot2)
library(mgcv)
library(ggpubr)
library(raster)
library(RColorBrewer)
library(rgdal)
library(rgeos)
library(RgoogleMaps) # for qbbox()
#library(devtools)
#install_github("markusfritsch/smoothLUR")
library(smoothLUR)
library(sp)




###
### Fig.4: Partial effects of smoothA (LUR model based on additive regression smoothers using data observed at all monitoring sites)
###

rm(list = ls())

load("data/monSitesDE.rda")
dat	<- monSitesDE[, c(1,2,4:7,9:24)]

smoothA <- smoothLUR(data = dat
                     ,x = c("Lon", "Lat", "Alt", "HighDens", "LowDens", "Ind", "Transp"
#					,"Seap", "Airp", "Constr"
					,"UrbGreen", "Agri", "Forest", "PopDens"
					, "PriRoad", "SecRoad", "FedAuto", "LocRoute")
                     ,spVar1 = "Lon"
                     ,spVar2 = "Lat"
                     ,y = "Y"
                     ,thresh = 0.95)

newdata.tmp <- data.frame(matrix(NA, nrow = 1000, ncol = 18))
dat.pred <- dat[, c(3:5,7:10,14:21)]
colnames(newdata.tmp) <- colnames(dat.pred)
for(j in 1:ncol(dat.pred)){
  newdata.tmp[,j] <- seq(min(dat.pred[,j]), max(dat.pred[,j]), length.out = 1000)
}

pred.tmpA <- predict(object = smoothA, newdata = newdata.tmp, se.fit = TRUE, type = "terms")

summary(smoothA)$s.table
rownames(summary(smoothA)$s.table)      # structure of coefficient table identical across models

vec.tmp <- c(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14)

sp.tmp <- rownames(summary(smoothA)$s.table)[vec.tmp]

edf.tmp <- round(summary(smoothA)$s.table[vec.tmp, "edf"], 2)

stripe.tmp <- paste(substr(sp.tmp, start = 1, stop = nchar(sp.tmp)-1),
                    ", ", edf.tmp, ")", sep = "")

pred.tmp2 <- pred.tmpA[[1]][,sp.tmp]
sd.tmp <- pred.tmpA[[2]][,sp.tmp]

conf.lower <- pred.tmp2 - 2*sd.tmp
conf.upper <- pred.tmp2 + 2*sd.tmp

newdata.tmp2 <- newdata.tmp[,substr(sp.tmp, start = 3, stop = nchar(sp.tmp)-1)]


# transform in long format
pred.tmp2.melt <- reshape2::melt(pred.tmp2)
conf.lower.melt <- reshape2::melt(conf.lower)
conf.upper.melt <- reshape2::melt(conf.upper)

pred.conf <- cbind(pred.tmp2.melt[,-1], conf.lower.melt[,3], conf.upper.melt[,3])
names(pred.conf) <- c("variable", "value", "lwr", "upr")

levels(pred.conf$variable)
levels(pred.conf$variable) <- stripe.tmp

newdata.tmp2.melt <- reshape2::melt(newdata.tmp2)
colnames(newdata.tmp2.melt) <- c("pred", "x")

dt.tmp <- cbind(pred.conf, newdata.tmp2.melt)


#pdf("../img/PartialEffects_smoothA.pdf", width = 12, height = 12)
ggplot(data = dt.tmp, aes(x = x, y = value)) +
  theme_bw() +
  xlab("") +
  ylab("") +
  geom_line(col = brewer.pal(11, "BrBG")[10],# "royalblue",
            lwd = 0.7) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.25) +
  facet_wrap(~variable, nrow = 4, scales = "free_x") +
  theme(axis.text = element_text(size = 14),
        strip.text = element_text(size = 14))
#dev.off()








###
### Fig.5: Interpolation maps derived from smoothA (LUR model based on additive regression smoothers using data observed at all monitoring sites)
###

rm(list = ls())


# Load grid over Germany
load("data/Data_built/grid.DE.RData")
names(df.grid.DE)
df.grid.DE <- df.grid.DE[df.grid.DE$AGS!="01056025", ] # exclude Helgoland constituted of two small islands
names(df.grid.DE)[c(4,5)] <- c("Lon", "Lat")


load("data/monSitesDE.rda")
dat  	<- monSitesDE[, c(1,2,4:7,9:24)]
datB  <- dat[dat$AQeType == "background", ]
datTI <- dat[dat$AQeType != "background", ]


smoothA <- smoothLUR(data = dat
                     ,x = c("Lon", "Lat", "Alt", "HighDens", "LowDens", "Ind", "Transp"
                            #					,"Seap", "Airp", "Constr"
                            ,"UrbGreen", "Agri", "Forest", "PopDens"
                            , "PriRoad", "SecRoad", "FedAuto", "LocRoute")
                     ,spVar1 = "Lon"
                     ,spVar2 = "Lat"
                     ,y = "Y"
                     ,thresh = 0.95)

smoothB <- smoothLUR(data = datB
                     ,x = c("Lon", "Lat", "Alt", "HighDens", "LowDens", "Ind", "Transp"
                            #					,"Seap", "Airp", "Constr"
                            ,"UrbGreen", "Agri", "Forest", "PopDens"
                            , "PriRoad", "SecRoad", "FedAuto", "LocRoute")
                     ,spVar1 = "Lon"
                     ,spVar2 = "Lat"
                     ,y = "Y"
                     ,thresh = 0.95)

smoothTI <- smoothLUR(data = datTI
                      ,x = c("Lon", "Lat", "Alt", "HighDens", "LowDens", "Ind", "Transp"
                             #					,"Seap", "Airp", "Constr"
                             ,"UrbGreen", "Agri", "Forest", "PopDens"
                             , "PriRoad", "SecRoad", "FedAuto", "LocRoute")
                      ,spVar1 = "Lon"
                      ,spVar2 = "Lat"
                      ,y = "Y"
                      ,thresh = 0.95)

df.grid.DE$smoothA.sp	<- predict(object = smoothA, newdata = df.grid.DE, type="terms")[,"s(Lon,Lat)"] +
  predict(object = smoothA, newdata = df.grid.DE, type="terms")[,"s(Alt)"]

df.grid.DE$smoothB.sp	<- predict(object = smoothB, newdata = df.grid.DE, type="terms")[,"s(Lon,Lat)"] +
  predict(object = smoothB, newdata = df.grid.DE, type="terms")[,"s(Alt)"]

df.grid.DE$smoothTI.sp	<- predict(object = smoothTI, newdata = df.grid.DE, type="terms")[,"s(Lon,Lat)"] +
  predict(object = smoothB, newdata = df.grid.DE, type="terms")[,"s(Alt)"]

#common color scheme
(brks.sp <- seq(from = min(df.grid.DE$smoothA.sp, df.grid.DE$smoothB.sp, df.grid.DE$smoothTI.sp),
                to = max(df.grid.DE$smoothA.sp, df.grid.DE$smoothB.sp, df.grid.DE$smoothTI.sp),
                length.out = 11))
brks2.sp <- seq(-12, 8, 4)


#estimated spatial effect in smoothA (left plot in Figure 5)
p.sp.effA <- ggplot(df.grid.DE, aes(x = Lon.GK3, y = Lat.GK3)) +
  ggmap::theme_nothing(legend = TRUE) +  #theme_bw() +
  xlab("") +
  ylab("") +
  coord_fixed(1) +
  geom_tile(aes(fill = smoothA.sp), width = 1000, height = 1000, na.rm = TRUE) +
  scale_fill_gradientn(name = "",
                       colours = brewer.pal(11, "BrBG")[-6],
                       breaks = brks2.sp,
                       labels = brks2.sp,
                       limits = range(brks.sp),
                       na.value = "white") +
  theme(legend.text  = element_text(size = 17),
        legend.background = element_blank(),
        legend.position = "bottom",
        legend.box = "vertical",
        axis.ticks = element_blank()) +
  guides(fill = guide_colourbar(barwidth = 20))




df.grid.DE$smoothA	<- as.vector(predict(object = smoothA, newdata = df.grid.DE))
df.grid.DE$smoothB	<- as.vector(predict(object = smoothB, newdata = df.grid.DE))
df.grid.DE$smoothTI	<- as.vector(predict(object = smoothTI, newdata = df.grid.DE))



#NO2 concentrations not supported by the underlying data
#all monitoring sites
length(df.grid.DE$smoothA[df.grid.DE$smoothA < min(dat$Y)]) # 951
length(df.grid.DE$smoothA[df.grid.DE$smoothA < min(dat$Y)])/length(df.grid.DE$smoothA)*100		# share in % of predicted values
df.grid.DE$smoothA[df.grid.DE$smoothA < min(dat$Y)]		<- min(dat$Y)				# replace by min contained in data set

length(df.grid.DE$smoothA[df.grid.DE$smoothA > max(dat$Y)]) # 0
length(df.grid.DE$smoothA[df.grid.DE$smoothA > max(dat$Y)])/length(df.grid.DE$smoothA)*100		# share in % of predicted values
df.grid.DE$smoothA[df.grid.DE$smoothA > max(dat$Y)]		<- max(dat$Y)				# replace by max contained in data set


#background
length(df.grid.DE$smoothB[df.grid.DE$smoothB < min(datB$Y)]) # 383
length(df.grid.DE$smoothB[df.grid.DE$smoothB < min(datB$Y)])/length(df.grid.DE$smoothB)*100		# share in % of predicted values
df.grid.DE$smoothB[df.grid.DE$smoothB < min(datB$Y)]		<- min(datB$Y)				# replace by min contained in data set

length(df.grid.DE$smoothB[df.grid.DE$smoothB > max(datB$Y)]) # 2
length(df.grid.DE$smoothB[df.grid.DE$smoothB > max(datB$Y)])/length(df.grid.DE$smoothB)*100		# share in % of predicted values
df.grid.DE$smoothB[df.grid.DE$smoothB > max(datB$Y)]		<- max(datB$Y)				# replace by max contained in data set


#traffic/industrial
length(df.grid.DE$smoothTI[df.grid.DE$smoothTI < min(datTI$Y)]) # 2923
length(df.grid.DE$smoothTI[df.grid.DE$smoothTI < min(datTI$Y)])/length(df.grid.DE$smoothTI)*100		# share in % of predicted values
df.grid.DE$smoothTI[df.grid.DE$smoothTI < min(datTI$Y)]		<- min(datTI$Y)				# replace by min contained in data set

length(df.grid.DE$smoothTI[df.grid.DE$smoothTI > max(datTI$Y)]) # 0
length(df.grid.DE$smoothTI[df.grid.DE$smoothTI > max(datTI$Y)])/length(df.grid.DE$smoothTI)*100		# share in % of predicted values
df.grid.DE$smoothTI[df.grid.DE$smoothTI > max(datTI$Y)]		<- max(datTI$Y)				# replace by max contained in data set


#common color scheme
(brks <- seq(from = min(df.grid.DE$smoothA, df.grid.DE$smoothB, df.grid.DE$smoothTI),
             to = max(df.grid.DE$smoothA, df.grid.DE$smoothB, df.grid.DE$smoothTI),
             length.out = 11))
brks2 <- seq(10, 70, 20)


###map visualizing conditional mean annual NO2 concentration levels derived from smoothA (right plot in Figure 5)
p.predA <- ggplot(df.grid.DE, aes(x = Lon.GK3, y = Lat.GK3)) +
  ggmap::theme_nothing(legend = TRUE) +
  xlab("") +
  ylab("") +
  coord_fixed(1) +
  geom_tile(aes(fill = smoothA), width = 1000, height = 1000, na.rm = TRUE) +
  scale_fill_gradientn(name = "",
                       colours = brewer.pal(9, "YlOrRd")[-1],
                       breaks = brks2,
                       labels = brks2,
                       limits = range(brks),
                       na.value = "white") +
  theme(legend.text  = element_text(size = 17),
        legend.background = element_blank(),
        legend.position = "bottom",
        legend.box = "vertical",
        axis.ticks = element_blank()) +
  guides(fill = guide_colourbar(barwidth = 20))



###complementing 'p.predA' by boundary of Rhine-Ruhr region

# Data on population density; BKG (Bundesamt für Kartographie und Geodäsie)
# German administrative regions at municipality level
sPdf.Municipalities <- readOGR(dsn = "data/Data_BKG/vg250-ew_ebenen",
                               layer = "VG250_GEM",
                               encoding = "UTF-8",
                               use_iconv = TRUE)

# Read indicator vector for filtering AGS referring to Rhine-Ruhr region
ind.RR <- readRDS("indRhineRuhr.rds")

# Filter administrative regions referring to Rhine-Ruhr area
sPdf.Municipalities.RR    <- sPdf.Municipalities[sPdf.Municipalities$AGS %in% ind.RR, ]
sPdf.Municipalities.RR$ID <- "RR"

GK3 <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"

#boundary of RR region
admin.bndry.RR <- spTransform(sPdf.Municipalities.RR, GK3)
admin.bndry.RR.f <- fortify(admin.bndry.RR, region = "ID")
bndry.tmp <- admin.bndry.RR.f


names(bndry.tmp)[1:2] <- c("Lon.GK3", "Lat.GK3")
p.predA2 <- p.predA +
  geom_polygon(data = bndry.tmp, color = brewer.pal(11, "BrBG")[1], lwd = 0.7, fill = NA)



#png("../img/SpEff_PredA2.png", width = 900, height = 675)
#pdf("../img/SpEff_PredA2.pdf", width = 12, height = 9)
plot_grid(p.sp.effA, p.predA2, nrow = 1)
#dev.off()






###
### Fig.6: Histogram and empirical density curve for LOOCV prediction errors (at background & traffic/industrial monitoring sites for LUR model based on additive regression smoothers smoothA)
###

rm(list = ls())

load("cvResultsA.RData")


## LOOCV prediction errors
loocvA <- cbind(loocv.A$df.err, monSitesDE[,c(1,7)])


loocvA.bg <- loocvA[loocvA$AQeType == "background", ]
loocvA.ti <- loocvA[loocvA$AQeType != "background", ]


dat.val  <- data.frame(Y = c(loocvA.bg$Err.par, loocvA.ti$Err.par),
                      type = c(rep("Background", 246), rep("Traffic/Industrial", 157)))
dat.val2 <- data.frame(Y = c(loocvA.bg$Err.smooth, loocvA.ti$Err.smooth),
                       type = c(rep("Background", 246), rep("Traffic/Industrial", 157)))


(p.hist <- ggplot(dat.val2, aes(x = Y, fill = type, color = type)) +
    theme_minimal() +
    theme_classic() +
    scale_x_continuous(breaks = seq(0, 90, by = 15), limits = c(-30, 55)) +
    geom_histogram(aes(y=..density..), fill = "white", position = "identity", alpha = 0.5, binwidth = 5, lwd = 1.4) +
    geom_density(alpha = 0.6, lwd = 1.2) +
    xlab("prediction error") +
    ylab("empirical density") +
    scale_color_manual(values = brewer.pal(9, "BrBG")[c(1,3)],
                       aesthetics = c("fill", "colour"))+
    theme(axis.text = element_text(size = 18),
          axis.title = element_text(size = 18),
          legend.title = element_blank(),
          legend.text = element_text(size = 18),
          legend.position = c(0.75, 0.9)))


#pdf("../img/empDensPredLoocvATrInd.pdf", height = 6, width = 9)
p.hist
#dev.off()





###
### Figure 8: Satellite and map images for grid cells centered around three points ----
### Table 7: Structural predictors of three locations ----
### Table 8: Prediciton for three locations ----
###

rm(list = ls())

load("Data_built/grid.DE.RData")
names(df.grid.DE)[c(4,5)] <- c("Lon", "Lat")

dat.Positions <- data.frame(matrix(NA, nrow = 3, ncol = ncol(df.grid.DE)+1))
names(dat.Positions) <- c("Name", names(df.grid.DE))

# 1) Cologne city centre, close to main station (urban)
# Look on GoogleMaps for coordinates close to Cologne main station and filter respective cells from 'df.grid.DE'
# View(df.grid.DE[6.95 < df.grid.DE$Lon & df.grid.DE$Lon < 6.96 & 50.93 < df.grid.DE$Lat & df.grid.DE$Lat < 50.94, ])
dat.Positions[1,1] <- "Cologne City Centre"
dat.Positions[1,-1] <-  df.grid.DE[df.grid.DE$ID == "295827", ]

# 2) South-east of Mühlheim an der Ruhr (rural)
#View(df.grid.DE[6.90 < df.grid.DE$Lon & df.grid.DE$Lon < 6.91 & 51.40 < df.grid.DE$Lat & df.grid.DE$Lat < 51.41, ])
dat.Positions[2,1] <- "MuehlheimRuhr"
dat.Positions[2,-1] <-  df.grid.DE[df.grid.DE$ID == "262025", ]

# 3) Suburb of Dortmund
# View(df.grid.DE[7.47 < df.grid.DE$Lon & df.grid.DE$Lon < 7.48 & 51.49 < df.grid.DE$Lat & df.grid.DE$Lat < 51.50, ])
dat.Positions[3,1] <- "Dortmund"
dat.Positions[3,-1] <-  df.grid.DE[df.grid.DE$ID == "256215", ]






WGS84 <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
GK3 <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"

df.tmp <- dat.Positions[1,]
spdf.tmp <- SpatialPoints(coords = cbind(df.tmp$Lon.GK3, df.tmp$Lat.GK3),
                          proj4string = CRS(GK3))
grid.topo <- GridTopology(cellcentre.offset = coordinates(spdf.tmp)[1,],
                          cellsize = c(1000, 1000),
                          cells.dim = c(1, 1))
grid.tmp <- SpatialGrid(grid.topo, proj4string = GK3)
b <- bbox(grid.tmp)
sp.b <- SpatialPoints(t(b), CRS(GK3))
sp.b2 <- spTransform(sp.b, WGS84)
b
bbox(sp.b)
b2 <- qbbox(lat = coordinates(sp.b2)[,2],
            lon = coordinates(sp.b2)[,1])

# Sys.setenv(LANG = "en")
# GetMap.bbox(lonR = b2$lonR, latR = b2$latR, destfile = "../img/MapCologneCityCentre.png", type = "google-s")
# GetMap.bbox(lonR = b2$lonR, latR = b2$latR, destfile = "../img/MapCologneCityCentre2.png", type = "google")



df.tmp <- dat.Positions[2,]
spdf.tmp <- SpatialPoints(coords = cbind(df.tmp$Lon.GK3, df.tmp$Lat.GK3),
                          proj4string = CRS(GK3))

grid.topo <- GridTopology(cellcentre.offset = coordinates(spdf.tmp)[1,],
                          cellsize = c(1000, 1000),
                          cells.dim = c(1, 1))
grid.tmp <- SpatialGrid(grid.topo, proj4string = GK3)
b <- bbox(grid.tmp)
sp.b <- SpatialPoints(t(b), CRS(GK3))
sp.b2 <- spTransform(sp.b, WGS84)
b
bbox(sp.b)
b2 <- qbbox(lat = coordinates(sp.b2)[,2],
            lon = coordinates(sp.b2)[,1])

#GetMap.bbox(lonR = b2$lonR, latR = b2$latR, destfile = "../img/MapMuehlheim.png", type = "google-s")
#GetMap.bbox(lonR = b2$lonR, latR = b2$latR, destfile = "../img/MapMuehlheim2.png", type = "google")


df.tmp <- dat.Positions[3,]
spdf.tmp <- SpatialPoints(coords = cbind(df.tmp$Lon.GK3, df.tmp$Lat.GK3),
                          proj4string = CRS(GK3))

grid.topo <- GridTopology(cellcentre.offset = coordinates(spdf.tmp)[1,],
                          cellsize = c(1000, 1000),
                          cells.dim = c(1, 1))
grid.tmp <- SpatialGrid(grid.topo, proj4string = GK3)

b <- bbox(grid.tmp)
sp.b <- SpatialPoints(t(b), CRS(GK3))
sp.b2 <- spTransform(sp.b, WGS84)
b
bbox(sp.b)
b2 <- qbbox(lat = coordinates(sp.b2)[,2],
            lon = coordinates(sp.b2)[,1])

#GetMap.bbox(lonR = b2$lonR, latR = b2$latR, destfile = "../img/MapDortmund.png", type = "google-s")
#GetMap.bbox(lonR = b2$lonR, latR = b2$latR, destfile = "../img/MapDortmund2.png", type = "google")





## Derive predictions from LUR models based on additive regression smoothers for the three locations

load("data/monSitesDE.rda")
dat  	<- monSitesDE[, c(1,2,4:7,9:24)]
datB  <- dat[dat$AQeType == "background", ]
datTI <- dat[dat$AQeType != "background", ]

smoothA <- smoothLUR(data = dat
                     ,x = c("Lon", "Lat", "Alt", "HighDens", "LowDens", "Ind", "Transp"
                            #					,"Seap", "Airp", "Constr"
                            ,"UrbGreen", "Agri", "Forest", "PopDens"
                            , "PriRoad", "SecRoad", "FedAuto", "LocRoute")
                     ,spVar1 = "Lon"
                     ,spVar2 = "Lat"
                     ,y = "Y"
                     ,thresh = 0.95)


smoothB <- smoothLUR(data = datB
                     ,x = c("Lon", "Lat", "Alt", "HighDens", "LowDens", "Ind", "Transp"
                            #					,"Seap", "Airp", "Constr"
                            ,"UrbGreen", "Agri", "Forest", "PopDens"
                            ,"PriRoad", "SecRoad", "FedAuto", "LocRoute")
                     ,spVar1 = "Lon"
                     ,spVar2 = "Lat"
                     ,y = "Y"
                     ,thresh = 0.95)


smoothTI <- smoothLUR(data = datTI
                      ,x = c("Lon", "Lat", "Alt", "HighDens", "LowDens", "Ind", "Transp"
                             #					,"Seap", "Airp", "Constr"
                             ,"UrbGreen", "Agri", "Forest", "PopDens"
                             ,"PriRoad", "SecRoad", "FedAuto", "LocRoute")
                      ,spVar1 = "Lon"
                      ,spVar2 = "Lat"
                      ,y = "Y"
                      ,thresh = 0.95)



dat.Positions$smoothB  <- predict(object = smoothB, newdata = dat.Positions)
dat.Positions$smoothTI <- predict(object = smoothTI, newdata = dat.Positions)
dat.Positions$smoothA  <- predict(object = smoothA, newdata = dat.Positions)

#saveRDS(object = dat.Positions, file = "Data_built/dat.Positions.rds")





###
### Figure 7: Interpolation maps derived from smoothB and smoothTI across Rhine-Ruhr metropolitan region ----
### smoothB (smoothTI) refers to LUR model based on additive regression smoothers
### using data observed at background (traffic/industrial) monitoring sites
###

rm(list = ls())


# Load grid over Germany
load("Data_built/grid.DE.RData")
names(df.grid.DE)
df.grid.DE <- df.grid.DE[df.grid.DE$AGS!="01056025", ] # exclude Helgoland constituted of two small islands
names(df.grid.DE)[c(4,5)] <- c("Lon", "Lat")


load("data/monSitesDE.rda")
dat  	<- monSitesDE[, c(1,2,4:7,9:24)]
datB  <- dat[dat$AQeType == "background", ]
datTI <- dat[dat$AQeType != "background", ]


smoothA <- smoothLUR(data = dat
                     ,x = c("Lon", "Lat", "Alt", "HighDens", "LowDens", "Ind", "Transp"
                            #					,"Seap", "Airp", "Constr"
                            ,"UrbGreen", "Agri", "Forest", "PopDens"
                            , "PriRoad", "SecRoad", "FedAuto", "LocRoute")
                     ,spVar1 = "Lon"
                     ,spVar2 = "Lat"
                     ,y = "Y"
                     ,thresh = 0.95)

smoothB <- smoothLUR(data = datB
                     ,x = c("Lon", "Lat", "Alt", "HighDens", "LowDens", "Ind", "Transp"
                            #					,"Seap", "Airp", "Constr"
                            ,"UrbGreen", "Agri", "Forest", "PopDens"
                            , "PriRoad", "SecRoad", "FedAuto", "LocRoute")
                     ,spVar1 = "Lon"
                     ,spVar2 = "Lat"
                     ,y = "Y"
                     ,thresh = 0.95)

smoothTI <- smoothLUR(data = datTI
                      ,x = c("Lon", "Lat", "Alt", "HighDens", "LowDens", "Ind", "Transp"
                             #					,"Seap", "Airp", "Constr"
                             ,"UrbGreen", "Agri", "Forest", "PopDens"
                             , "PriRoad", "SecRoad", "FedAuto", "LocRoute")
                      ,spVar1 = "Lon"
                      ,spVar2 = "Lat"
                      ,y = "Y"
                      ,thresh = 0.95)


df.grid.DE$smoothA	<- as.vector(predict(object = smoothA, newdata = df.grid.DE))
df.grid.DE$smoothB	<- as.vector(predict(object = smoothB, newdata = df.grid.DE))
df.grid.DE$smoothTI	<- as.vector(predict(object = smoothTI, newdata = df.grid.DE))



#NO2 concentrations not supported by the underlying data

#all monitoring sites
df.grid.DE$smoothA[df.grid.DE$smoothA < min(dat$Y)]		<- min(dat$Y)				# replace by min contained in data set
df.grid.DE$smoothA[df.grid.DE$smoothA > max(dat$Y)]		<- max(dat$Y)				# replace by max contained in data set

#background
df.grid.DE$smoothB[df.grid.DE$smoothB < min(datB$Y)]		<- min(datB$Y)				# replace by min contained in data set
df.grid.DE$smoothB[df.grid.DE$smoothB > max(datB$Y)]		<- max(datB$Y)				# replace by max contained in data set

#traffic/industrial
df.grid.DE$smoothTI[df.grid.DE$smoothTI < min(datTI$Y)]		<- min(datTI$Y)				# replace by min contained in data set
df.grid.DE$smoothTI[df.grid.DE$smoothTI > max(datTI$Y)]		<- max(datTI$Y)				# replace by max contained in data set


#common color scheme
(brks <- seq(from = min(df.grid.DE$smoothA, df.grid.DE$smoothB, df.grid.DE$smoothTI),
             to = max(df.grid.DE$smoothA, df.grid.DE$smoothB, df.grid.DE$smoothTI),
             length.out = 11))
brks2 <- seq(10, 70, 20)



# Read indicator vector for filtering AGS referring to Rhine-Ruhr region
ind.RR <- readRDS("indRhineRuhr.rds")
df.grid.RR <- df.grid.DE[df.grid.DE$AGS %in% ind.RR, ]

p.predB.RR <- ggplot(df.grid.RR, aes(x = Lon.GK3, y = Lat.GK3)) +
  ggmap::theme_nothing() +
  xlab("") +
  ylab("") +
  coord_fixed(1) +
  geom_tile(aes(fill = smoothB), width = 1000, height = 1000, na.rm = TRUE) +
  scale_fill_gradientn(name = "",
                       colours = brewer.pal(9, "YlOrRd")[-1],
                       breaks = brks2,
                       labels = brks2,
                       limits = range(brks),
                       na.value = "white") +
  theme(legend.text  = element_text(size = 17),
        legend.background = element_blank(),
        legend.position = "bottom",
        legend.box = "vertical",
        axis.ticks = element_blank()) +
  guides(fill = guide_colourbar(barwidth = 20))



p.predTI.RR <- ggplot(df.grid.RR, aes(Lon.GK3, y = Lat.GK3)) +
  ggmap::theme_nothing() +
  xlab("") +
  ylab("") +
  coord_fixed(1) +
  geom_tile(aes(fill = smoothTI), width = 1000, height = 1000, na.rm = TRUE) +
  scale_fill_gradientn(name = "",
                       colours = brewer.pal(9, "YlOrRd")[-1],
                       breaks = brks2,
                       labels = brks2,
                       limits = range(brks),
                       na.value = "white") +
  theme(legend.text  = element_text(size = 17),
        legend.background = element_blank(),
        legend.position = "bottom",
        legend.box = "vertical",
        axis.ticks = element_blank()) +
  guides(fill = guide_colourbar(barwidth = 20))


dat.Positions <- readRDS("dat.Positions.rds")


## Plot points 1 to 3 (of `dat.Positions`) on map
(p.predB.RR2 <- p.predB.RR +
    geom_point(data = dat.Positions, aes(x = lon.GK3, y = lat.GK3),
               pch = 19, colour = brewer.pal(11, "BrBG")[10],
               size = 2) +
    geom_text(data = dat.Positions, aes(x = lon.GK3, y = lat.GK3, label = c("1", "2", "3")),
              colour = brewer.pal(11, "BrBG")[10],
              nudge_x = -2500, nudge_y = 2500, size = 5, fontface = "bold"))

(p.predTI.RR2 <- p.predTI.RR +
    geom_point(data = dat.Positions, aes(x = lon.GK3, y = lat.GK3),
               pch = 19, colour = brewer.pal(11, "BrBG")[10],
               size = 2) +
    geom_text(data = dat.Positions, aes(x = lon.GK3, y = lat.GK3, label = c("1", "2", "3")),
              colour = brewer.pal(11, "BrBG")[10],
              nudge_x = -2500, nudge_y = 2500, size = 5, fontface = "bold"))

#png("../img/PredBackTrIndRRWithPointsSP.png", width = 900, height = 600)
ggarrange(p.predB.RR2, p.predTI.RR2, widths = c(1,1), common.legend = TRUE, legend = "bottom")
#dev.off()





###
### Figure B.9: Partial residual plots derived from parB ----
### parB refers to LUR model based on parametric polyonomials
### using data observed at background monitoring sites
###

rm(list = ls())

load("data/monSitesDE.rda")
dat	 <- monSitesDE[, c(1,2,4:7,9:24)]
datB <- dat[dat$AQeType == "background", ]

parB <- parLUR(data = datB
               ,x = c("Lon", "Lat", "Alt", "HighDens", "LowDens", "Ind", "Transp"
                      #				,"Seap", "Airp", "Constr"
                      ,"UrbGreen", "Agri", "Forest", "PopDens"
                      ,"PriRoad", "SecRoad", "FedAuto", "LocRoute")
               ,y = "Y"
               ,dirEff = c(0,0,-1,1,1,1,1,-1,0,-1,1,1,1,1,1)
               ,thresh = 0.95, thresh_pval = 0.1)


pred.parB <- predict(object = parB, newdata = datB, type = "terms")
pred.res.parB <- pred.parB + residuals(parB)

pred.res.parB.l <- reshape2::melt(pred.res.parB)
names(datB)
colnames(pred.res.parB)

datB.l <- reshape2::melt(subset(datB, select = names(coef(parB))[-1]))
names(datB.l)[2] <- "observed"

dat.scatter <- cbind(datB.l, pred.res.parB.l[,3])
names(dat.scatter)[3] <- "partial.residual"

str(dat.scatter)
dat.scatter$variable <- factor(dat.scatter$variable, levels(dat.scatter$variable)[c(1,2,6,5,3,4)])


#pdf("../img/PartialResidual_ggplot.pdf", width = 12, height = 7)
ggplot(data = dat.scatter, aes(x = observed, y = partial.residual, group = variable)) +
  theme_bw() +
  xlab("") +
  ylab("") +
  geom_point(col = "grey")+# brewer.pal(11, "BrBG")[7]) +
  geom_smooth(method = "lm", lty = "dashed", se = FALSE, #col = "royalblue",
              col = brewer.pal(11, "BrBG")[3], lwd = 1.2) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "tp"), se = FALSE,# col = "darkorange",
              col = brewer.pal(11, "BrBG")[10], lwd = 1.2) +
  facet_wrap(~variable, nrow = 2, scale = "free_x") +
  theme(axis.text = element_text(size = 14),
        strip.text = element_text(size = 14),
        plot.margin = unit(c(0,0.5,0,0), "cm"))
#dev.off()




###
### Figure B.10: Partial effects of smoothB ----
### smoothB refers to LUR model based on additive regression smoothers
### using data observed at background monitoring sites
###

rm(list = ls())

load("data/monSitesDE.rda")
dat	 <- monSitesDE[, c(1,2,4:7,9:24)]
datB <- dat[dat$AQeType == "background", ]

smoothB <- smoothLUR(data = datB
                     ,x = c("Lon", "Lat", "Alt", "HighDens", "LowDens", "Ind", "Transp"
                            #					,"Seap", "Airp", "Constr"
                            ,"UrbGreen", "Agri", "Forest", "PopDens"
                            , "PriRoad", "SecRoad", "FedAuto", "LocRoute")
                     ,spVar1 = "Lon"
                     ,spVar2 = "Lat"
                     ,y = "Y"
                     ,thresh = 0.95)


newdata.tmp <- data.frame(matrix(NA, nrow = 1000, ncol = 18))
dat.pred <- datB[, c(3:5,7:10,14:21)]
colnames(newdata.tmp) <- colnames(dat.pred)
for(j in 1:ncol(dat.pred)){
  newdata.tmp[,j] <- seq(min(dat.pred[,j]), max(dat.pred[,j]), length.out = 1000)
}

pred.tmpB <- predict(object = smoothB, newdata = newdata.tmp, se.fit = TRUE, type = "terms")

summary(smoothB)$s.table
rownames(summary(smoothB)$s.table)      # structure of coefficient table identical across models

vec.tmp <- c(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14)

sp.tmp <- rownames(summary(smoothB)$s.table)[vec.tmp]

edf.tmp <- round(summary(smoothB)$s.table[vec.tmp, "edf"], 2)

stripe.tmp <- paste(substr(sp.tmp, start = 1, stop = nchar(sp.tmp)-1),
                    ", ", edf.tmp, ")", sep = "")


pred.tmp2 <- pred.tmpB[[1]][,sp.tmp]
sd.tmp <- pred.tmpB[[2]][,sp.tmp]

conf.lower <- pred.tmp2 - 2*sd.tmp
conf.upper <- pred.tmp2 + 2*sd.tmp

newdata.tmp2 <- newdata.tmp[,substr(sp.tmp, start = 3, stop = nchar(sp.tmp)-1)]


# transform in long format
pred.tmp2.melt <- reshape2::melt(pred.tmp2)
conf.lower.melt <- reshape2::melt(conf.lower)
conf.upper.melt <- reshape2::melt(conf.upper)

pred.conf <- cbind(pred.tmp2.melt[,-1], conf.lower.melt[,3], conf.upper.melt[,3])
names(pred.conf) <- c("variable", "value", "lwr", "upr")

levels(pred.conf$variable)
levels(pred.conf$variable) <- stripe.tmp

newdata.tmp2.melt <- reshape2::melt(newdata.tmp2)
colnames(newdata.tmp2.melt) <- c("pred", "x")

dt.tmp <- cbind(pred.conf, newdata.tmp2.melt)


#pdf("../img/FittedSplines_ggplot_smoothB.pdf", width = 12, height = 12)
ggplot(data = dt.tmp, aes(x = x, y = value)) +
  theme_bw() +
  xlab("") +
  ylab("") +
  geom_line(col = brewer.pal(11, "BrBG")[10],# "royalblue",
            lwd = 0.7) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.25) +
  facet_wrap(~variable, nrow = 4, scales = "free_x") +
  theme(axis.text = element_text(size = 14),
        strip.text = element_text(size = 14))
#dev.off()





###
### Figure B.11: Interpolation maps derived from smoothB ----
### smoothB refers to LUR model based on additive regression smoothers
### using data observed at background monitoring sites
###

rm(list = ls())


# Load grid over Germany
load("Data_built/grid.DE.RData")
names(df.grid.DE)
df.grid.DE <- df.grid.DE[df.grid.DE$AGS!="01056025", ] # exclude Helgoland constituted of two small islands
names(df.grid.DE)[c(4,5)] <- c("Lon", "Lat")


load("data/monSitesDE.rda")
dat  	<- monSitesDE[, c(1,2,4:7,9:24)]
datB  <- dat[dat$AQeType == "background", ]
datTI <- dat[dat$AQeType != "background", ]


smoothA <- smoothLUR(data = dat
                     ,x = c("Lon", "Lat", "Alt", "HighDens", "LowDens", "Ind", "Transp"
                            #					,"Seap", "Airp", "Constr"
                            ,"UrbGreen", "Agri", "Forest", "PopDens"
                            , "PriRoad", "SecRoad", "FedAuto", "LocRoute")
                     ,spVar1 = "Lon"
                     ,spVar2 = "Lat"
                     ,y = "Y"
                     ,thresh = 0.95)

smoothB <- smoothLUR(data = datB
                     ,x = c("Lon", "Lat", "Alt", "HighDens", "LowDens", "Ind", "Transp"
                            #					,"Seap", "Airp", "Constr"
                            ,"UrbGreen", "Agri", "Forest", "PopDens"
                            , "PriRoad", "SecRoad", "FedAuto", "LocRoute")
                     ,spVar1 = "Lon"
                     ,spVar2 = "Lat"
                     ,y = "Y"
                     ,thresh = 0.95)

smoothTI <- smoothLUR(data = datTI
                      ,x = c("Lon", "Lat", "Alt", "HighDens", "LowDens", "Ind", "Transp"
                             #					,"Seap", "Airp", "Constr"
                             ,"UrbGreen", "Agri", "Forest", "PopDens"
                             , "PriRoad", "SecRoad", "FedAuto", "LocRoute")
                      ,spVar1 = "Lon"
                      ,spVar2 = "Lat"
                      ,y = "Y"
                      ,thresh = 0.95)

df.grid.DE$smoothA.sp	<- predict(object = smoothA, newdata = df.grid.DE, type="terms")[,"s(Lon,Lat)"] +
  predict(object = smoothA, newdata = df.grid.DE, type="terms")[,"s(Alt)"]

df.grid.DE$smoothB.sp	<- predict(object = smoothB, newdata = df.grid.DE, type="terms")[,"s(Lon,Lat)"] +
  predict(object = smoothB, newdata = df.grid.DE, type="terms")[,"s(Alt)"]

df.grid.DE$smoothTI.sp	<- predict(object = smoothTI, newdata = df.grid.DE, type="terms")[,"s(Lon,Lat)"] +
  predict(object = smoothB, newdata = df.grid.DE, type="terms")[,"s(Alt)"]

#common color scheme
(brks.sp <- seq(from = min(df.grid.DE$smoothA.sp, df.grid.DE$smoothB.sp, df.grid.DE$smoothTI.sp),
                to = max(df.grid.DE$smoothA.sp, df.grid.DE$smoothB.sp, df.grid.DE$smoothTI.sp),
                length.out = 11))
brks2.sp <- seq(-12, 8, 4)


#estimated spatial effect in smoothB (left plot in Figure B.11)
p.sp.effB <- ggplot(df.grid.DE, aes(x = Lon.GK3, y = Lat.GK3)) +
  ggmap::theme_nothing(legend = TRUE) +  #theme_bw() +
  xlab("") +
  ylab("") +
  coord_fixed(1) +
  geom_tile(aes(fill = smoothB.sp), width = 1000, height = 1000, na.rm = TRUE) +
  scale_fill_gradientn(name = "",
                       colours = brewer.pal(11, "BrBG")[-6],
                       breaks = brks2.sp,
                       labels = brks2.sp,
                       limits = range(brks.sp),
                       na.value = "white") +
  theme(legend.text  = element_text(size = 17),
        legend.background = element_blank(),
        legend.position = "bottom",
        legend.box = "vertical",
        axis.ticks = element_blank()) +
  guides(fill = guide_colourbar(barwidth = 20))





df.grid.DE$smoothA	<- as.vector(predict(object = smoothA, newdata = df.grid.DE))
df.grid.DE$smoothB	<- as.vector(predict(object = smoothB, newdata = df.grid.DE))
df.grid.DE$smoothTI	<- as.vector(predict(object = smoothTI, newdata = df.grid.DE))



#NO2 concentrations not supported by the underlying data
#all monitoring sites
length(df.grid.DE$smoothA[df.grid.DE$smoothA < min(dat$Y)]) # 951
length(df.grid.DE$smoothA[df.grid.DE$smoothA < min(dat$Y)])/length(df.grid.DE$smoothA)*100		# share in % of predicted values
df.grid.DE$smoothA[df.grid.DE$smoothA < min(dat$Y)]		<- min(dat$Y)				# replace by min contained in data set

length(df.grid.DE$smoothA[df.grid.DE$smoothA > max(dat$Y)]) # 0
length(df.grid.DE$smoothA[df.grid.DE$smoothA > max(dat$Y)])/length(df.grid.DE$smoothA)*100		# share in % of predicted values
df.grid.DE$smoothA[df.grid.DE$smoothA > max(dat$Y)]		<- max(dat$Y)				# replace by max contained in data set


#background
length(df.grid.DE$smoothB[df.grid.DE$smoothB < min(datB$Y)]) # 383
length(df.grid.DE$smoothB[df.grid.DE$smoothB < min(datB$Y)])/length(df.grid.DE$smoothB)*100		# share in % of predicted values
df.grid.DE$smoothB[df.grid.DE$smoothB < min(datB$Y)]		<- min(datB$Y)				# replace by min contained in data set

length(df.grid.DE$smoothB[df.grid.DE$smoothB > max(datB$Y)]) # 2
length(df.grid.DE$smoothB[df.grid.DE$smoothB > max(datB$Y)])/length(df.grid.DE$smoothB)*100		# share in % of predicted values
df.grid.DE$smoothB[df.grid.DE$smoothB > max(datB$Y)]		<- max(datB$Y)				# replace by max contained in data set


#traffic/industrial
length(df.grid.DE$smoothTI[df.grid.DE$smoothTI < min(datTI$Y)]) # 2923
length(df.grid.DE$smoothTI[df.grid.DE$smoothTI < min(datTI$Y)])/length(df.grid.DE$smoothTI)*100		# share in % of predicted values
df.grid.DE$smoothTI[df.grid.DE$smoothTI < min(datTI$Y)]		<- min(datTI$Y)				# replace by min contained in data set

length(df.grid.DE$smoothTI[df.grid.DE$smoothTI > max(datTI$Y)]) # 0
length(df.grid.DE$smoothTI[df.grid.DE$smoothTI > max(datTI$Y)])/length(df.grid.DE$smoothTI)*100		# share in % of predicted values
df.grid.DE$smoothTI[df.grid.DE$smoothTI > max(datTI$Y)]		<- max(datTI$Y)				# replace by max contained in data set


#common color scheme
(brks <- seq(from = min(df.grid.DE$smoothA, df.grid.DE$smoothB, df.grid.DE$smoothTI),
             to = max(df.grid.DE$smoothA, df.grid.DE$smoothB, df.grid.DE$smoothTI),
             length.out = 11))
brks2 <- seq(10, 70, 20)


###map visualizing conditional mean annual NO2 concentration levels derived from smoothA (right plot in Figure 5)
p.predB <- ggplot(df.grid.DE, aes(x = Lon.GK3, y = Lat.GK3)) +
  ggmap::theme_nothing(legend = TRUE) +
  xlab("") +
  ylab("") +
  coord_fixed(1) +
  geom_tile(aes(fill = smoothB), width = 1000, height = 1000, na.rm = TRUE) +
  scale_fill_gradientn(name = "",
                       colours = brewer.pal(9, "YlOrRd")[-1],
                       breaks = brks2,
                       labels = brks2,
                       limits = range(brks),
                       na.value = "white") +
  theme(legend.text  = element_text(size = 17),
        legend.background = element_blank(),
        legend.position = "bottom",
        legend.box = "vertical",
        axis.ticks = element_blank()) +
  guides(fill = guide_colourbar(barwidth = 20))



###complementing 'p.predB' by boundary of Rhine-Ruhr region

# Data on population density; BKG (Bundesamt für Kartographie und Geodäsie)
# German administrative regions at municipality level
sPdf.Municipalities <- readOGR(dsn = "DataFull/Data_BKG/vg250-ew_ebenen",
                               layer = "VG250_GEM",
                               encoding = "UTF-8",
                               use_iconv = TRUE)

# Read indicator vector for filtering AGS referring to Rhine-Ruhr region
ind.RR <- readRDS("indRhineRuhr.rds")

# Filter administrative regions referring to Rhine-Ruhr area
sPdf.Municipalities.RR    <- sPdf.Municipalities[sPdf.Municipalities$AGS %in% ind.RR, ]
sPdf.Municipalities.RR$ID <- "RR"

GK3 <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"

#boundary of RR region
admin.bndry.RR <- spTransform(sPdf.Municipalities.RR, GK3)
admin.bndry.RR.f <- fortify(admin.bndry.RR, region = "ID")
bndry.tmp <- admin.bndry.RR.f


names(bndry.tmp)[1:2] <- c("Lon.GK3", "Lat.GK3")
p.predB2 <- p.predB +
  geom_polygon(data = bndry.tmp, color = brewer.pal(11, "BrBG")[1], lwd = 0.7, fill = NA)



#png("../img/SpEff_PredB2.png", width = 900, height = 675)
#pdf("../img/SpEff_PredB2.pdf", width = 12, height = 9)
plot_grid(p.sp.effB, p.predB2, nrow = 1)
#dev.off()








###
### Figure B.12, right plot: s_{u,p}(PopDens) derived from smoothB ----
### smoothB referst to LUR model based on additive regression smoothers
### using data observed at background monitoring sites
###

rm(list = ls())

load("data/monSitesDE.rda")
dat	 <- monSitesDE[, c(1,2,4:7,9:24)]
datB <- dat[dat$AQeType == "background", ]

smoothB <- smoothLUR(data = datB
                     ,x = c("Lon", "Lat", "Alt", "HighDens", "LowDens", "Ind", "Transp"
                            #					,"Seap", "Airp", "Constr"
                            ,"UrbGreen", "Agri", "Forest", "PopDens"
                            , "PriRoad", "SecRoad", "FedAuto", "LocRoute")
                     ,spVar1 = "Lon"
                     ,spVar2 = "Lat"
                     ,y = "Y"
                     ,thresh = 0.95)


newdata.tmp <- data.frame(matrix(NA, nrow = 1000, ncol = 18))
dat.pred <- datB[, c(3:5,7:10,14:21)]
colnames(newdata.tmp) <- colnames(dat.pred)
for(j in 1:ncol(dat.pred)){
  newdata.tmp[,j] <- seq(min(dat.pred[,j]), max(dat.pred[,j]), length.out = 1000)
}
pred.tmpB   <- predict(object = smoothB, newdata = newdata.tmp, type = "terms")[,"s(PopDens)"]

summary(smoothB)$s.table
rownames(summary(smoothB)$s.table)      # structure of coefficient table identical across models

sp.tmp     <- rownames(summary(smoothB)$s.table)[10]
edf.tmp    <- round(summary(smoothB)$s.table[10, "edf"], 2)
stripe.tmp <- paste(substr(sp.tmp, start = 1, stop = nchar(sp.tmp)-1),
                    ", ", edf.tmp, ")", sep = "")

df.tmp <- data.frame(x = newdata.tmp[,"PopDens"], value = pred.tmpB, variable = stripe.tmp)


dat.Positions <- readRDS("dat.Positions.rds")
df.points <- data.frame(x = dat.Positions[1:2, "PopDens"],
                        value = c(predict(object = smoothB,
                                          newdata = dat.Positions[1:2, ], type="terms")[,"s(PopDens)"]))

p.spline.popDens <- ggplot(data = df.tmp, aes(x = x, y = value)) +
  theme_bw() +
  ylim(-5,10) +
  xlab("") +
  ylab("") +
  geom_line(col = brewer.pal(11, "BrBG")[10],#"blue",
            lwd = 1) +
  geom_point(data = df.points, col = brewer.pal(9, "YlOrRd")[5],# "orangered",
             size = 2.5) +
  geom_text(data = df.points, label = c("1", "2"),
            colour = brewer.pal(9, "YlOrRd")[5],# "orangered",
            nudge_x = -60, nudge_y = 0.5, size = 5, fontface = "bold") +
  facet_wrap(~variable) +
  theme(axis.text = element_text(size = 14),
        strip.text = element_text(size = 14))



#pdf("../img/PartialEff_smoothB_PopDens.pdf", width = 6, height = 4)
p.spline.popDens
#dev.off()



###
### Figure B.12, left plot: Estimated spatial effect in smoothB with two locations ----
### smoothA (smoothB) refers to LUR model based on additive regression smoothers
### using data observed at all (background) monitoring sites
###

rm(list = ls())


# Load grid over Germany
load("Data_built/grid.DE.RData")
names(df.grid.DE)
df.grid.DE <- df.grid.DE[df.grid.DE$AGS!="01056025", ] # exclude Helgoland constituted of two small islands
names(df.grid.DE)[c(4,5)] <- c("Lon", "Lat")

load("data/monSitesDE.rda")
dat  	<- monSitesDE[, c(1,2,4:7,9:24)]
datB  <- dat[dat$AQeType == "background", ]
datTI <- dat[dat$AQeType != "background", ]


smoothA <- smoothLUR(data = dat
                     ,x = c("Lon", "Lat", "Alt", "HighDens", "LowDens", "Ind", "Transp"
                            #					,"Seap", "Airp", "Constr"
                            ,"UrbGreen", "Agri", "Forest", "PopDens"
                            , "PriRoad", "SecRoad", "FedAuto", "LocRoute")
                     ,spVar1 = "Lon"
                     ,spVar2 = "Lat"
                     ,y = "Y"
                     ,thresh = 0.95)

smoothB <- smoothLUR(data = datB
                     ,x = c("Lon", "Lat", "Alt", "HighDens", "LowDens", "Ind", "Transp"
                            #					,"Seap", "Airp", "Constr"
                            ,"UrbGreen", "Agri", "Forest", "PopDens"
                            , "PriRoad", "SecRoad", "FedAuto", "LocRoute")
                     ,spVar1 = "Lon"
                     ,spVar2 = "Lat"
                     ,y = "Y"
                     ,thresh = 0.95)

smoothTI <- smoothLUR(data = datTI
                      ,x = c("Lon", "Lat", "Alt", "HighDens", "LowDens", "Ind", "Transp"
                             #					,"Seap", "Airp", "Constr"
                             ,"UrbGreen", "Agri", "Forest", "PopDens"
                             , "PriRoad", "SecRoad", "FedAuto", "LocRoute")
                      ,spVar1 = "Lon"
                      ,spVar2 = "Lat"
                      ,y = "Y"
                      ,thresh = 0.95)


df.grid.DE$smoothA.sp	<- predict(object = smoothA, newdata = df.grid.DE, type="terms")[,"s(Lon,Lat)"] +
  predict(object = smoothA, newdata = df.grid.DE, type="terms")[,"s(Alt)"]

df.grid.DE$smoothB.sp	<- predict(object = smoothB, newdata = df.grid.DE, type="terms")[,"s(Lon,Lat)"] +
  predict(object = smoothB, newdata = df.grid.DE, type="terms")[,"s(Alt)"]

df.grid.DE$smoothTI.sp	<- predict(object = smoothTI, newdata = df.grid.DE, type="terms")[,"s(Lon,Lat)"] +
  predict(object = smoothB, newdata = df.grid.DE, type="terms")[,"s(Alt)"]

#common color scheme
(brks.sp <- seq(from = min(df.grid.DE$smoothA.sp, df.grid.DE$smoothB.sp, df.grid.DE$smoothTI.sp),
                to = max(df.grid.DE$smoothA.sp, df.grid.DE$smoothB.sp, df.grid.DE$smoothTI.sp),
                length.out = 11))
brks2.sp <- seq(-12, 8, 4)


#background
p.sp.effB <- ggplot(df.grid.DE, aes(x = Lon.GK3, y = Lat.GK3)) +
  ggmap::theme_nothing(legend = TRUE) +  #theme_bw() +
  xlab("") +
  ylab("") +
  coord_fixed(1) +
  geom_tile(aes(fill = smoothB.sp), width = 1000, height = 1000, na.rm = TRUE) +
  scale_fill_gradientn(name = "",
                       colours = brewer.pal(11, "BrBG")[-6],
                       breaks = brks2.sp,
                       labels = brks2.sp,
                       limits = range(brks.sp),
                       na.value = "white") +
  theme(legend.text  = element_text(size = 17),
        legend.background = element_blank(),
        legend.position = "bottom",
        legend.box = "vertical",
        axis.ticks = element_blank()) +
  guides(fill = guide_colourbar(barwidth = 20))



dat.Positions <- readRDS("dat.Positions.rds")
df.grid2points	<- dat.Positions[1:2, c("lon.GK3", "lat.GK3")]

p.sp.effB2 <- p.sp.effB +
  geom_point(data = df.grid2points, aes(x = lon.GK3, y = lat.GK3),
             pch = 19, colour =  brewer.pal(9, "YlOrRd")[5], #"orangered",
             size = 2) +
  geom_text(data = df.grid2points, aes(x = lon.GK3, y = lat.GK3, label = c("1", "2")),
            colour =  brewer.pal(9, "YlOrRd")[5],# "orangered",
            nudge_x = -10000, nudge_y = 10000, size = 5, fontface = "bold")


#png("../img/SpatialEffect2_B.png", width = 450, height = 675)
#pdf("../img/SpatialEffect2_B.pdf", width = 6, height = 9)
p.sp.effB2
#dev.off()
