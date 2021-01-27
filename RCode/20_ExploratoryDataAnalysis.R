#######################################################
### Figures and tables for exploratory data analysis
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
### Fig.1: Map of Germany and Rhine-Ruhr metropolitan area (PopDens at municipality key level and locations of monitoring sites)
###

rm(list = ls())


# Data on population density; BKG (Bundesamt für Kartographie und Geodäsie)
# German administrative regions at municipality level
sPdf.Municipalities <- readOGR(dsn = "data/Data_BKG/vg250-ew_ebenen",
                               layer = "VG250_GEM",
                               encoding = "UTF-8",
                               use_iconv = TRUE)


load("data/Data_built/grid.DE.RData")
load("data/monSitesDE.rda")

dat  	<- monSitesDE[, c(2,4:7,9:24)]
datB  <- dat[dat$AQeType == "background", ]
datTI <- dat[dat$AQeType != "background", ]


spdf.sites.back   <- SpatialPointsDataFrame(coords = cbind(datB$Lon, datB$Lat),
                                            data = datB,
                                            proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))


spdf.sites.tr.ind <- SpatialPointsDataFrame(coords = cbind(datTI$Lon, datTI$Lat),
                                            data = datTI,
                                            proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))



# Derive SpatialPointsDataFrame from 'df.grid.DE'
spdf.DE <- SpatialPointsDataFrame(coords = cbind(df.grid.DE$Lon.WGS84, df.grid.DE$Lat.WGS84),
                                  data = df.grid.DE,
                                  proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))




# Read indicator vector for filtering AGS referring to Rhine-Ruhr region
ind.RR <- readRDS("indRhineRuhr.rds")

spdf.RR                   <- spdf.DE[spdf.DE$AGS %in% ind.RR, ]
sPdf.Municipalities.RR    <- sPdf.Municipalities[sPdf.Municipalities$AGS %in% ind.RR, ]
sPdf.Municipalities.RR$ID <- "RR"

col.range <- range(spdf.DE$PopDens)

GK3 <- CRS("+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")
spdf.RR2 <- spTransform(x = spdf.RR, CRSobj = GK3)

spdf.sites.tr.ind.2 <- spTransform(spdf.sites.tr.ind, GK3)
spdf.sites.back.2   <- spTransform(spdf.sites.back, GK3)

sPdf.Municipalities.RR2 <- spTransform(sPdf.Municipalities.RR, GK3)
ind.tmp  <- which(!is.na(over(spdf.sites.tr.ind.2, sPdf.Municipalities.RR2)[,1]))
ind.tmp2 <- which(!is.na(over(spdf.sites.back.2, sPdf.Municipalities.RR2)[,1]))

spdf.sites.tr.ind.RR <- spdf.sites.tr.ind.2[ind.tmp, ]
spdf.sites.back.RR   <- spdf.sites.back.2[ind.tmp2, ]

dat.tmp.RR <- spdf.RR2@data
dat.tmp.RR$long <- coordinates(spdf.RR2)[,1]
dat.tmp.RR$lat  <- coordinates(spdf.RR2)[,2]


dat.tmp <- dat.tmp.RR
spdf.tmp1 <- spdf.sites.back.RR
spdf.tmp2 <- spdf.sites.tr.ind.RR


(p.RR <- ggplot(dat.tmp, aes(x = long, y = lat)) +
    ggmap::theme_nothing(legend = TRUE) + #theme_bw() +
    xlab("") +
    ylab("") +
    coord_fixed(1) +
    geom_tile(aes(fill = PopDens), width = 1000, height = 1000, na.rm = TRUE)+
    geom_point(data = spdf.tmp1@data,
               aes(x = coordinates(spdf.tmp1)[,1],
                   y = coordinates(spdf.tmp1)[,2],
                   colour = brewer.pal(9, "BrBG")[1]),
               size = 1, shape = 15) +
    geom_point(data = spdf.tmp2@data,
               aes(x = coordinates(spdf.tmp2)[,1],
                   y = coordinates(spdf.tmp2)[,2],
                   colour = brewer.pal(9, "BrBG")[3]),
               size = 1, shape = 15) +
    scale_colour_manual(values = brewer.pal(9, "BrBG")[c(1,3)],
                        name = "Type of monitoring site",
                        labels = c("background", "traffic/industrial")) +
    scale_fill_gradientn(name = "PopDens",
                         colours = brewer.pal(11, "BrBG")[-c(1:7)],
                         #                       colours = brewer.pal(9, "Blues")[-c(1,2)],
                         breaks = seq(1000, 4000, 1000),
                         labels = seq(1000, 4000, 1000),
                         limits = col.range,
                         na.value = "white") +
    theme(legend.title = element_text(size = 17),
          legend.text  = element_text(size = 17),
          legend.background = element_blank(),
          legend.position = "bottom",
          legend.box = "vertical",
          axis.ticks = element_blank()) +
    guides(fill = guide_colourbar(barwidth = 20),
           color = guide_legend(order = 1)))


(p.RR2 <- p.RR +
    ggsn::scalebar(dat.tmp.RR, dist = 20, st.size = 3, transform = FALSE,
                   dist_unit = "km", model = "WGS84", st.color = brewer.pal(11, "BrBG")[3],
                   box.fill = c(brewer.pal(11, "BrBG")[3],"white"),
                   box.color = brewer.pal(11, "BrBG")[3],
                   border.size = 0.5))

proj4string(spdf.DE)
spdf.DE2 <- spTransform(spdf.DE, GK3)
proj4string(spdf.DE2)

sPdf.bndry.RR <- spTransform(sPdf.Municipalities.RR, GK3)
sPdf.bndry.RR.f <- fortify(sPdf.bndry.RR, region = "ID")

dat.tmp <- spdf.DE2@data
dat.tmp$long <- coordinates(spdf.DE2)[,1]
dat.tmp$lat  <- coordinates(spdf.DE2)[,2]


bndry.tmp <- sPdf.bndry.RR.f

p.DE <- ggplot(dat.tmp, aes(x = long, y = lat)) +
  ggmap::theme_nothing() +  #theme_bw() +
  xlab("") +
  ylab("") +
  coord_fixed(1) +
  geom_tile(aes(fill = PopDens), width = 1000, height = 1000, na.rm = TRUE)+
  geom_point(data = spdf.sites.back.2@data,
             aes(x = coordinates(spdf.sites.back.2)[,1],
                 y = coordinates(spdf.sites.back.2)[,2],
                 colour = brewer.pal(9, "BrBG")[1]),
             size = 1, shape = 15) +
  geom_point(data = spdf.sites.tr.ind.2@data,
             aes(x = coordinates(spdf.sites.tr.ind.2)[,1],
                 y = coordinates(spdf.sites.tr.ind.2)[,2],
                 colour = brewer.pal(9, "BrBG")[3]),
             size = 1, shape = 15) +
  geom_polygon(data = bndry.tmp, color = brewer.pal(11, "BrBG")[1], lwd = 0.7, fill = NA) +
  scale_fill_gradientn(name = "PopDens",
                       colours = brewer.pal(11, "BrBG")[-c(1:7)],
                       breaks = seq(1000, 4000, 1000),
                       labels = seq(1000, 4000, 1000),
                       limits = col.range,
                       na.value = "white") +
  scale_colour_manual(values = brewer.pal(9, "BrBG")[c(1,3)],
                      name = "Type of monitoring site",
                      labels = c("background", "traffic/industrial")) +
  theme(legend.title = element_text(size = 14),
        legend.text  = element_text(size = 14),
        legend.background = element_blank(),
        axis.ticks = element_blank()) +
  guides(fill = guide_colourbar(barwidth = 15))



p.DE2 <- p.DE +
  ggsn::scalebar(dat.tmp, dist = 100, st.size = 3, transform = FALSE,
                 dist_unit = "km", model = "WGS84", st.color = brewer.pal(11, "BrBG")[3],
                 box.fill = c(brewer.pal(11, "BrBG")[3],"white"),
                 box.color = brewer.pal(11, "BrBG")[3],
                 border.size = 0.5)



# png("../img/MonitoringSitesPopDens_RR_3.png", width = 900, height = 675)
# #pdf("../img/MonitoringSitesPopDens_RR_3.pdf", width = 13, height = 8)
ggdraw() +
 draw_plot(p.DE2, 0, 0, 0.5, 1) +
 draw_plot(p.RR2, 0.5, 0.05, 0.5, 0.9)
# dev.off()




###
### Tab.2: Tukey's Five and Mean ----
###

rm(list = ls())

load("data/monSitesDE.rda")
dat	<- monSitesDE[, c(2,4:7,9:24)]

datB  <- dat[dat$AQeType == "background", ]
datTI <- dat[dat$AQeType != "background", ]

nrow(dat); mean(dat$Y); sd(dat$Y); fivenum(dat$Y)
nrow(datB); mean(datB$Y); sd(datB$Y); fivenum(datB$Y)
nrow(datTI); mean(datTI$Y); sd(datTI$Y); fivenum(datTI$Y)




###
### Fig.2: Histogram with empirical density curve ----
###


rm(list = ls())

load("data/monSitesDE.rda")
dat	<- monSitesDE[, c(2,4:7,9:24)]

datB  <- dat[dat$AQeType == "background", ]
datTI <- dat[dat$AQeType != "background", ]

dat.all <- data.frame(Y = c(datB$Y, datTI$Y),
                      type = c(rep("Background", 246), rep("Traffic/Industrial", 157)))

(p.hist <- ggplot(dat.all, aes(x = Y, fill = type, color = type)) +
  theme_minimal() +
  theme_classic() +
  scale_x_continuous(breaks = seq(0, 90, by = 15), limits = c(-5, 95)) +
  geom_histogram(aes(y=..density..), fill = "white", position = "identity", alpha = 0.5, binwidth = 5, lwd = 1.4) +
  geom_density(alpha = 0.6, lwd = 1.2) +
  xlab(expression(paste(NO[2], " concentration level in ", mu, "g/", m^3, sep = ""))) +
  ylab("empirical density") +
  scale_color_manual(values = brewer.pal(9, "BrBG")[c(1,3)],
                       aesthetics = c("fill", "colour"))+
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 18),
        legend.title = element_blank(),
        legend.text = element_text(size = 18),
        legend.position = c(0.75, 0.9)))


# pdf("../img/HistogramDensitiesBackTrInd.pdf", height = 6, width = 9)
# p.hist
# dev.off()




###
### Fig.3: Pairwise Bravais-Pearson correlations ----
###

names(dat)
corr.DE <- round(cor(dat[,c(2:4, 6:9, 13:20)]), digits = 2)
corr.DE[lower.tri(corr.DE)] <- NA
corr.melt <- reshape2::melt(corr.DE, na.rm = TRUE)


(p.corr <- ggplot(data = corr.melt, aes(Var2, Var1, fill = value))+
    geom_tile(color = "white")+
    xlab("") +
    ylab("") +
    scale_fill_gradientn(name = "",
                         colours = brewer.pal(11, "BrBG")[-6],
                         breaks = c(-1, -0.5, 0, 0.5, 1),
                         labels = c("-1", "-0.5", "0", "0.5", "1"),
                         limits = c(-1,1),
                         na.value = "white") +
    theme_minimal() +
    coord_fixed() +
    # geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                     size = 14, hjust = 1),
          axis.text.y = element_text(size = 14),
          axis.ticks = element_blank(),
          legend.title = element_blank(),
          legend.justification = c(1, 0),
          legend.position = c(0.6, 0.8),
          legend.direction = "horizontal")+
    guides(fill = guide_colorbar(barwidth = 10, barheight = 1.5,
                                 title.position = "top", title.hjust = 0.5))
)



# pdf("../img/CorrMatrix.pdf", width = 8, height = 8)
# p.corr
# dev.off()


