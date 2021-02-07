#######################################################
### Figures and tables for exploratory data analysis
#######################################################



## Load packages
library(data.table)
library(ggplot2)
library(RColorBrewer)
library(rgdal)
library(rgeos)
library(sp)
library(cowplot)



###
### Fig.1: Map of Germany and Rhine-Ruhr metropolitan area (PopDens at municipality key level and locations of monitoring sites)
###

rm(list = ls())

load("data/monSitesDE.rda")

dat  	<- monSitesDE[, c(2,4:7,9:24)]
datB  <- dat[dat$AQeType == "background", ]
datT  <- dat[dat$AQeType == "traffic", ]
datI  <- dat[dat$AQeType == "industrial",]


dat$AQeType <- factor(dat$AQeType)

p.MonSites <- ggplot(data=dat,aes(x=Lon,y=Lat,color=Y))+
  geom_point(aes(shape=AQeType), size = 2)+
  coord_equal()+
  theme_bw()+
  xlab("Lon (in WGS84)")+
  ylab("Lat (in WGS84)")+
  scale_fill_gradientn(name = "Y",
                       colours = brewer.pal(11, "BrBG")[-c(1:7)],
                       breaks = seq(15, 75, 15),
                       labels = seq(15, 75, 15),
                       limits = c(min(dat$Y),max(dat$Y)),
                       aesthetics = "colour")+
  theme(legend.title = element_text(size = 16),
        legend.text  = element_text(size = 16),
        legend.background = element_blank(),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16)) +
  guides(color = guide_colourbar(barheight = 14, barwidth = 2))


# pdf("../img/MonSitesTypeNO2Level.pdf", width = 13, height = 8)
p.MonSites
# dev.off()



load("data/Data_built/grid.DE.RData")


# Derive SpatialPointsDataFrame from 'df.grid.DE'
spdf.DE <- SpatialPointsDataFrame(coords = cbind(df.grid.DE$Lon.WGS84, df.grid.DE$Lat.WGS84),
                                  data = df.grid.DE,
                                  proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))




GK3 <- CRS("+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")

spdf.sites  <- SpatialPointsDataFrame(coords = cbind(dat$Lon, dat$Lat),
                                       data = dat,
                                       proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
spdf.sites2 <- spTransform(spdf.sites, GK3)

proj4string(spdf.DE)
spdf.DE2 <- spTransform(spdf.DE, GK3)
proj4string(spdf.DE2)

dat.tmp <- spdf.DE2@data
dat.tmp$long <- coordinates(spdf.DE2)[,1]
dat.tmp$lat  <- coordinates(spdf.DE2)[,2]

(p.DE.Alt <- ggplot(dat.tmp, aes(x = long, y = lat)) +
    ggmap::theme_nothing(legend = TRUE) +
    xlab("") +
    ylab("") +
    coord_fixed(1) +
    geom_tile(aes(fill = Alt), width = 1000, height = 1000, na.rm = TRUE)+
    scale_fill_gradientn(name = "Alt",
                         colours = rev(brewer.pal(11, "BrBG")[c(1:4)]),
                         breaks = seq(0,2500,500),
                         labels = seq(0,2500,500),
                         limits = c(min(dat.tmp$Alt),max(dat.tmp$Alt)),
                         na.value = "white") +
    theme(legend.text  = element_text(size = 17),
          legend.title  = element_text(size = 17),
          legend.background = element_blank(),
          legend.position = "bottom",
          legend.box = "vertical",
          axis.ticks = element_blank()) +
    guides(fill = guide_colourbar(barwidth = 20)))




(p.DE.PopDens <- ggplot(dat.tmp, aes(x = long, y = lat)) +
    ggmap::theme_nothing(legend = TRUE) +
    xlab("") +
    ylab("") +
    coord_fixed(1) +
    geom_tile(aes(fill = PopDens), width = 1000, height = 1000, na.rm = TRUE)+
    scale_fill_gradientn(name = "PopDens",
                         colours = brewer.pal(11, "BrBG")[-c(1:7)],
                         breaks = seq(1000, 4000, 1000),
                         labels = seq(1000, 4000, 1000),
                         limits = range(dat.tmp$PopDens),
                         na.value = "white") +
    theme(legend.text  = element_text(size = 17),
          legend.title  = element_text(size = 17),
          legend.background = element_blank(),
          legend.position = "bottom",
          legend.box = "vertical",
          axis.ticks = element_blank()) +
    guides(fill = guide_colourbar(barwidth = 20)))



# png("../img/GridAltPopDens.png",width = 900, height = 675)
ggdraw() +
  draw_plot(p.DE.Alt, 0, 0, 0.5, 1) +
  draw_plot(p.DE.PopDens, 0.5, 0, 0.5, 1)
# dev.off()


