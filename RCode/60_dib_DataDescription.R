#######################################################
### Figures and tables for exploratory data analysis
#######################################################



## Load packages
library(data.table)
library(ggplot2)
library(RColorBrewer)
library(sp)
library(cowplot)




###
### Table 3: Descriptives of continous variables in monSitesDE ----
###


rm(list = ls())

load("data/monSitesDE.rda")
dat	<- monSitesDE

MEAN <- round(apply(dat[,c(2,7,10:19,21:25)],
                    MARGIN = 2,
                    mean), 2)

SD <- round(apply(dat[,c(2,7,10:19,21:25)],
                  MARGIN = 2,
                  sd), 2)

FIVENUM <- round(apply(dat[,c(2,7,10:19,21:25)],
                       MARGIN = 2,
                       fivenum), 2)




###
### Fig.1: Boxplots of continuous variables in monSitesDE depending AQeType ----
###

rm(list = ls())

load("data/monSitesDE.rda")
dat	<- monSitesDE

dat.long <- melt(dat[,c(2,7,10:19,21:25)])
dat.long$AQeType <- dat$AQeType

p.box <- ggplot(data = dat.long, aes(x = variable, y = value, fill = AQeType, color = AQeType)) +
  theme_bw() +
  xlab("") +
  ylab("") +
  geom_boxplot(fill=NA,lwd=0.75) +
  facet_wrap(~variable,scale="free",nrow = 6)+
  scale_color_manual(values = brewer.pal(9, "BrBG")[c(1,2,3)],
                     aesthetics = c("fill", "colour"))+
  theme(axis.text.y = element_text(size = 14),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        strip.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 16),
        legend.position = c(0.8, 0.075))

pdf("../img/BoxplotsVariablesMonSitesDE.pdf",width = 12, height = 9)
p.box
dev.off()


dat.long2 <- rbind(dat.long[,c(1:2)],dat.long[,c(1:2)])
dat.long2$AQeType <- NA
dat.long2$AQeType[1:6851] <- dat.long[,3]
dat.long2$AQeType[6852:13702] <- "All"

p.box2 <- ggplot(data = dat.long2, aes(x = variable, y = value, fill = AQeType, color = AQeType)) +
  theme_bw() +
  xlab("") +
  ylab("") +
  geom_boxplot(fill=NA,lwd=0.75) +
  facet_wrap(~variable,scale="free",nrow = 6)+
  scale_color_manual(values = brewer.pal(9, "BrBG")[c(9,1,2,3)],
                     aesthetics = c("fill", "colour"))+
  theme(axis.text.y = element_text(size = 14),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        strip.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.position = c(0.8, 0.075))

pdf("../img/BoxplotsVariablesMonSitesDE2.pdf",width = 12, height = 9)
p.box2
dev.off()



###
### Fig.2: Locations of monitoring sites and concentration levels in monSitesDE ----
###

rm(list = ls())

load("data/monSitesDE.rda")

dat  	<- monSitesDE
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
                       colours = rev(brewer.pal(11, "BrBG")[c(1:5)]),
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


pdf("../img/MonSitesTypeNO2Level.pdf", width = 12, height = 8)
p.MonSites
dev.off()







###
### Fig.3: Interpolation maps of variables in gridDE ----
###

rm(list = ls())

load("data/Data_built/grid.DE.RData")


# Derive SpatialPointsDataFrame from 'df.grid.DE'
spdf.DE <- SpatialPointsDataFrame(coords = cbind(df.grid.DE$Lon.WGS84, df.grid.DE$Lat.WGS84),
                                  data = df.grid.DE,
                                  proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))




GK3 <- CRS("+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")

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
                         colours = rev(brewer.pal(11, "BrBG")[c(1:5)]),
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



(p.DE.LowDens <- ggplot(dat.tmp, aes(x = long, y = lat)) +
    ggmap::theme_nothing(legend = TRUE) +
    xlab("") +
    ylab("") +
    coord_fixed(1) +
    geom_tile(aes(fill = LowDens), width = 1000, height = 1000, na.rm = TRUE)+
    scale_fill_gradientn(name = "LowDens",
                         colours =  rev(brewer.pal(11, "BrBG")[c(1:5)]),
                         breaks = seq(0,1,0.2),
                         labels = seq(0,1,0.2),
                         limits = c(min(dat.tmp$LowDens),max(dat.tmp$LowDens)),
                         na.value = "white") +
    theme(legend.text  = element_text(size = 17),
          legend.title  = element_text(size = 17),
          legend.background = element_blank(),
          legend.position = "bottom",
          legend.box = "vertical",
          axis.ticks = element_blank()) +
    guides(fill = guide_colourbar(barwidth = 20)))



(p.DE.Agri <- ggplot(dat.tmp, aes(x = long, y = lat)) +
    ggmap::theme_nothing(legend = TRUE) +
    xlab("") +
    ylab("") +
    coord_fixed(1) +
    geom_tile(aes(fill = Agri), width = 1000, height = 1000, na.rm = TRUE)+
    scale_fill_gradientn(name = "Agri",
                         colours = rev(brewer.pal(11, "BrBG")[c(1:5)]),
                         breaks = seq(0,1,0.2),
                         labels = seq(0,1,0.2),
                         limits = c(min(dat.tmp$Agri),max(dat.tmp$Agri)),
                         na.value = "white") +
    theme(legend.text  = element_text(size = 17),
          legend.title  = element_text(size = 17),
          legend.background = element_blank(),
          legend.position = "bottom",
          legend.box = "vertical",
          axis.ticks = element_blank()) +
    guides(fill = guide_colourbar(barwidth = 20)))



(p.DE.Forest <- ggplot(dat.tmp, aes(x = long, y = lat)) +
    ggmap::theme_nothing(legend = TRUE) +
    xlab("") +
    ylab("") +
    coord_fixed(1) +
    geom_tile(aes(fill = Forest), width = 1000, height = 1000, na.rm = TRUE)+
    scale_fill_gradientn(name = "Forest",
                         colours = rev(brewer.pal(11, "BrBG")[c(1:5)]),
                         breaks = seq(0,1,0.2),
                         labels = seq(0,1,0.2),
                         limits = c(min(dat.tmp$Forest),max(dat.tmp$Forest)),
                         na.value = "white") +
    theme(legend.text  = element_text(size = 17),
          legend.title  = element_text(size = 17),
          legend.background = element_blank(),
          legend.position = "bottom",
          legend.box = "vertical",
          axis.ticks = element_blank()) +
    guides(fill = guide_colourbar(barwidth = 20)))



(p.DE.PriRoad <- ggplot(dat.tmp, aes(x = long, y = lat)) +
    ggmap::theme_nothing(legend = TRUE) +
    xlab("") +
    ylab("") +
    coord_fixed(1) +
    geom_tile(aes(fill = PriRoad), width = 1000, height = 1000, na.rm = TRUE)+
    scale_fill_gradientn(name = "PriRoad",
                         colours = rev(brewer.pal(11, "BrBG")[c(1:5)]),
                         breaks = seq(0,7000,1400),
                         labels = seq(0,7000,1400),
                         limits = c(min(dat.tmp$PriRoad),max(dat.tmp$PriRoad)),
                         na.value = "white") +
    theme(legend.text  = element_text(size = 17),
          legend.title  = element_text(size = 17),
          legend.background = element_blank(),
          legend.position = "bottom",
          legend.box = "vertical",
          axis.ticks = element_blank()) +
    guides(fill = guide_colourbar(barwidth = 20)))



(p.DE.FedAuto <- ggplot(dat.tmp, aes(x = long, y = lat)) +
    ggmap::theme_nothing(legend = TRUE) +
    xlab("") +
    ylab("") +
    coord_fixed(1) +
    geom_tile(aes(fill = FedAuto), width = 1000, height = 1000, na.rm = TRUE)+
    scale_fill_gradientn(name = "FedAuto",
                         colours = rev(brewer.pal(11, "BrBG")[c(1:5)]),
                         breaks = seq(0,5000,1000),
                         labels = seq(0,5000,1000),
                         limits = c(min(dat.tmp$FedAuto),max(dat.tmp$FedAuto)),
                         na.value = "white") +
    theme(legend.text  = element_text(size = 17),
          legend.title  = element_text(size = 17),
          legend.background = element_blank(),
          legend.position = "bottom",
          legend.box = "vertical",
          axis.ticks = element_blank()) +
    guides(fill = guide_colourbar(barwidth = 20)))



png("../img/GridVariables.png",width = 900, height = 1500)
pdf("../img/GridVariables.pdf",width = 12, height = 20)
ggdraw() +
  draw_plot(p.DE.Alt, 0, 0.66, 0.5, 0.33) +
  draw_plot(p.DE.LowDens, 0.5, 0.66, 0.5, 0.33) +
  draw_plot(p.DE.Agri, 0,0.33,0.5,0.33) +
  draw_plot(p.DE.Forest, 0.5,0.33,0.5,0.33) +
  draw_plot(p.DE.PriRoad, 0,0,0.5,0.33) +
  draw_plot(p.DE.FedAuto, 0.5,0,0.5,0.33)
dev.off()
