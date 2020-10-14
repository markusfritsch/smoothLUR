##################################################################################
### Creating and exporting figures and tables for presentation slides and paper
##################################################################################

# To obtain the same ratio when using pdf instead of png, divide height and width (used to produce png) by 75


#	setwd("D:/Work/20_Projekte/570_Behm-and-Fritsch/smoothLUR")


rm(list = ls())

## Load packages ----
library(smoothLUR)
library(xlsx)
library(rgdal)
library(raster)
library(RColorBrewer)
library(data.table)
library(mgcv)
library(ggplot2)
library(ggsn)        # for scalebar
library(cowplot)     # for plot_grid(), draw_plot()
library(RgoogleMaps) # for qbbox()
library(sp)
library(rgeos)
library(car)          # required for functions crp() and adaptiveKernel()
library(smoothLUR)
library(xtable)





load("data/monSitesDE.rda")
dat	<- monSitesDE[, c(2,4:7,9:24)]








###
### Some descriptives ----
###



dat.B  <- dat[dat$AQeType == "background", ]
dat.TI <- dat[dat$AQeType != "background", ]

# Tukey's Five and Mean
nrow(dat.B); mean(dat.B$Y); sd(dat.B$Y); fivenum(dat.B$Y)
nrow(dat.TI); mean(dat.TI$Y); sd(dat.TI$Y); fivenum(dat.TI$Y)
nrow(dat); mean(dat$Y); sd(dat$Y); fivenum(dat$Y)



dat.all <- data.frame(Y = c(dat.B$Y, dat.TI$Y),
                      type = c(rep("Background", 246), rep("Traffic/Industrial", 157)))

dat.all2 <- data.frame(Y = rep(c(dat.B$Y, dat.TI$Y),2),
                       type = c(rep("Background", 246), rep("Traffic/Industrial", 157), rep("All", 403)))


# Histograms with empirical density curve
(p.hist <- ggplot(dat.all, aes(x = Y, fill = type, color = type)) +
  theme_minimal() +
  theme_classic() +
  scale_x_continuous(breaks = seq(0, 90, by = 15), limits = c(-5, 95)) +
  geom_histogram(aes(y=..density..), fill = "white", position = "identity", alpha = 0.5, binwidth = 5, lwd = 1.4) +
  geom_density(alpha = 0.6, lwd = 1.2) +
  xlab(expression(paste(NO[2], " concentration level in ", mu, "g/", m^3, sep = ""))) +
  ylab("empirical density") +
#  scale_color_brewer(palette = "Dark2",
 #                    aesthetics = c("fill", "colour")) +
    scale_color_manual(values = brewer.pal(9, "BrBG")[c(1,3)],
                       aesthetics = c("fill", "colour"))+
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 18),
        legend.title = element_blank(),
        legend.text = element_text(size = 18),
        legend.position = c(0.75, 0.9)))


pdf("../img/HistogramDensitiesBackTrInd.pdf", height = 6, width = 9)
p.hist
dev.off()

if(FALSE){
(p.hist2 <- ggplot(dat.all2, aes(x = Y, fill = type, color = type)) +
    theme_minimal() +
    theme_classic() +
    scale_x_continuous(breaks = seq(0, 90, by = 15), limits = c(-5, 95)) +
    geom_histogram(aes(y=..density..), fill = "white", position = "identity", alpha = 0.5, binwidth = 5, lwd = 1.4) +
    geom_density(alpha = 0.6, lwd = 1.2) +
    xlab(expression(paste(NO[2], " concentration level in ", mu, "g/", m^3, sep = ""))) +
    ylab("empirical density") +
    scale_color_brewer(palette = "Dark2", aesthetics = c("fill", "colour")) +
    theme(axis.text = element_text(size = 18),
          axis.title = element_text(size = 18),
          legend.title = element_blank(),
          legend.text = element_text(size = 18),
          legend.position = c(0.75, 0.9)))


pdf("../img/HistogramDensitiesBackTrInd2.pdf", height = 6, width = 9)
p.hist2
dev.off()
}



#names(dat.B)
names(dat)
#corr.DE <- round(cor(dat.B[,c(2:4, 6:9, 13:20)]), digits = 2)
corr.DE <- round(cor(dat[,c(2:4, 6:9, 13:20)]), digits = 2)


# for correlation matrix in ggplot2 see
# http://www.sthda.com/english/wiki/ggplot2-quick-correlation-matrix-heatmap-r-software-and-data-visualization
# accessed 2020-01-30

# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}



upper_tri <- get_upper_tri(corr.DE)
corr.melt <- reshape2::melt(upper_tri, na.rm = TRUE)


p.corr <- ggplot(data = corr.melt, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  xlab("") +
  ylab("") +
#  scale_fill_gradient2(low = brewer.pal(11, "BrBG")[1], high = brewer.pal(11, "BrBG")[11], mid = brewer.pal(11, "BrBG")[6],
#                       low = "darkblue", high = "orange", mid = "white",
#                       midpoint = 0, limit = c(-1,1), space = "Lab",
#                       name="") +
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


pdf("../img/CorrMatrix_Back.pdf", width = 8, height = 8)
p.corr
dev.off()


if(FALSE){
###
### ScatterplotMatrix ----
### Investigate if there are nonlinear relationships between the predictors
###

Y <- dat$Y

# Reconstruct scatterplotMatrix with smoothing curve
panel.density <- function(x){
  dens.x <- adaptiveKernel(x, adjust = 1, na.rm = TRUE,
                           bw = bw.nrd0, kernel = dnorm)
  lines(dens.x$x, min(x, na.rm=TRUE) + dens.x$y * diff(range(x, na.rm=TRUE))/diff(range(dens.x$y, na.rm=TRUE)), col = "black")
  rug(x)
}

set.seed(42)
X1 <- rnorm(1000)
set.seed(42)
X2 <- rnorm(1000)


y <- 2*X1 + rnorm(1000)
a1 <- resid(lm(y ~ X1))
a2 <- fitted(lm(y ~ X1)) # a1 and a2 are orthogonal


pairs(cbind(a1, a2),
      diag.panel = panel.density,
      panel = function(x, y, ...){
        points(x, y, pch = 20, col = "gray60", cex = .5)
        gamLine(x, y, col = "royalblue",
                smoother.args = list(lwd.smooth = 2))
        dataEllipse(x, y, plot.points = FALSE, robust = TRUE,
                    levels = c(.5, .9), col = "coral")
      }
)


pdf("../img/ScatterplotMatrixGamLine2.pdf", width = 9, height = 6)
pairs(cbind(Y, dat[,c(2, 3, 14, 15)]),
      diag.panel = panel.density,
      panel = function(x, y, ...){
        points(x, y, pch = 20, col = "gray60", cex = .5)
        gamLine(x, y, col = "royalblue",
                smoother.args = list(lwd.smooth = 2))
        dataEllipse(x, y, plot.points = FALSE, robust = TRUE,
                    levels = c(.5, .9), col = "coral")
      }
)
dev.off()
}




###
### Locations of monitoring sites (background for Germany, traffic/industrial for NRW) with background popDens ----
###

WGS84 <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
GK3 <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"

# BKG (Bundesamt für Kartographie und Geodäsie)
# German administrative regions
admin.regions.2015 <- readOGR(dsn = "DataFull/Data_BKG/vg250-ew_ebenen",		# M: otherwise, a conflict with GIT results
                              #dsn = "../DATA/Data_BKG/vg250-ew_ebenen",
                              layer = "VG250_GEM",
                              encoding = "UTF-8",
                              use_iconv = TRUE)



load("DataFull/Data_built/grid.DE.RData")							# M: otherwise, a conflict with GIT results
#load("Data_built/grid.DE.RData")



load("data/monSitesDE.rda")
dat	<- monSitesDE[, c(2,4:7,9:24)]


dat.B  <- dat[dat$AQeType == "background", ]
dat.TI <- dat[dat$AQeType != "background", ]


spdf.sites.back <- SpatialPointsDataFrame(coords = cbind(dat.B$Lon, dat.B$Lat),
                                          data = dat.B,
                                          proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))


spdf.sites.tr.ind <- SpatialPointsDataFrame(coords = cbind(dat.TI$Lon, dat.TI$Lat),
                                            data = dat.TI,
                                            proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))



# Derive SpatialPointsDataFrame from 'df.grid.DE'
spdf.DE <- SpatialPointsDataFrame(coords = cbind(df.grid.DE$lon.WGS84, df.grid.DE$lat.WGS84),
                                  data = df.grid.DE,
                                  proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))




# Read indicator vector for filtering AGS referring to Rhine-Ruhr region
ind.RR <- readRDS("indRhineRuhr.rds")

spdf.RR <- spdf.DE[spdf.DE$AGS %in% ind.RR, ]
admin.regions.RR <- admin.regions.2015[admin.regions.2015$AGS %in% ind.RR, ]
admin.regions.RR$ID <- "RR"


# Filter data for North Rhine-Westphalia
#spdf.NRW <- spdf.DE[substring(spdf.DE$AGS, 1, 2) == "05", ]
# 05: North Rhine-Westphalia
# proj4string(spdf.NRW)
# spdf.NRW2 <- spTransform(spdf.NRW, GK3)
# proj4string(spdf.NRW2)
# dat.tmp.NRW <- spdf.NRW2@data
# dat.tmp.NRW$long <- coordinates(spdf.NRW2)[,1]
# dat.tmp.NRW$lat  <- coordinates(spdf.NRW2)[,2]


col.range <- range(spdf.DE$PopDens)

spdf.RR2 <- spTransform(spdf.RR, GK3)

spdf.sites.tr.ind.2 <- spTransform(spdf.sites.tr.ind, GK3)
spdf.sites.back.2 <- spTransform(spdf.sites.back, GK3)

admin.regions.RR2 <- spTransform(admin.regions.RR, GK3)
ind.tmp  <- which(!is.na(over(spdf.sites.tr.ind.2, admin.regions.RR2)[,1]))
ind.tmp2 <- which(!is.na(over(spdf.sites.back.2, admin.regions.RR2)[,1]))

spdf.sites.tr.ind.RR <- spdf.sites.tr.ind.2[ind.tmp, ]
spdf.sites.back.RR   <- spdf.sites.back.2[ind.tmp2, ]

dat.tmp.RR <- spdf.RR2@data
dat.tmp.RR$long <- coordinates(spdf.RR2)[,1]
dat.tmp.RR$lat  <- coordinates(spdf.RR2)[,2]

# Adjust!
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
  # north(dat.tmp, location = "bottomright", symbol = 3) +
  ggsn::scalebar(dat.tmp, dist = 20, st.size = 3, transform = FALSE,
                 dist_unit = "km", model = "WGS84", st.color = brewer.pal(11, "BrBG")[3],#"darkgrey",
                 box.fill = c(brewer.pal(11, "BrBG")[3],"white"),
                 #darkgrey", "white"),
                 box.color = brewer.pal(11, "BrBG")[3],#"darkgrey",
                 border.size = 0.5))

proj4string(spdf.DE)
spdf.DE2 <- spTransform(spdf.DE, GK3)
proj4string(spdf.DE2)

# sPdf.bndry.NRW2 <- spTransform(sPdf.bndry.NRW, GK3)
# sPdf.bndry.NRW2.f <- fortify(sPdf.bndry.NRW2, region = "ID_1")

sPdf.bndry.RR <- spTransform(admin.regions.RR, GK3)
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
  #  scale_x_continuous(breaks = x.GK3, labels = paste(x.WGS, "°E", sep = "")) +
  #  scale_y_continuous(breaks = y.GK3, labels = paste(y.WGS, "°N", sep = "")) +
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
#                       colours = brewer.pal(9, "Blues")[-c(1,2)],
                       breaks = seq(1000, 4000, 1000),
                       labels = seq(1000, 4000, 1000),
                       limits = col.range,
                       na.value = "white") +
  scale_colour_manual(values = brewer.pal(9, "BrBG")[c(1,3)],# c("darkorange", "darkorchid3"),
                      name = "Type of monitoring site",
                      labels = c("background", "traffic/industrial")) +
  theme(legend.title = element_text(size = 14),
        legend.text  = element_text(size = 14),
        legend.background = element_blank(),
        axis.ticks = element_blank()) +
  guides(fill = guide_colourbar(barwidth = 15))
#guides(fill = guide_colourbar(barheight = 15))



p.DE2 <- p.DE +
  # north(dat.tmp, location = "bottomright", symbol = 3) +
  ggsn::scalebar(dat.tmp, dist = 100, st.size = 3, transform = FALSE,
                 dist_unit = "km", model = "WGS84", st.color = brewer.pal(11, "BrBG")[3],#"darkgrey",
                 box.fill = c(brewer.pal(11, "BrBG")[3],"white"),
                 #darkgrey", "white"),
                 box.color = brewer.pal(11, "BrBG")[3],#"darkgrey",
                 border.size = 0.5)



png("../img/MonitoringSitesPopDens_RR.png", width = 900, height = 600)
#pdf("../img/MonitoringSitesPopDens_RR.pdf", width = 12, height = 8)
#plot_grid(p.DE2, p.RR2, rel_widths = c(0.55, 0.45))
plot_grid(p.DE2, p.RR2,
          rel_widths = c(0.5, 0.5))
dev.off()




png("../img/MonitoringSitesPopDens_RR_3.png", width = 900, height = 675)
#pdf("../img/MonitoringSitesPopDens_RR_3.pdf", width = 13, height = 8)
ggdraw() +
  draw_plot(p.DE2, 0, 0, 0.5, 1) +
  draw_plot(p.RR2, 0.5, 0.05, 0.5, 0.9)
dev.off()







###
### Identification of three appropriate locations ----
### for the discussing marginal effects and the assessment of individual exposure to air pollution
###


load("DataFull/Data_built/grid.DE.RData")
WGS84 <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
GK3 <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"

names(df.grid.DE)[c(4,5)] <- c("Lon", "Lat")


dat.Positions <- data.frame(matrix(NA, nrow = 3, ncol = ncol(df.grid.DE)+1))
names(dat.Positions) <- c("Name", names(df.grid.DE))

# 1) Cologne city centre, close to main station (urban)
# Look on GoogleMaps for coordinates close to Cologne main station and filter respective cells from 'df.grid.DE'
# View(df.grid.DE[6.95 < df.grid.DE$Lon & df.grid.DE$Lon < 6.96 & 50.93 < df.grid.DE$Lat & df.grid.DE$Lat < 50.94, ])
df.tmp <- df.grid.DE[df.grid.DE$layer == "295827", ]
spdf.tmp <- SpatialPoints(coords = cbind(df.tmp$lon.GK3, df.tmp$lat.GK3),
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

Sys.setenv(LANG = "en")
GetMap.bbox(lonR = b2$lonR, latR = b2$latR, destfile = "../img/MapCologneCityCentre.png", type = "google-s")	# M: otherwise, a conflict with GIT results
GetMap.bbox(lonR = b2$lonR, latR = b2$latR, destfile = "../img/MapCologneCityCentre2.png", type = "google")		# M: otherwise, a conflict with GIT results
#GetMap.bbox(lonR = b2$lonR, latR = b2$latR, destfile = "../img/MapCologneCityCentre.png", type = "google-s")
#GetMap.bbox(lonR = b2$lonR, latR = b2$latR, destfile = "../img/MapCologneCityCentre2.png", type = "google")

dat.Positions[1,1] <- "Cologne City Centre"
dat.Positions[1,-1] <-  df.grid.DE[df.grid.DE$layer == "295827", ]



# 2) South-east of Mühlheim an der Ruhr (rural)
#View(df.grid.DE[6.90 < df.grid.DE$Lon & df.grid.DE$Lon < 6.91 & 51.40 < df.grid.DE$Lat & df.grid.DE$Lat < 51.41, ])
df.tmp <- df.grid.DE[df.grid.DE$layer == "262025", ]
spdf.tmp <- SpatialPoints(coords = cbind(df.tmp$lon.GK3, df.tmp$lat.GK3),
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

GetMap.bbox(lonR = b2$lonR, latR = b2$latR, destfile = "../img/MapMuehlheim.png", type = "google-s")
GetMap.bbox(lonR = b2$lonR, latR = b2$latR, destfile = "../img/MapMuehlheim2.png", type = "google")
#GetMap.bbox(lonR = b2$lonR, latR = b2$latR, destfile = "img/MapMuehlheim.png", type = "google-s")
#GetMap.bbox(lonR = b2$lonR, latR = b2$latR, destfile = "img/MapMuehlheim2.png", type = "google")

dat.Positions[2,1] <- "MuehlheimRuhr"
dat.Positions[2,-1] <-  df.grid.DE[df.grid.DE$layer == "262025", ]



# 3) Suburb of Dortmund
# View(df.grid.DE[7.47 < df.grid.DE$Lon & df.grid.DE$Lon < 7.48 & 51.49 < df.grid.DE$Lat & df.grid.DE$Lat < 51.50, ])
df.tmp <- df.grid.DE[df.grid.DE$layer == "256215", ]
spdf.tmp <- SpatialPoints(coords = cbind(df.tmp$lon.GK3, df.tmp$lat.GK3),
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

GetMap.bbox(lonR = b2$lonR, latR = b2$latR, destfile = "../img/MapDortmund.png", type = "google-s")
GetMap.bbox(lonR = b2$lonR, latR = b2$latR, destfile = "../img/MapDortmund2.png", type = "google")
#GetMap.bbox(lonR = b2$lonR, latR = b2$latR, destfile = "img/MapDortmund.png", type = "google-s")
#GetMap.bbox(lonR = b2$lonR, latR = b2$latR, destfile = "img/MapDortmund2.png", type = "google")

dat.Positions[3,1] <- "Dortmund"
dat.Positions[3,-1] <-  df.grid.DE[df.grid.DE$layer == "256215", ]

#saveRDS(object = dat.Positions, file = "DataFull/Data_built/dat.Positions.rds")









###
### Partial residual plot based on parametric model ----
###


load("data/monSitesDE.rda")
dat	<- monSitesDE[, c(1,2,4:7,9:24)]



dat.B  <- dat[dat$AQeType == "background", ]
dat.TI <- dat[dat$AQeType != "background", ]


parA <- parLUR(data = dat
                  ,x = c("Lon", "Lat", "Alt", "HighDens", "LowDens", "Ind", "Transp"
#				,"Seap", "Airp", "Constr"
				,"UrbGreen", "Agri", "Forest", "PopDens"
				,"PriRoad", "SecRoad", "FedAuto", "LocRoute")
                  ,y = "Y"
                  ,dirEff = c(0,0,-1,1,1,1,1,-1,0,-1,1,1,1,1,1)
                  ,thresh = 0.95, thresh_pval = 0.1)

parB <- parLUR(data = dat.B
                  ,x = c("Lon", "Lat", "Alt", "HighDens", "LowDens", "Ind", "Transp"
#				,"Seap", "Airp", "Constr"
				,"UrbGreen", "Agri", "Forest", "PopDens"
				,"PriRoad", "SecRoad", "FedAuto", "LocRoute")
                  ,y = "Y"
                  ,dirEff = c(0,0,-1,1,1,1,1,-1,0,-1,1,1,1,1,1)
                  ,thresh = 0.95, thresh_pval = 0.1)


parTI <- parLUR(data = dat.TI
                   ,x = c("Lon", "Lat", "Alt", "HighDens", "LowDens", "Ind", "Transp"
#					,"Seap", "Airp", "Constr"
					,"UrbGreen", "Agri", "Forest", "PopDens"
					,"PriRoad", "SecRoad", "FedAuto", "LocRoute")
                   ,y = "Y"
                   ,dirEff = c(0,0,-1,1,1,1,1,-1,0,-1,1,1,1,1,1)
                   ,thresh = 0.95, thresh_pval = 0.1)



if(FALSE){
# Difference between loessLine smoother and gamLine smoother
par(mfrow = c(1,2))
crp(parB, term = "PopDens", pch = ".", smooth = list(smoother = loessLine),
    grid = FALSE, ylab = "res.partial.popDens") # default smoother is loessLine
crp(parB, term = "PopDens", pch = ".", smooth = list(smoother = gamLine),
    grid = FALSE, ylab = "res.partial.popDens")
par(mfrow = c(1,1))



pdf("../img/ResidualPlotLurParGamLine.pdf", width = 15, height = 8)

par(mfrow = c(2, 3), mai = c(1.2, 1.2, 0.3, 0.3), cex = 1.3)
crp(par1, term = "PopDens", pch = ".", smooth = list(smoother = gamLine),
    col.lines = c("royalblue", "darkorange"), grid = FALSE, ylab = "res.partial.popDens")
crp(par1, term = "Forest", pch = ".", smooth = list(smoother = gamLine),
    col.lines = c("royalblue", "darkorange"), grid = FALSE, ylab = "res.partial.Forest")
crp(par1, term = "FedAuto", pch = ".", smooth = list(smoother = gamLine),
    col.lines = c("royalblue", "darkorange"), grid = FALSE, ylab = "res.partial.NatMot")
crp(par1, term = "Agri", pch = ".", smooth = list(smoother = gamLine),
    col.lines = c("royalblue", "darkorange"), grid = FALSE, ylab = "res.partial.Agri")
crp(par1, term = "Lat", pch = ".", smooth = list(smoother = gamLine),
    col.lines = c("royalblue", "darkorange"), grid = FALSE, ylab = "res.partial.Lat")
crp(par1, term = "Alt", pch = ".", smooth = list(smoother = gamLine),
    col.lines = c("royalblue", "darkorange"), grid = FALSE, ylab = "res.partial.Alt")

dev.off()
}




# With ggplot

###parB
pred.parB <- predict(object = parB, newdata = dat.B, type = "terms")
pred.res.parB <- pred.parB + residuals(parB)

pred.res.parB.l <- reshape2::melt(pred.res.parB)
names(dat.B)
colnames(pred.res.parB)

dat.B.l <- reshape2::melt(subset(dat.B, select = names(coef(parB))[-1]))
names(dat.B.l)[2] <- "observed"

dat.scatter <- cbind(dat.B.l, pred.res.parB.l[,3])
names(dat.scatter)[3] <- "partial.residual"

str(dat.scatter)
dat.scatter$variable <- factor(dat.scatter$variable, levels(dat.scatter$variable)[c(1,2,6,5,3,4)])


pdf("../img/PartialResidual_ggplot.pdf", width = 12, height = 7)
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
dev.off()



### parTI
pred.parTI <- predict(object = parTI, newdata = dat.TI, type = "terms")
pred.res.parTI <- pred.parTI + residuals(parTI)

pred.res.parTI.l <- reshape2::melt(pred.res.parTI)
names(dat.TI)
colnames(pred.res.parTI)

dat.TI.l <- reshape2::melt(subset(dat.TI, select = names(coef(parTI))[-1]))
names(dat.TI.l)[2] <- "observed"

dat.scatter <- cbind(dat.TI.l, pred.res.parTI.l[,3])
names(dat.scatter)[3] <- "partial.residual"

str(dat.scatter)
dat.scatter$variable <- factor(dat.scatter$variable)

pdf("../img/PartialResidual_ggplot_TI.pdf", width = 12, height = 7)
ggplot(data = dat.scatter, aes(x = observed, y = partial.residual, group = variable)) +
  theme_bw() +
  xlab("") +
  ylab("") +
  geom_point(col = "grey")+# brewer.pal(11, "BrBG")[7]) +
  geom_smooth(method = "lm", lty = "dashed", se = FALSE, #col = "royalblue"
              col = brewer.pal(11, "BrBG")[3], lwd = 1.2) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "tp"), se = FALSE,# col = "darkorange"
              col = brewer.pal(11, "BrBG")[10], lwd = 1.2) +
  facet_wrap(~variable, nrow = 2, scale = "free_x") +
  theme(axis.text = element_text(size = 14),
        strip.text = element_text(size = 14),
        plot.margin = unit(c(0,0.5,0,0), "cm"))
dev.off()



###parA
pred.parA <- predict(object = parA, newdata = dat, type = "terms")
pred.res.parA <- pred.parA + residuals(parA)

pred.res.parA.l <- reshape2::melt(pred.res.parA)
names(dat)
colnames(pred.res.parA)

dat.A.l <- reshape2::melt(subset(dat, select = names(coef(parA))[-1]))
names(dat.A.l)[2] <- "observed"

dat.scatter <- cbind(dat.A.l, pred.res.parA.l[,3])
names(dat.scatter)[3] <- "partial.residual"

str(dat.scatter)
dat.scatter$variable <- factor(dat.scatter$variable)

pdf("../img/PartialResidual_ggplot_A.pdf", width = 12, height = 7)
ggplot(data = dat.scatter, aes(x = observed, y = partial.residual, group = variable)) +
  theme_bw() +
  xlab("") +
  ylab("") +
  geom_point(col = "grey")+# brewer.pal(11, "BrBG")[7]) +
  geom_smooth(method = "lm", lty = "dashed", se = FALSE, # col = "royalblue"
              col = brewer.pal(11, "BrBG")[3], lwd = 1.2) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "tp"), se = FALSE,# col = "darkorange"
              col = brewer.pal(11, "BrBG")[10], lwd = 1.2) +
  facet_wrap(~variable, nrow = 2, scale = "free_x") +
  theme(axis.text = element_text(size = 14),
        strip.text = element_text(size = 14),
        plot.margin = unit(c(0,0.5,0,0), "cm"))
dev.off()







###
### Fitted splines based on smooth model ----
###


load("data/monSitesDE.rda")
dat	<- monSitesDE[, c(1,2,4:7,9:24)]


dat.B  <- dat[dat$AQeType == "background", ]
dat.TI <- dat[dat$AQeType != "background", ]



smoothA <- smoothLUR(data = dat
                     ,x = c("Lon", "Lat", "Alt", "HighDens", "LowDens", "Ind", "Transp"
#					,"Seap", "Airp", "Constr"
					,"UrbGreen", "Agri", "Forest", "PopDens"
					, "PriRoad", "SecRoad", "FedAuto", "LocRoute")
                     ,spVar1 = "Lon"
                     ,spVar2 = "Lat"
                     ,y = "Y"
                     ,thresh = 0.95)


smoothB <- smoothLUR(data = dat.B
                     ,x = c("Lon", "Lat", "Alt", "HighDens", "LowDens", "Ind", "Transp"
#					,"Seap", "Airp", "Constr"
					,"UrbGreen", "Agri", "Forest", "PopDens"
					,"PriRoad", "SecRoad", "FedAuto", "LocRoute")
                     ,spVar1 = "Lon"
                     ,spVar2 = "Lat"
                     ,y = "Y"
                     ,thresh = 0.95)


smoothTI <- smoothLUR(data = dat.TI
                      ,x = c("Lon", "Lat", "Alt", "HighDens", "LowDens", "Ind", "Transp"
#					,"Seap", "Airp", "Constr"
					,"UrbGreen", "Agri", "Forest", "PopDens"
					,"PriRoad", "SecRoad", "FedAuto", "LocRoute")
                      ,spVar1 = "Lon"
                      ,spVar2 = "Lat"
                      ,y = "Y"
                      ,thresh = 0.95)



dat.Positions <- readRDS("dat.Positions.rds")


dat.Positions$smoothB  <- predict(object = smoothB, newdata = dat.Positions)
dat.Positions$smoothTI <- predict(object = smoothTI, newdata = dat.Positions)
dat.Positions$smoothA  <- predict(object = smoothA, newdata = dat.Positions)

#saveRDS(object = dat.Positions, file = "DataFull/Data_built/dat.Positions.rds")


if(FALSE){
  semipar <- gam2.3

  pdf("../img/FittedSplines_semipar.pdf", width = 15, height = 16)
  par(mfrow = c(4, 3), mai = c(1.2, 1.2, 0.3, 0.3), cex = 1.3)
  for(i in c(3:14)){
    plot(semipar, se = TRUE, select = i, shade = TRUE, rug = FALSE,
         col = "royalblue", lwd = 2)
    # points(dat[ ,attributes(gam2.3$terms)$term.labels[i+1]],
    #        predict(gam2.3, type = "terms")[ ,i] + residuals(gam2.3),
    #        pch = 16, cex = 0.3)
  }
  dev.off()


  pdf("../img/MargEffect_semipar_PopDens.pdf", width = 6, height = 4)
  par(mfrow = c(1,1), mai = c(1, 0.8, 0.1, 0.5))
  plot(semipar, se = FALSE, select = 10)
  points(x = c(df.grid.DE[332323,"popDens"], df.grid.DE[355210,"popDens"]),
         y = c(predict(object = semipar, newdata = df.grid.DE[332323, ], type="terms")[,"s(popDens)"],
               predict(object = semipar, newdata = df.grid.DE[355210, ], type="terms")[,"s(popDens)"]),
         pch = 20, col = "orangered",cex = 2)
  dev.off()
}




# Alternatively, with ggplot2:
newdata.tmp <- data.frame(matrix(NA, nrow = 1000, ncol = 18))
dat.pred <- dat[, c(3:5,7:10,14:21)]
colnames(newdata.tmp) <- colnames(dat.pred)
for(j in 1:ncol(dat.pred)){
  newdata.tmp[,j] <- seq(min(dat.pred[,j]), max(dat.pred[,j]), length.out = 1000)
}









pred.tmpA <- predict(object = smoothA, newdata = newdata.tmp, se.fit = TRUE, type = "terms")
pred.tmpB <- predict(object = smoothB, newdata = newdata.tmp, se.fit = TRUE, type = "terms")
pred.tmpTI <- predict(object = smoothTI, newdata = newdata.tmp, se.fit = TRUE, type = "terms")


summary(smoothA)$s.table
summary(smoothB)$s.table
summary(smoothTI)$s.table
rownames(summary(smoothA)$s.table)      # structure of coefficient table identical across models

vec.tmp <- c(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14)



#all monitoring sites
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


pdf("../img/FittedSplines_ggplot_smoothA.pdf", width = 12, height = 12)
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
dev.off()



#background
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
dt.tmpB <- cbind(pred.conf, newdata.tmp2.melt)


pdf("../img/FittedSplines_ggplot_smoothB.pdf", width = 12, height = 12)
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
dev.off()




#traffic/industrial
sp.tmp <- rownames(summary(smoothTI)$s.table)[vec.tmp]

edf.tmp <- round(summary(smoothTI)$s.table[vec.tmp, "edf"], 2)

stripe.tmp <- paste(substr(sp.tmp, start = 1, stop = nchar(sp.tmp)-1),
                    ", ", edf.tmp, ")", sep = "")



pred.tmp2 <- pred.tmpTI[[1]][,sp.tmp]
sd.tmp <- pred.tmpTI[[2]][,sp.tmp]

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


pdf("../img/FittedSplines_ggplot_smoothTI.pdf", width = 12, height = 12)
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
dev.off()







### only required for smoothB


dat.Positions <- readRDS("dat.Positions.rds")
dt.points <- data.frame(x = dat.Positions[1:2, "PopDens"],
                        value = c(predict(object = smoothB,
                                          newdata = dat.Positions[1:2, ], type="terms")[,"s(PopDens)"]))

p.spline.popDens <- ggplot(data = dt.tmpB[dt.tmp$pred == "PopDens", ], aes(x = x, y = value)) +
  theme_bw() +
  ylim(-5,10) +
  xlab("") +
  ylab("") +
  geom_line(col = brewer.pal(11, "BrBG")[10],#"blue",
            lwd = 1) +
  geom_point(data = dt.points, col = brewer.pal(9, "YlOrRd")[5],# "orangered",
             size = 2.5) +
  geom_text(data = dt.points, label = c("1", "2"),
            colour = brewer.pal(9, "YlOrRd")[5],# "orangered",
            nudge_x = -60, nudge_y = 0.5, size = 5, fontface = "bold") +
  facet_wrap(~variable) +
  theme(axis.text = element_text(size = 14),
        strip.text = element_text(size = 14))



pdf("../img/MargEffect_smoothB_PopDens.pdf", width = 6, height = 4)
#pdf("img/MargEffect_smoothB_PopDens.pdf", width = 6, height = 4)
p.spline.popDens
dev.off()















###
### Marginal spatial effect in smooth model (bivariate smoothing spline plus parametric term for altitude) ----
###


WGS84 <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
GK3 <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"

# BKG (Bundesamt für Kartographie und Geodäsie) - German administrative regions
admin.regions.2015 <- readOGR(dsn = "DataFull/Data_BKG/vg250-ew_ebenen",
#admin.regions.2015 <- readOGR(dsn = "../DATA/Data_BKG/vg250-ew_ebenen",
                              layer = "VG250_GEM",
                              encoding = "UTF-8",
                              use_iconv = TRUE)


# Read indicator vector for filtering AGS referring to Rhine-Ruhr region
ind.RR <- readRDS("indRhineRuhr.rds")


load("data/monSitesDE.rda")
dat	<- monSitesDE[, c(1,2,4:7,9:24)]


dat.B  <- dat[dat$AQeType == "background", ]
dat.TI <- dat[dat$AQeType != "background", ]






# Filter administrative regions referring to Rhine-Ruhr area
admin.regions.RR <- admin.regions.2015[admin.regions.2015$AGS %in% ind.RR, ]
admin.regions.RR$ID <- "RR"


# Load grid over Germany
load("DataFull/Data_built/grid.DE.RData")
names(df.grid.DE)
df.grid.DE <- df.grid.DE[df.grid.DE$GEN != "Helgoland", ]

names(df.grid.DE)[c(4,5)] <- c("Lon", "Lat")

# Filter grid cells referring to Rhine-Ruhr area
df.grid.RR <- df.grid.DE[df.grid.DE$AGS %in% ind.RR, ]



df.grid.DE$smoothA.sp	<- predict(object = smoothA, newdata = df.grid.DE, type="terms")[,"s(Lon,Lat)"] +
  predict(object = smoothA, newdata = df.grid.DE, type="terms")[,"s(Alt)"]

df.grid.DE$smoothB.sp	<- predict(object = smoothB, newdata = df.grid.DE, type="terms")[,"s(Lon,Lat)"] +
  predict(object = smoothA, newdata = df.grid.DE, type="terms")[,"s(Alt)"]

df.grid.DE$smoothTI.sp	<- predict(object = smoothTI, newdata = df.grid.DE, type="terms")[,"s(Lon,Lat)"] +
  predict(object = smoothA, newdata = df.grid.DE, type="terms")[,"s(Alt)"]


#common color scheme
(brks.sp <- seq(from = min(df.grid.DE$smoothB.sp, df.grid.DE$smoothTI.sp),
             to = max(df.grid.DE$smoothB.sp, df.grid.DE$smoothTI.sp),
             length.out = 11))
brks2.sp <- seq(-12, 8, 4)



dat.Positions <- readRDS("dat.Positions.rds")
#dat.Positions <- readRDS("Data_built/dat.Positions.rds")
df.grid2points	<- dat.Positions[1:2, c("lon.GK3", "lat.GK3")]



#all monitoring sites
p.sp.effA <- ggplot(df.grid.DE, aes(x = lon.GK3, y = lat.GK3)) +
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



p.sp.effA2 <- p.sp.effA +
  geom_point(data = df.grid2points, aes(x = lon.GK3, y = lat.GK3),
             pch = 19, colour =  brewer.pal(9, "YlOrRd")[5], #"orangered",
             size = 2) +
  geom_text(data = df.grid2points, aes(x = lon.GK3, y = lat.GK3, label = c("1", "2")),
            colour =  brewer.pal(9, "YlOrRd")[5],# "orangered",
            nudge_x = -10000, nudge_y = 10000, size = 5, fontface = "bold")



png("../img/SpatialEffect1_A.png", width = 450, height = 675)
#pdf("../img/SpatialEffect1_A.png", width = 6, height = 9)
p.sp.effA
dev.off()


png("../img/SpatialEffect2_A.png", width = 450, height = 675)
#pdf("../img/SpatialEffect2_A.pdf", width = 6, height = 9)
p.sp.effA2
dev.off()









#background
p.sp.effB <- ggplot(df.grid.DE, aes(x = lon.GK3, y = lat.GK3)) +
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



p.sp.effB2 <- p.sp.effB +
  geom_point(data = df.grid2points, aes(x = lon.GK3, y = lat.GK3),
             pch = 19, colour =  brewer.pal(9, "YlOrRd")[5], #"orangered",
             size = 2) +
  geom_text(data = df.grid2points, aes(x = lon.GK3, y = lat.GK3, label = c("1", "2")),
            colour =  brewer.pal(9, "YlOrRd")[5],# "orangered",
            nudge_x = -10000, nudge_y = 10000, size = 5, fontface = "bold")



png("../img/SpatialEffect1_B.png", width = 450, height = 675)
#pdf("../img/SpatialEffect1_B.png", width = 6, height = 9)
p.sp.effB
dev.off()


png("../img/SpatialEffect2_B.png", width = 450, height = 675)
#pdf("../img/SpatialEffect2_B.pdf", width = 6, height = 9)
p.sp.effB2
dev.off()


png("../img/SpatialEffect_PopDens_B.png", width = 900, height = 600)
#pdf("../img/SpatialEffect_PopDens_B.pdf", width = 12, height = 8)
ggdraw() +
  draw_plot(p.sp.effB2, 0, 0, 0.57, 1) +
  draw_plot(p.spline.popDens, 0.57, 0.25, 0.43, 0.5)
dev.off()






#traffic/industrial
p.sp.effTI <- ggplot(df.grid.DE, aes(x = lon.GK3, y = lat.GK3)) +
  ggmap::theme_nothing(legend = TRUE) +  #theme_bw() +
  xlab("") +
  ylab("") +
  coord_fixed(1) +
  geom_tile(aes(fill = smoothTI.sp), width = 1000, height = 1000, na.rm = TRUE) +
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



p.sp.effTI2 <- p.sp.effTI +
  geom_point(data = df.grid2points, aes(x = lon.GK3, y = lat.GK3),
             pch = 19, colour =  brewer.pal(9, "YlOrRd")[5], #"orangered",
             size = 2) +
  geom_text(data = df.grid2points, aes(x = lon.GK3, y = lat.GK3, label = c("1", "2")),
            colour =  brewer.pal(9, "YlOrRd")[5],# "orangered",
            nudge_x = -10000, nudge_y = 10000, size = 5, fontface = "bold")



png("../img/SpatialEffect1_TI.png", width = 450, height = 675)
#pdf("../img/SpatialEffect1_TI.png", width = 6, height = 9)
p.sp.effTI
dev.off()


png("../img/SpatialEffect2_TI.png", width = 450, height = 675)
#pdf("../img/SpatialEffect2_TI.pdf", width = 6, height = 9)
p.sp.effTI2
dev.off()















###
### Conditional background mean NO2 surface over Germany fitted by smooth model ----
###


df.grid.DE$smoothA	<- as.vector(predict(object = smoothA, newdata = df.grid.DE))
df.grid.DE$smoothB	<- as.vector(predict(object = smoothB, newdata = df.grid.DE))
df.grid.DE$smoothTI	<- as.vector(predict(object = smoothTI, newdata = df.grid.DE))



#NO2 concentrations not supported by the underlying data
#all monitoring sites
min(dat$Y)
df.grid.DE$smoothA[df.grid.DE$smoothA < 0]
df.grid.DE$smoothA[df.grid.DE$smoothA < min(dat$Y)]
length(df.grid.DE$smoothA[df.grid.DE$smoothA < min(dat$Y)]) # 951
length(df.grid.DE$smoothA[df.grid.DE$smoothA < min(dat$Y)])/length(df.grid.DE$smoothA)*100		# share in % of predicted values
df.grid.DE$smoothA[df.grid.DE$smoothA < min(dat$Y)]		<- min(dat$Y)				# replace by min contained in data set


max(dat$Y)
df.grid.DE$smoothA[df.grid.DE$smoothA > max(dat$Y)]
length(df.grid.DE$smoothA[df.grid.DE$smoothA > max(dat$Y)]) # 0
length(df.grid.DE$smoothA[df.grid.DE$smoothA > max(dat$Y)])/length(df.grid.DE$smoothA)*100		# share in % of predicted values
df.grid.DE$smoothA[df.grid.DE$smoothA > max(dat$Y)]		<- max(dat$Y)				# replace by max contained in data set
max(df.grid.DE$smoothA)
length(df.grid.DE$smoothA)



#background
min(dat.B$Y)
df.grid.DE$smoothB[df.grid.DE$smoothB < 0]
df.grid.DE$smoothB[df.grid.DE$smoothB < min(dat.B$Y)]
length(df.grid.DE$smoothB[df.grid.DE$smoothB < min(dat.B$Y)]) # 383
length(df.grid.DE$smoothB[df.grid.DE$smoothB < min(dat.B$Y)])/length(df.grid.DE$smoothB)*100		# share in % of predicted values
df.grid.DE$smoothB[df.grid.DE$smoothB < min(dat.B$Y)]		<- min(dat.B$Y)				# replace by min contained in data set


max(dat.B$Y)
df.grid.DE$smoothB[df.grid.DE$smoothB > max(dat.B$Y)]
length(df.grid.DE$smoothB[df.grid.DE$smoothB > max(dat.B$Y)]) # 2
length(df.grid.DE$smoothB[df.grid.DE$smoothB > max(dat.B$Y)])/length(df.grid.DE$smoothB)*100		# share in % of predicted values
df.grid.DE$smoothB[df.grid.DE$smoothB > max(dat.B$Y)]		<- max(dat.B$Y)				# replace by max contained in data set



#traffic/industrial
min(dat.TI$Y)
df.grid.DE$smoothTI[df.grid.DE$smoothTI < 0]
df.grid.DE$smoothTI[df.grid.DE$smoothTI < min(dat.TI$Y)]
length(df.grid.DE$smoothTI[df.grid.DE$smoothTI < min(dat.TI$Y)]) # 2921
length(df.grid.DE$smoothTI[df.grid.DE$smoothTI < min(dat.TI$Y)])/length(df.grid.DE$smoothTI)*100		# share in % of predicted values
df.grid.DE$smoothTI[df.grid.DE$smoothTI < min(dat.TI$Y)]		<- min(dat.TI$Y)				# replace by min contained in data set


max(dat.TI$Y)
df.grid.DE$smoothTI[df.grid.DE$smoothTI > max(dat.TI$Y)]
length(df.grid.DE$smoothTI[df.grid.DE$smoothTI > max(dat.TI$Y)]) # 0
length(df.grid.DE$smoothTI[df.grid.DE$smoothTI > max(dat.TI$Y)])/length(df.grid.DE$smoothTI)*100		# share in % of predicted values
df.grid.DE$smoothTI[df.grid.DE$smoothTI > max(dat.TI$Y)]		<- max(dat.TI$Y)				# replace by max contained in data set




#common color scheme
(brks <- seq(from = min(df.grid.DE$smoothB, df.grid.DE$smoothTI),
             to = max(df.grid.DE$smoothB, df.grid.DE$smoothTI),
             length.out = 11))
brks2 <- seq(10, 70, 20)



#boundaries of RR region
admin.bndry.RR <- spTransform(admin.regions.RR, GK3)
admin.bndry.RR.f <- fortify(admin.bndry.RR, region = "ID")
bndry.tmp <- admin.bndry.RR.f





###smoothA
p.predA <- ggplot(df.grid.DE, aes(x = lon.GK3, y = lat.GK3)) +
  ggmap::theme_nothing(legend = TRUE) +  #theme_bw() +
  xlab("") +
  ylab("") +
  coord_fixed(1) +
  #  geom_tile(aes(fill = gam.pred), width = 1000, height = 1000, na.rm = TRUE) +
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


names(bndry.tmp)[1:2] <- c("lon.GK3", "lat.GK3")
p.predA2 <- p.predA +
  geom_polygon(data = bndry.tmp, color = brewer.pal(1, "BrBG")[11], lwd = 0.7, fill = NA)



png("../img/SpEff_PredA.png", width = 900, height = 675)
#pdf("../img/SpEff_PredA.pdf", width = 12, height = 9)
plot_grid(p.sp.effA, p.predA, nrow = 1)
dev.off()

png("../img/SpEff_PredA2.png", width = 900, height = 675)
#pdf("../img/SpEff_PredA2.pdf", width = 12, height = 9)
plot_grid(p.sp.effA, p.predA2, nrow = 1)
dev.off()






###smoothB
p.predB <- ggplot(df.grid.DE, aes(x = lon.GK3, y = lat.GK3)) +
  ggmap::theme_nothing(legend = TRUE) +  #theme_bw() +
  xlab("") +
  ylab("") +
  coord_fixed(1) +
#  geom_tile(aes(fill = gam.pred), width = 1000, height = 1000, na.rm = TRUE) +
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


names(bndry.tmp)[1:2] <- c("lon.GK3", "lat.GK3")
p.predB2 <- p.predB +
  geom_polygon(data = bndry.tmp, color = brewer.pal(11, "BrBG")[1], lwd = 0.7, fill = NA)



png("../img/SpEff_PredBack.png", width = 900, height = 675)
#pdf("../img/SpEff_PredBack.pdf", width = 12, height = 9)
plot_grid(p.sp.effB, p.predB, nrow = 1)
dev.off()

png("../img/SpEff_PredBack2.png", width = 900, height = 675)
#pdf("../img/SpEff_PredBack2.pdf", width = 12, height = 9)
plot_grid(p.sp.effB, p.predB2, nrow = 1)
dev.off()







###smoothTI
p.predTI <- ggplot(df.grid.DE, aes(x = lon.GK3, y = lat.GK3)) +
  ggmap::theme_nothing(legend = TRUE) +  #theme_bw() +
  xlab("") +
  ylab("") +
  coord_fixed(1) +
#  geom_tile(aes(fill = gam.pred), width = 1000, height = 1000, na.rm = TRUE) +
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


names(bndry.tmp)[1:2] <- c("lon.GK3", "lat.GK3")
p.predTI2 <- p.predTI +
  geom_polygon(data = bndry.tmp, color = brewer.pal(1, "BrBG")[11], lwd = 0.7, fill = NA)



png("../img/SpEff_PredTI.png", width = 900, height = 675)
#pdf("../img/SpEff_PredTI.pdf", width = 12, height = 9)
plot_grid(p.sp.effTI, p.predTI, nrow = 1)
dev.off()

png("../img/SpEff_PredTI2.png", width = 900, height = 675)
#pdf("../img/SpEff_PredTI2.pdf", width = 12, height = 9)
plot_grid(p.sp.effTI, p.predTI2, nrow = 1)
dev.off()












###
### Conditional background and traffic/industrial mean NO2 surface over RR ----
###


df.grid.RR$smoothA	<- as.vector(predict(object = smoothA, newdata = df.grid.RR))
df.grid.RR$smoothB	<- as.vector(predict(object = smoothB, newdata = df.grid.RR))
df.grid.RR$smoothTI	<- as.vector(predict(object = smoothTI, newdata = df.grid.RR))


p.predA.RR <- ggplot(df.grid.RR, aes(x = lon.GK3, y = lat.GK3)) +
  ggmap::theme_nothing(legend = TRUE) +  #theme_bw() +
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



p.predB.RR <- ggplot(df.grid.RR, aes(x = lon.GK3, y = lat.GK3)) +
  ggmap::theme_nothing() +  #theme_bw() +
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
#guides(fill = guide_colourbar(barheight = 15))



p.predTI.RR <- ggplot(df.grid.RR, aes(lon.GK3, y = lat.GK3)) +
  ggmap::theme_nothing() +  #theme_bw() +
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
#  guides(fill = guide_colourbar(barheight = 15))


library(ggpubr)
png("../img/PredBackTrIndRR.png", width = 900, height = 600)
#pdf("../img/PredBackTrIndRR.pdf", width = 12, height = 8)
#plot_grid(p.back.pred.sub, p.tr.ind.pred.sub, rel_widths = c(0.51, 0.45))
ggarrange(p.predB.RR, p.predTI.RR, widths = c(1,1), common.legend = TRUE, legend = "bottom")
dev.off()


png("../img/PredBackDERR.png", width = 900, height = 750)
ggarrange(p.predB2, p.predB.RR, widths = c(1,1), common.legend = TRUE, legend = "bottom")
dev.off()

ggarrange(p.predB2, p.predB.RR, widths = c(1,1), common.legend = TRUE, legend = "bottom") %>%
  ggexport(filename = "../img/PredBackDERR.pdf", width = 12, height = 10)
#  ggexport(filename = "img/PredBackDERR.pdf", width = 12, height = 10)


dat.Positions <- readRDS("dat.Positions.rds")
#dat.Positions <- readRDS("Data_built/dat.Positions.rds")


## Plot points 1 to 3 (of `dat.Positions`) on map
(p.predB.RR2 <- p.predB.RR +
    geom_point(data = dat.Positions, aes(x = lon.GK3, y = lat.GK3),
               pch = 19, colour = brewer.pal(11, "BrBG")[10],#"blue",
               size = 2) +
    geom_text(data = dat.Positions, aes(x = lon.GK3, y = lat.GK3, label = c("1", "2", "3")),
              colour = brewer.pal(11, "BrBG")[10],# "blue",
              nudge_x = -2500, nudge_y = 2500, size = 5, fontface = "bold"))

(p.predTI.RR2 <- p.predTI.RR +
    geom_point(data = dat.Positions, aes(x = lon.GK3, y = lat.GK3),
               pch = 19, colour = brewer.pal(11, "BrBG")[10],#"blue",
               size = 2) +
    geom_text(data = dat.Positions, aes(x = lon.GK3, y = lat.GK3, label = c("1", "2", "3")),
              colour = brewer.pal(11, "BrBG")[10],#"blue",
              nudge_x = -2500, nudge_y = 2500, size = 5, fontface = "bold"))


png("../img/PredBackTrIndRRWithPointsSP.png", width = 900, height = 600)
ggarrange(p.predB.RR2, p.predTI.RR2, widths = c(1,1), common.legend = TRUE, legend = "bottom")
#plot_grid(p.back.pred.sub2, p.tr.ind.pred.sub2, rel_widths = c(0.51, 0.45))
dev.off()

ggarrange(p.predB.RR2, p.predTI.RR2, widths = c(1,1), common.legend = TRUE, legend = "bottom") %>%
  ggexport(filename = "../img/PredBackTrIndRRWithPointsSP.pdf", width = 12, height = 6)
#  ggexport(filename = "img/PredBackTrIndRRWithPointsSP.pdf", width = 12, height = 6)

#ggarrange(p.back.pred.sub2, p.tr.ind.pred.sub2, widths = c(1,1), common.legend = TRUE, legend = "bottom") %>%
#  ggexport(filename = "img/PredBackTrIndRRWithPointsSP.png", width = 900, height = 600)






###
### Correlation between predictions of par0, parB, and smoothB ----
###


data(monSitesDE)
dat <- monSitesDE
names(dat)

dat.B  <- dat[dat$AQeType == "background", ]


par0 <- parLUR(data = dat.B
                  ,x = c("HighDens", "LowDens", "Ind", "Transp"
#				,"Seap", "Airp", "Constr"
				,"UrbGreen", "Agri", "Forest", "PopDens"
				,"PriRoad", "SecRoad", "FedAuto", "LocRoute")
                  ,y = "Y"
                  ,dirEff = c(0,0,-1,1,1,1,1,-1,0,-1,1,1,1,1,1)
                  ,thresh = 0.95)


parB <- parLUR(data = dat[dat$AQeType=="background", ]
                  ,x = c("Lon", "Lat", "Alt", "HighDens", "LowDens", "Ind", "Transp"
#				, "Seap", "Airp", "Constr"
				,"UrbGreen", "Agri", "Forest", "PopDens"
				,"PriRoad", "SecRoad", "FedAuto", "LocRoute")
                  ,y = "Y"
                  ,dirEff = c(0,0,-1,1,1,1,1,-1,0,-1,1,1,1,1,1)
                  ,thresh = 0.95)

smoothB <- smoothLUR(data = dat.B
                     ,x = c("Lon", "Lat", "Alt", "HighDens"
                              ,"LowDens", "Ind", "Transp"
#					,"Seap", "Airp", "Constr"
					,"UrbGreen", "Agri", "Forest"
                              , "PopDens", "PriRoad", "SecRoad", "FedAuto", "LocRoute")
                     ,spVar1 = "Lon"
                     ,spVar2 = "Lat"
                     ,y = "Y"
                     ,thresh = 0.95)


parA <- parLUR(data = dat
                  ,x = c("Lon", "Lat", "Alt", "HighDens", "LowDens", "Ind", "Transp"
#				,"Seap", "Airp", "Constr"
				,"UrbGreen", "Agri", "Forest", "PopDens"
				,"PriRoad", "SecRoad", "FedAuto", "LocRoute")
                  ,y = "Y"
                  ,dirEff = c(0,0,-1,1,1,1,1,-1,0,-1,1,1,1,1,1)
                  ,thresh = 0.95)

smoothA <- smoothLUR(data = dat
                     ,x = c("Lon", "Lat", "Alt", "HighDens", "LowDens", "Ind", "Transp"
#					,"Seap", "Airp", "Constr"
					,"UrbGreen", "Agri", "Forest", "PopDens"
					,"PriRoad", "SecRoad", "FedAuto", "LocRoute")
                     ,spVar1 = "Lon"
                     ,spVar2 = "Lat"
                     ,y = "Y"
                     ,thresh = 0.95)


parTI <- parLUR(data = dat[dat$AQeType!="background", ]
                  ,x = c("Lon", "Lat", "Alt", "HighDens", "LowDens", "Ind", "Transp"
#					,"Seap", "Airp", "Constr"
	                        ,"UrbGreen", "Agri", "Forest", "PopDens"
      	                  ,"PriRoad", "SecRoad", "FedAuto", "LocRoute")
                  ,y = "Y"
                  ,dirEff = c(0,0,-1,1,1,1,1,-1,0,-1,1,1,1,1,1)
                  ,thresh = 0.95)

smoothTI <- smoothLUR(data = dat[dat$AQeType!="background", ]
                     ,x = c("Lon", "Lat", "Alt", "HighDens", "LowDens", "Ind", "Transp"
#					,"Seap", "Airp", "Constr"
					,"UrbGreen", "Agri", "Forest", "PopDens"
					,"PriRoad", "SecRoad", "FedAuto", "LocRoute")
                     ,spVar1 = "Lon"
                     ,spVar2 = "Lat"
                     ,y = "Y"
                     ,thresh = 0.95)

load("grid.DE.RData")
names(df.grid.DE)
df.grid.DE <- df.grid.DE[df.grid.DE$GEN != "Helgoland", ]
names(df.grid.DE)[c(4,5,20,24)] <- c("Lon", "Lat", "PopDens", "FedAuto")



df.grid.DE$smoothB	<- as.vector(predict(object = smoothB, newdata = df.grid.DE))
#df.grid.DE$par0		<- as.vector(predict(object = par0, newdata = df.grid.DE))
df.grid.DE$parB		<- as.vector(predict(object = parB, newdata = df.grid.DE))

df.grid.DE$smoothTI	<- as.vector(predict(object = smoothTI, newdata = df.grid.DE))
df.grid.DE$parTI		<- as.vector(predict(object = parTI, newdata = df.grid.DE))

df.grid.DE$smoothA	<- as.vector(predict(object = smoothA, newdata = df.grid.DE))
df.grid.DE$parA		<- as.vector(predict(object = parA, newdata = df.grid.DE))


#cor(df.grid.DE[,c("smoothB", "par0", "parB")])
cor(df.grid.DE[,c("parB", "parTI", "parA", "smoothB", "smoothTI", "smoothA")])
cor(df.grid.DE[,c("smoothB", "parB")])
cor(df.grid.DE[,c("smoothTI", "parTI")])
cor(df.grid.DE[,c("smoothA", "parA")])



xtable(cor(df.grid.DE[,c("parB", "parTI", "parA", "smoothB", "smoothTI", "smoothA")]))

