##################################################################################
### Creating and exporting figures and tables for presentation slides and paper
##################################################################################

# To obtain the same ratio when using pdf instead of png, divide height and width (used to produce png) by 75

rm(list = ls())

## Load packages ----

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

# library(car)          # required for functions crp() and adaptiveKernel()



###
### Some descriptives ----
###

# Data referring to background sites 
DATA <- read.csv("R/DATA/Data_built/DATA.csv", header=TRUE)[, -1]

table(DATA$BBSRArea)
table(DATA$AQeArea)
table(DATA$BBSRArea2)

dat.back <- DATA[,c(2,5:7,11:20,26:30)]
str(dat.back)
summary(dat.back[, c(2:4, 15, 16:19)])

#rename some columns
names(dat.back)[c(1:4, 15)] <- c("Y", "Lon", "Lat", "Alt", "popDens")


# Data referring to traffic/industrial sites 
DATA <- read.csv("R/DATA/Data_built/DATA_DE_traffic_industrial.csv", header=TRUE)[, -1]
dat.tr.ind <- DATA[,c(2,5:7,11:20,26:30)]

#rename some columns
names(dat.tr.ind)[c(1:4, 15)] <- c("Y", "Lon", "Lat", "Alt", "popDens")


# Tukey's Five and Mean
fivenum(dat.back$Y); mean(dat.back$Y); sd(dat.back$Y)
fivenum(dat.tr.ind$Y); mean(dat.tr.ind$Y); sd(dat.tr.ind$Y)

dat.all <- data.frame(Y = c(dat.back$Y, dat.tr.ind$Y), 
                      type = c(rep("Backround", 246), rep("Traffic/Industrial", 157)))

# Histograms with empirical density curve
p.hist <- ggplot(dat.all, aes(x = Y, fill = type, color = type)) +
  theme_minimal() +
  theme_classic() +
  scale_x_continuous(breaks = seq(0, 90, by = 15), limits = c(-5, 95)) +
  geom_histogram(aes(y=..density..), fill = "white", position = "identity", alpha = 0.5, binwidth = 5, lwd = 1.4) +
  geom_density(alpha = 0.6, lwd = 1.2) +
  xlab(expression(paste(NO[2], " concentration level in ", mu, "g/", m^3, sep = ""))) +
  ylab("density") +
  scale_color_brewer(palette = "Dark2", aesthetics = c("fill", "colour")) +
  theme(axis.text = element_text(size = 18), 
        axis.title = element_text(size = 18),
        legend.title = element_blank(),
        legend.text = element_text(size = 18),
        legend.position = c(0.75, 0.9))
  

pdf("img/HistogramDensitiesBackTrInd.pdf", height = 6, width = 9)
p.hist
dev.off()



# Adjust!
dat.tmp <- dat.back

t(apply(dat.tmp, MARGIN = 2, FUN = function(x){
  return(c(nr.values = length(unique(x)),
           max.extreme =  isTRUE(max(x) > quantile(x, 0.9) + 3* (quantile(x, 0.9) - quantile(x, 0.1))),
           min.extreme =  isTRUE(min(x) < quantile(x, 0.1) - 3* (quantile(x, 0.9) - quantile(x, 0.1))))
  )}))

names(dat.back)
corr.DE <- round(cor(dat.back[,-c(1, 9:11)]), 2)


# for correlation matrix in ggplot2 see 
# http://www.sthda.com/english/wiki/ggplot2-quick-correlation-matrix-heatmap-r-software-and-data-visualization
# accessed 2020-01-30

# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}


# Adjust!
corr.tmp <- corr.DE

upper_tri <- get_upper_tri(corr.tmp)
corr.melt <- melt(upper_tri, na.rm = TRUE)


p.corr <- ggplot(data = corr.melt, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  xlab("") +
  ylab("") +
  scale_fill_gradient2(low = "darkblue", high = "orange", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="") +
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


pdf("img/CorrMatrix_Back.pdf", width = 8, height = 8)
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


pdf("img/ScatterplotMatrixGamLine2.pdf", width = 9, height = 6)
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
admin.regions.2015 <- readOGR(dsn = "../DATA/Data_BKG/vg250-ew_ebenen",
                              layer = "VG250_GEM",
                              encoding = "UTF-8",
                              use_iconv = TRUE)



load("R/DATA/Data_built/grid.DE.NEW.RData")


# Data referring to background sites

DATA <- read.csv("R/DATA/Data_built/DATA.csv", header=TRUE)[, -1]
dat.back <- DATA[,c(2,5:7,11:20,26:30)]

#rename some columns
names(dat.back)[c(1:4, 15)] <- c("Y", "Lon", "Lat", "Alt", "popDens")


# Data referring to traffic/industrial sites 
DATA <- read.csv("R/DATA/Data_built/DATA_DE_traffic_industrial.csv", header=TRUE)[, -1]
dat.tr.ind <- DATA[,c(2,5:7,11:20,26:30)]

#rename some columns
names(dat.tr.ind)[c(1:4, 15)] <- c("Y", "Lon", "Lat", "Alt", "popDens")



spdf.sites.back <- SpatialPointsDataFrame(coords = cbind(dat.back$Lon, dat.back$Lat),
                                          data = dat.back,
                                          proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))


spdf.sites.tr.ind <- SpatialPointsDataFrame(coords = cbind(dat.tr.ind$Lon, dat.tr.ind$Lat),
                                            data = dat.tr.ind,
                                            proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))



# Derive SpatialPointsDataFrame from 'df.grid.DE'
spdf.DE <- SpatialPointsDataFrame(coords = cbind(df.grid.DE$Lon.WGS84, df.grid.DE$Lat.WGS84),
                                  data = df.grid.DE,
                                  proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))




# Read indicator vector for filtering AGS referring to Rhine-Ruhr region
ind.RR <- readRDS("R/DATA/Data_built/indRhineRuhr.rds")

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


col.range <- range(spdf.DE$BBSRpopDens)

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
    geom_tile(aes(fill = BBSRpopDens), width = 1000, height = 1000, na.rm = TRUE)+
    geom_point(data = spdf.tmp1@data,
               aes(x = coordinates(spdf.tmp1)[,1],
                   y = coordinates(spdf.tmp1)[,2],
               colour = "darkorange"),
               size = 1, shape = 15) +
    geom_point(data = spdf.tmp2@data,
               aes(x = coordinates(spdf.tmp2)[,1],
                   y = coordinates(spdf.tmp2)[,2],
               colour = "darkorchid3"),
               size = 1, shape = 15) +
    scale_colour_manual(values = c("darkorange", "darkorchid3"),
                        name = "Type of monitoring site",
                        labels = c("background", "traffic/industrial")) +
    scale_fill_gradientn(name = "popDens",
                         colours = brewer.pal(9, "Blues")[-c(1,2)], 
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
  
  
p.RR2 <- p.RR +
  # north(dat.tmp, location = "bottomright", symbol = 3) +
  scalebar(dat.tmp, dist = 20, st.size = 3, transform = FALSE,
           dist_unit = "km", model = "WGS84", st.color = "darkgrey",
           box.fill = c("darkgrey", "white"), box.color = "darkgrey",
           border.size = 0.5)

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
  geom_tile(aes(fill = BBSRpopDens), width = 1000, height = 1000, na.rm = TRUE)+
  geom_point(data = spdf.sites.back.2@data,
             aes(x = coordinates(spdf.sites.back.2)[,1],
                 y = coordinates(spdf.sites.back.2)[,2], 
                 colour = "darkorange"),
             size = 1, shape = 15) +
  geom_point(data = spdf.sites.tr.ind.2@data,
             aes(x = coordinates(spdf.sites.tr.ind.2)[,1],
                 y = coordinates(spdf.sites.tr.ind.2)[,2],
                 colour = "darkorchid3"),
             size = 1, shape = 15) +
  geom_polygon(data = bndry.tmp, color = "orangered", lwd = 0.7, fill = NA) +
  scale_fill_gradientn(name = "popDens",
                       colours = brewer.pal(9, "Blues")[-c(1,2)], 
                       breaks = seq(1000, 4000, 1000),
                       labels = seq(1000, 4000, 1000),
                       limits = col.range,
                       na.value = "white") +
  scale_colour_manual(values = c("darkorange", "darkorchid3"),
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
  scalebar(dat.tmp, dist = 100, st.size = 3, transform = FALSE,
           dist_unit = "km", model = "WGS84", st.color = "darkgrey",
           box.fill = c("darkgrey", "white"), box.color = "darkgrey",
           border.size = 0.5)



png("img/MonitoringSitesPopDens_RR.png", width = 900, height = 600)
pdf("img/MonitoringSitesPopDens_RR.pdf", width = 12, height = 8)
#plot_grid(p.DE2, p.RR2, rel_widths = c(0.55, 0.45))
plot_grid(p.DE2, p.RR2, 
          rel_widths = c(0.5, 0.5))
dev.off()


library(ggpubr)
png("img/MonitoringSitesPopDens_RR_2.png", width = 900, height = 600)
ggarrange(p.DE2, p.RR2, widths = c(2,1), common.legend = TRUE, legend = "top")
dev.off()


png("img/MonitoringSitesPopDens_RR_3.png", width = 900, height = 675)
pdf("img/MonitoringSitesPopDens_RR_3.pdf", width = 13, height = 8)
ggdraw() +
  draw_plot(p.DE2, 0, 0, 0.5, 1) +
  draw_plot(p.RR2, 0.5, 0.05, 0.5, 0.9)
dev.off()


###
### Identification of three appropriate locations ----
### for the discussing marginal effects and the assessment of individual exposure to air pollution
###

load("R/DATA/Data_built/grid.DE.NEW.RData")
WGS84 <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
GK3 <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"

names(df.grid.DE)[c(4,5,22)] <- c("Lon", "Lat", "popDens")


dat.Positions <- data.frame(matrix(NA, nrow = 3, ncol = ncol(df.grid.DE)+1))
names(dat.Positions) <- c("Name", names(df.grid.DE))

# 1) Cologne city centre, close to main station (urban)
# Look on GoogleMaps for coordinates close to Cologne main station and filter respective cells from 'df.grid.DE'
# View(df.grid.DE[6.95 < df.grid.DE$Lon & df.grid.DE$Lon < 6.96 & 50.93 < df.grid.DE$Lat & df.grid.DE$Lat < 50.94, ])
df.tmp <- df.grid.DE[df.grid.DE$Layer == "295827", ]
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

Sys.setenv(LANG = "en")
GetMap.bbox(lonR = b2$lonR, latR = b2$latR, destfile = "img/MapCologneCityCentre.png", type = "google-s")
GetMap.bbox(lonR = b2$lonR, latR = b2$latR, destfile = "img/MapCologneCityCentre2.png", type = "google")

dat.Positions[1,1] <- "Cologne City Centre"
dat.Positions[1,-1] <-  df.grid.DE[df.grid.DE$Layer == "295827", ]



# 2) South-east of Mühlheim an der Ruhr (rural)
#View(df.grid.DE[6.90 < df.grid.DE$Lon & df.grid.DE$Lon < 6.91 & 51.40 < df.grid.DE$Lat & df.grid.DE$Lat < 51.41, ])
df.tmp <- df.grid.DE[df.grid.DE$Layer == "262025", ]
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

GetMap.bbox(lonR = b2$lonR, latR = b2$latR, destfile = "img/MapMuehlheim.png", type = "google-s")
GetMap.bbox(lonR = b2$lonR, latR = b2$latR, destfile = "img/MapMuehlheim2.png", type = "google")

dat.Positions[2,1] <- "MuehlheimRuhr"
dat.Positions[2,-1] <-  df.grid.DE[df.grid.DE$Layer == "262025", ]



# 3) Suburb of Dortmund
# View(df.grid.DE[7.47 < df.grid.DE$Lon & df.grid.DE$Lon < 7.48 & 51.49 < df.grid.DE$Lat & df.grid.DE$Lat < 51.50, ])
df.tmp <- df.grid.DE[df.grid.DE$Layer == "256215", ]
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

GetMap.bbox(lonR = b2$lonR, latR = b2$latR, destfile = "img/MapDortmund.png", type = "google-s")
GetMap.bbox(lonR = b2$lonR, latR = b2$latR, destfile = "img/MapDortmund2.png", type = "google")

dat.Positions[3,1] <- "Dortmund"
dat.Positions[3,-1] <-  df.grid.DE[df.grid.DE$Layer == "256215", ]

#saveRDS(object = dat.Positions, file = "R/DATA/Data_built/dat.Positions.rds")



###
### Partial residual plot based on model par1 ----
###


DATA <- read.csv("R/DATA/Data_built/DATA.csv", header=TRUE)[, -1]
dat <- DATA[,c(2,5:7,11:20,26:30)]
#rename some columns
names(dat)[c(1:4, 15)] <- c("Y", "Lon", "Lat", "Alt", "popDens")

par1 <- lm(Y ~ popDens + Forest + Lat + Alt + Agri + NatMot, data = dat)


if(FALSE){
# Difference between loessLine smoother and gamLine smoother
par(mfrow = c(1,2))
crp(par1, term = "popDens", pch = ".", smooth = list(smoother = loessLine),
    grid = FALSE, ylab = "res.partial.popDens") # default smoother is loessLine
crp(par1, term = "popDens", pch = ".", smooth = list(smoother = gamLine),
    grid = FALSE, ylab = "res.partial.popDens")
par(mfrow = c(1,1))



pdf("img/ResidualPlotLurParGamLine.pdf", width = 15, height = 8)

par(mfrow = c(2, 3), mai = c(1.2, 1.2, 0.3, 0.3), cex = 1.3)
crp(par1, term = "popDens", pch = ".", smooth = list(smoother = gamLine),
    col.lines = c("royalblue", "darkorange"), grid = FALSE, ylab = "res.partial.popDens")
crp(par1, term = "Forest", pch = ".", smooth = list(smoother = gamLine),
    col.lines = c("royalblue", "darkorange"), grid = FALSE, ylab = "res.partial.Forest")
crp(par1, term = "NatMot", pch = ".", smooth = list(smoother = gamLine),
    col.lines = c("royalblue", "darkorange"), grid = FALSE, ylab = "res.partial.NatMot")
crp(par1, term = "Agri", pch = ".", smooth = list(smoother = gamLine),
    col.lines = c("royalblue", "darkorange"), grid = FALSE, ylab = "res.partial.Agri")
crp(par1, term = "Lat", pch = ".", smooth = list(smoother = gamLine),
    col.lines = c("royalblue", "darkorange"), grid = FALSE, ylab = "res.partial.Lat")
crp(par1, term = "Alt", pch = ".", smooth = list(smoother = gamLine),
    col.lines = c("royalblue", "darkorange"), grid = FALSE, ylab = "res.partial.Alt")

dev.off()
}


# Alternatively, with ggplot
dat.back <- dat 
pred.par1 <- predict(object = par1, newdata = dat.back, type = "terms")
pred.res.par1 <- pred.par1 + residuals(par1)

pred.res.par1.l <- melt(pred.res.par1)
names(dat.back)
colnames(pred.res.par1)

dat.back.l <- melt(dat.back[, c(15, 14, 3, 4, 13, 18)])
names(dat.back.l)[2] <- "observed"

dat.scatter <- cbind(dat.back.l, pred.res.par1.l[,3])
names(dat.scatter)[3] <- "partial.residual"

str(dat.scatter)
dat.scatter$variable <- factor(dat.scatter$variable, levels(dat.scatter$variable)[c(1,2,6,5,3,4)])


pdf("img/PartialResidual_ggplot.pdf", width = 12, height = 7)
ggplot(data = dat.scatter, aes(x = observed, y = partial.residual, group = variable)) +
  theme_bw() +
  xlab("") + 
  ylab("") +
  geom_point() +
  geom_smooth(method = "lm", lty = "dashed", se = FALSE, col = "royalblue", lwd = 1.2) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "tp"), se = FALSE, col = "darkorange", lwd = 1.2) +
  facet_wrap(~variable, nrow = 2, scale = "free_x") +
  theme(axis.text = element_text(size = 14),
        strip.text = element_text(size = 14),
        plot.margin = unit(c(0,0.5,0,0), "cm"))
dev.off()




###
### Fitted splines based on semipar ----
###

DATA <- read.csv("R/DATA/Data_built/DATA.csv", header=TRUE)[, -1]
dat <- DATA[,c(2,5:7,11:20,26:30)]
#rename some columns
names(dat)[c(1:4, 15)] <- c("Y", "Lon", "Lat", "Alt", "popDens")


form.gam2.3 <- Y ~ s(Lon, Lat, k=-1, bs="tp", fx=FALSE, xt=NULL, id=NULL, sp=NULL) +
  s(Alt, k=-1, bs="tp") +
  s(HighDens, k=-1, bs="tp") +
  s(LowDens, k=-1, bs="tp") +
  s(Ind, k=-1, bs="tp") +
  s(Transp, k=-1, bs="tp") +
  s(UrbGreen, k=-1, bs="tp") +
  s(Agri, k=-1, bs="tp") +
  s(Forest, k=-1, bs="tp") +
  s(popDens, k=-1, bs="tp") +
  s(PriRoad, k=-1, bs="tp") +
  s(SecRoad, k=-1, bs="tp") +
  s(NatMot, k=-1, bs="tp") +
  s(LocRoute, k=-1, bs="tp")

gam2.3 <- gam(formula=form.gam2.3, fit=TRUE, method="P-ML", data=dat, family=gaussian(),
              weights=NULL, subset=NULL, offset=NULL, optimizer=c("outer", "newton"), scale=0,
              select=TRUE, knots=NULL, sp=NULL, min.sp=NULL, H=NULL, gamma=1, paraPen=NULL, G=NULL)

dat.Positions$semipar <- predict(object = gam2.3, newdata = dat.Positions)
#saveRDS(object = dat.Positions, file = "R/DATA/Data_built/dat.Positions.rds")


if(FALSE){
  semipar <- gam2.3
  
  pdf("img/FittedSplines_semipar.pdf", width = 15, height = 16)
  par(mfrow = c(4, 3), mai = c(1.2, 1.2, 0.3, 0.3), cex = 1.3)
  for(i in c(3:14)){
    plot(semipar, se = TRUE, select = i, shade = TRUE, rug = FALSE,
         col = "royalblue", lwd = 2)
    # points(dat[ ,attributes(gam2.3$terms)$term.labels[i+1]],
    #        predict(gam2.3, type = "terms")[ ,i] + residuals(gam2.3),
    #        pch = 16, cex = 0.3)
  }
  dev.off()
  
  
  pdf("img/MargEffect_semipar_PopDens.pdf", width = 6, height = 4)
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
colnames(newdata.tmp) <- colnames(dat.back)[-1]
for(j in 1:18){
  newdata.tmp[,j] <- seq(min(dat.back[,j+1]), max(dat.back[,j+1]), length.out = 1000)
}  

semipar <- gam2.3

pred.tmp <- predict(object = semipar, newdata = newdata.tmp, se.fit = TRUE, type = "terms")


summary(semipar)$s.table
rownames(summary(semipar)$s.table)

vec.tmp <- c(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14)
sp.tmp <- rownames(summary(semipar)$s.table)[vec.tmp]

edf.tmp <- round(summary(semipar)$s.table[vec.tmp, "edf"], 2)

stripe.tmp <- paste(substr(sp.tmp, start = 1, stop = nchar(sp.tmp)-1), 
                    ", ", edf.tmp, ")", sep = "")



pred.tmp2 <- pred.tmp[[1]][,sp.tmp]
sd.tmp <- pred.tmp[[2]][,sp.tmp]

conf.lower <- pred.tmp2 - 2*sd.tmp
conf.upper <- pred.tmp2 + 2*sd.tmp

newdata.tmp2 <- newdata.tmp[,substr(sp.tmp, start = 3, stop = nchar(sp.tmp)-1)]


# transform in long format
pred.tmp2.melt <- melt(pred.tmp2)
conf.lower.melt <- melt(conf.lower)
conf.upper.melt <- melt(conf.upper)

pred.conf <- cbind(pred.tmp2.melt[,-1], conf.lower.melt[,3], conf.upper.melt[,3])
names(pred.conf) <- c("variable", "value", "lwr", "upr")

levels(pred.conf$variable)
levels(pred.conf$variable) <- stripe.tmp

newdata.tmp2.melt <- melt(newdata.tmp2)
colnames(newdata.tmp2.melt) <- c("pred", "x")

dt.tmp <- cbind(pred.conf, newdata.tmp2.melt)

pdf("img/FittedSplines_semipar_ggplot.pdf", width = 12, height = 12)
ggplot(data = dt.tmp, aes(x = x, y = value)) +
  theme_bw() + 
  xlab("") +
  ylab("") +
  geom_line(col = "royalblue", lwd = 0.7) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.25) +
  facet_wrap(~variable, nrow = 4, scales = "free_x") +
  theme(axis.text = element_text(size = 14),
        strip.text = element_text(size = 14))
dev.off()


dat.Positions <- readRDS("R/DATA/Data_built/dat.Positions.rds")
dt.points <- data.frame(x = dat.Positions[1:2, "popDens"],
                        value = c(predict(object = semipar,
                                          newdata = dat.Positions[1:2, ], type="terms")[,"s(popDens)"]))

p.spline.popDens <- ggplot(data = dt.tmp[dt.tmp$pred == "popDens", ], aes(x = x, y = value)) +
  theme_bw() + 
  ylim(-5,10) +
  xlab("") +
  ylab("") +
  geom_line(col = "blue", lwd = 1) +
  geom_point(data = dt.points, col = "orangered", size = 2.5) +
  geom_text(data = dt.points, label = c("1", "2"),
            colour = "orangered", nudge_x = -60, nudge_y = 0.5, size = 5, fontface = "bold") +
  facet_wrap(~variable) +
  theme(axis.text = element_text(size = 14),
        strip.text = element_text(size = 14))



pdf("img/MargEffect_semipar_PopDens.pdf", width = 6, height = 4)
p.spline.popDens
dev.off()



###
### Marginal spatial effect in semipar (bivariate smoothing spline plus parametric term for altitude) ----
###

# sPdf.bndry.NRW <- readOGR(dsn = "../DATA/Data_GADM",
#                           layer = "DEU_adm1", encoding = "UTF-8", use_iconv = TRUE)
# sPdf.bndry.NRW <- sPdf.bndry.NRW[sPdf.bndry.NRW$ID_1==10,]
# 

WGS84 <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
GK3 <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"

# BKG (Bundesamt für Kartographie und Geodäsie) - German administrative regions
admin.regions.2015 <- readOGR(dsn = "../DATA/Data_BKG/vg250-ew_ebenen",
                              layer = "VG250_GEM",
                              encoding = "UTF-8",
                              use_iconv = TRUE)


# Read indicator vector for filtering AGS referring to Rhine-Ruhr region
ind.RR <- readRDS("R/DATA/Data_built/indRhineRuhr.rds")


DATA <- read.csv("R/DATA/Data_built/DATA.csv", header=TRUE)[, -1]
dat.back <- DATA[,c(2,5:7,11:20,26:30)]
names(dat.back)[c(1:4, 15)] <- c("Y", "Lon", "Lat", "Alt", "popDens")

# GAM with bivariate spline for lon and lat and univariate spline for remaining predictors 
form.gam2.3 <- Y ~ s(Lon, Lat, k=-1, bs="tp", fx=FALSE, xt=NULL, id=NULL, sp=NULL) +
  s(Alt, k=-1, bs="tp") +
  s(HighDens, k=-1, bs="tp") +
  s(LowDens, k=-1, bs="tp") +
  s(Ind, k=-1, bs="tp") +
  s(Transp, k=-1, bs="tp") +
  s(UrbGreen, k=-1, bs="tp") +
  s(Agri, k=-1, bs="tp") +
  s(Forest, k=-1, bs="tp") +
  s(popDens, k=-1, bs="tp") +
  s(PriRoad, k=-1, bs="tp") +
  s(SecRoad, k=-1, bs="tp") +
  s(NatMot, k=-1, bs="tp") +
  s(LocRoute, k=-1, bs="tp")

semipar <- gam(formula=form.gam2.3, fit=TRUE, method="P-ML", data=dat.back, family=gaussian(),
               weights=NULL, subset=NULL, offset=NULL, optimizer=c("outer", "newton"), scale=0,
               select=TRUE, knots=NULL, sp=NULL, min.sp=NULL, H=NULL, gamma=1, paraPen=NULL, G=NULL)



# Filter administrative regions referring to Rhine-Ruhr area
admin.regions.RR <- admin.regions.2015[admin.regions.2015$AGS %in% ind.RR, ]
admin.regions.RR$ID <- "RR"


# Load grid over Germany
load("R/DATA/Data_built/grid.DE.NEW.RData")
names(df.grid.DE)[22] <- "popDens"
df.grid.DE <- df.grid.DE[df.grid.DE$GEN != "Helgoland", ]

names(df.grid.DE)[c(4,5)] <- c("Lon", "Lat")

# Filter grid cells referring to Rhine-Ruhr area
df.grid.RR <- df.grid.DE[df.grid.DE$AGS %in% ind.RR, ]

sp.eff	<- predict(object = semipar, newdata = df.grid.DE, type="terms")[,"s(Lon,Lat)"] +
  predict(object = semipar, newdata = df.grid.DE, type="terms")[,"s(Alt)"]

df.grid.DE$sp.eff	<- as.vector(sp.eff)


p.sp.eff <- ggplot(df.grid.DE, aes(x = Lon.GK3, y = Lat.GK3)) +
  ggmap::theme_nothing(legend = TRUE) +  #theme_bw() +
  xlab("") +
  ylab("") +
  coord_fixed(1) +
  geom_tile(aes(fill = sp.eff), width = 1000, height = 1000, na.rm = TRUE) +
  scale_fill_gradientn(name = "",
                       colours = brewer.pal(11, "BrBG")[-6],
                       breaks = c(-13, -11, -9, -7, -5, -3, -1, 1, 3, 5),
                       labels = c("-13", "-11", " -9", " -7", " -5", " -3", " -1",
                                  "   1", "   3", "   5"),
                       limits = range(df.grid.DE$sp.eff),
                       na.value = "white") +
  theme(legend.text  = element_text(size = 17),
        legend.background = element_blank(),
        legend.position = "bottom",
        legend.box = "vertical",
        axis.ticks = element_blank()) +
  guides(fill = guide_colourbar(barwidth = 20))
  #theme(legend.title = element_text(size = 14),
  #      legend.text  = element_text(size = 14),
  #      legend.background = element_blank()) +
  #guides(fill = guide_colourbar(barheight = 15))



dat.Positions <- readRDS("R/DATA/Data_built/dat.Positions.rds")
df.grid2points	<- dat.Positions[1:2, c("Lon.GK3", "Lat.GK3")]

p.sp.eff2 <- p.sp.eff +
  geom_point(data = df.grid2points, aes(x = Lon.GK3, y = Lat.GK3), 
             pch = 19, colour = "orangered", size = 2) +
  geom_text(data = df.grid2points, aes(x = Lon.GK3, y = Lat.GK3, label = c("1", "2")),
            colour = "orangered", nudge_x = -10000, nudge_y = 10000, size = 5, fontface = "bold")



png("img/SpatialEffect1.png", width = 450, height = 675)
pdf("img/SpatialEffect1.pdf", width = 6, height = 9)
p.sp.eff
dev.off()



png("img/SpatialEffect2.png", width = 450, height = 675)
pdf("img/SpatialEffect2.pdf", width = 6, height = 9)
p.sp.eff2
dev.off()


png("img/SpatialEffect_popDens.png", width = 900, height = 600)
pdf("img/SpatialEffect_popDens.pdf", width = 12, height = 8)
ggdraw() +
  draw_plot(p.sp.eff2, 0, 0, 0.57, 1) +
  draw_plot(p.spline.popDens, 0.57, 0.25, 0.43, 0.5)
dev.off()




###
### Conditional background mean NO2 surface over Germany fitted by semipar ----
###

gam.pred	<- predict(object = semipar, newdata = df.grid.DE)

df.grid.DE$gam.pred	<- as.vector(gam.pred)


#NO2 concentrations not supported by the underlying data
min(dat.back$Y)
df.grid.DE$gam.pred[df.grid.DE$gam.pred < 0]
df.grid.DE$gam.pred[df.grid.DE$gam.pred < min(dat.back$Y)]
length(df.grid.DE$gam.pred[df.grid.DE$gam.pred < min(dat.back$Y)]) # 107
length(df.grid.DE$gam.pred[df.grid.DE$gam.pred < min(dat.back$Y)])/length(df.grid.DE$gam.pred)*100		# share in % of predicted values
df.grid.DE$gam.pred[df.grid.DE$gam.pred < min(dat.back$Y)]		<- min(dat.back$Y)				# replace by min contained in data set


max(dat.back$Y)
df.grid.DE$gam.pred[df.grid.DE$gam.pred > max(dat.back$Y)]
length(df.grid.DE$gam.pred[df.grid.DE$gam.pred > max(dat.back$Y)]) # 17
length(df.grid.DE$gam.pred[df.grid.DE$gam.pred > max(dat.back$Y)])/length(df.grid.DE$gam.pred)*100		# share in % of predicted values
df.grid.DE$gam.pred[df.grid.DE$gam.pred > max(dat.back$Y)]		<- max(dat.back$Y)				# replace by max contained in data set


# Data referring to traffic/industrial sites 
DATA <- read.csv("R/DATA/Data_built/DATA_DE_traffic_industrial.csv", header=TRUE)[, -1]
dat.tr.ind <- DATA[,c(2,5:7,11:20,26:30)]

#rename some columns
names(dat.tr.ind)[c(1:4, 15)] <- c("Y", "Lon", "Lat", "Alt", "popDens")

form.gam1 <- Y ~  s(HighDens, k=-1, bs="tp") +
  s(LowDens, k=-1, bs="tp") +
  s(Ind, k=-1, bs="tp") +
  s(Transp, k=-1, bs="tp") +
  s(UrbGreen, k=-1, bs="tp") +
  s(Agri, k=-1, bs="tp") +
  s(Forest, k=-1, bs="tp") +
  s(popDens, k=-1, bs="tp") +
  s(PriRoad, k=-1, bs="tp") +
  s(SecRoad, k=-1, bs="tp") +
  s(NatMot, k=-1, bs="tp") +
  s(LocRoute, k=-1, bs="tp")

gam2 <- gam(formula=form.gam1, fit=TRUE, method="P-ML", data=dat.tr.ind, family=gaussian(),
            weights=NULL, subset=NULL, offset=NULL, optimizer=c("outer", "newton"), scale=0,
            select=TRUE, knots=NULL, sp=NULL, min.sp=NULL, H=NULL, gamma=1, paraPen=NULL, G=NULL)


form.gam.sp <- Y ~  s(Lon, Lat, k=-1, bs="tp") +
  s(Alt, k=-1, bs="tp") +
  s(HighDens, k=-1, bs="tp") +
  s(LowDens, k=-1, bs="tp") +
  s(Ind, k=-1, bs="tp") +
  s(Transp, k=-1, bs="tp") +
  s(UrbGreen, k=-1, bs="tp") +
  s(Agri, k=-1, bs="tp") +
  s(Forest, k=-1, bs="tp") +
  s(popDens, k=-1, bs="tp") +
  s(PriRoad, k=-1, bs="tp") +
  s(SecRoad, k=-1, bs="tp") +
  s(NatMot, k=-1, bs="tp") +
  s(LocRoute, k=-1, bs="tp")

gam.sp <- gam(formula=form.gam.sp, fit=TRUE, method="P-ML", data=dat.tr.ind, family=gaussian(),
              weights=NULL, subset=NULL, offset=NULL, optimizer=c("outer", "newton"), scale=0,
              select=TRUE, knots=NULL, sp=NULL, min.sp=NULL, H=NULL, gamma=1, paraPen=NULL, G=NULL)

dat.Positions$semiparTI <- as.vector(predict(object = gam.sp, newdata = dat.Positions))
#saveRDS(object = dat.Positions, file = "R/DATA/Data_built/dat.Positions.rds")


# Filter grid cells referring to Rhine-Ruhr area
df.grid.RR <- df.grid.DE[df.grid.DE$AGS %in% ind.RR, ]

#gam.tmp <- gam2
gam.tmp <- gam.sp
gam.tr.ind.pred.RR <- predict(object = gam.sp, newdata = df.grid.RR)

df.grid.RR$gam.pred.tr.ind <- as.vector(gam.tr.ind.pred.RR)

#NO2 concentrations not supported by the underlying data
min(dat.tr.ind$Y)
length(df.grid.RR$gam.pred.tr.ind[df.grid.RR$gam.pred.tr.ind < min(dat.tr.ind$Y)])
max(dat.tr.ind$Y)
length(df.grid.RR$gam.pred.tr.ind[df.grid.RR$gam.pred.tr.ind > max(dat.tr.ind$Y)])

dat.tr.ind.RR <- dat.tr.ind[ind.tmp, ]
(brks <- seq(from = min(gam.tr.ind.pred.RR, dat.tr.ind.RR$Y, dat.back$Y),# min(df.grid.DE$gam.pred), 
             to = max(gam.tr.ind.pred.RR, dat.tr.ind.RR$Y, dat.back$Y), #max(df.grid.NRW$gam.pred.tr.ind), 
             length.out = 11))
# [1]  2.525396  8.900901 15.276407 21.651912 28.027417 34.402922 40.778428 47.153933 53.529438 59.904944 66.280449
brks2 <- seq(10, 60, 10)

p.back.pred <- ggplot(df.grid.DE, aes(x = Lon.GK3, y = Lat.GK3)) +
  ggmap::theme_nothing(legend = TRUE) +  #theme_bw() +
  xlab("") +
  ylab("") +
  coord_fixed(1) +
  geom_tile(aes(fill = gam.pred), width = 1000, height = 1000, na.rm = TRUE) +
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
  # theme(legend.title = element_text(size = 14),
  #       legend.text  = element_text(size = 14),
  #       legend.background = element_blank()) +
  # guides(fill = guide_colourbar(barheight = 15))



admin.bndry.RR <- spTransform(admin.regions.RR, GK3)
admin.bndry.RR.f <- fortify(admin.bndry.RR, region = "ID")
bndry.tmp <- admin.bndry.RR.f

names(bndry.tmp)[1:2] <- c("Lon.GK3", "Lat.GK3")
p.back.pred2 <- p.back.pred +
  geom_polygon(data = bndry.tmp, color = "orangered", lwd = 0.7, fill = NA) 
  

png("img/PredBack_RR.png", width = 450, height = 675)
pdf("img/PredBack_RR.pdf", width = 6, height = 9)
p.back.pred2
dev.off()


png("img/SpEff_PredBack.png", width = 900, height = 675)
pdf("img/SpEff_PredBack.pdf", width = 12, height = 9)
plot_grid(p.sp.eff, p.back.pred, nrow = 1)
dev.off()



###
### Conditional background and traffic/industrial mean NO2 surface over RR ----
###


# Adjust!
df.grid.tmp <- df.grid.RR

p.back.pred.sub <- ggplot(df.grid.tmp, aes(x = Lon.GK3, y = Lat.GK3)) +
  ggmap::theme_nothing(legend = TRUE) +  #theme_bw() +
  xlab("") +
  ylab("") +
  coord_fixed(1) +
  geom_tile(aes(fill = gam.pred), width = 1000, height = 1000, na.rm = TRUE) +
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
# theme(legend.title = element_text(size = 14),
#         legend.text  = element_text(size = 14),
#         legend.background = element_blank()) +
#   guides(fill = guide_colourbar(barheight = 15))


p.back.pred.sub2 <- ggplot(df.grid.tmp, aes(x = Lon.GK3, y = Lat.GK3)) +
  ggmap::theme_nothing() +  #theme_bw() +
  xlab("") +
  ylab("") +
  coord_fixed(1) +
  geom_tile(aes(fill = gam.pred), width = 1000, height = 1000, na.rm = TRUE) +
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



(p.tr.ind.pred.sub <- ggplot(df.grid.tmp, aes(Lon.GK3, y = Lat.GK3)) +
  ggmap::theme_nothing() +  #theme_bw() +
  xlab("") +
  ylab("") +
  coord_fixed(1) +
  geom_tile(aes(fill = gam.pred.tr.ind), width = 1000, height = 1000, na.rm = TRUE) +
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
    guides(fill = guide_colourbar(barwidth = 20)))
#  guides(fill = guide_colourbar(barheight = 15)))


library(ggpubr)
png("img/PredBackTrIndRR.png", width = 900, height = 600)
pdf("img/PredBackTrIndRR.pdf", width = 12, height = 8)
#plot_grid(p.back.pred.sub, p.tr.ind.pred.sub, rel_widths = c(0.51, 0.45))
ggarrange(p.back.pred.sub, p.tr.ind.pred.sub, widths = c(1,1), common.legend = TRUE, legend = "bottom")
dev.off()


png("img/PredBackDERR.png", width = 900, height = 750)
#pdf("img/PredBackDERR.pdf", width = 12, height = 10)
#plot_grid(p.back.pred2, p.back.pred.sub, rel_widths = c(0.55, 0.45))
ggarrange(p.back.pred2, p.back.pred.sub, widths = c(1,1), common.legend = TRUE, legend = "bottom")
dev.off()

ggarrange(p.back.pred2, p.back.pred.sub, widths = c(1,1), common.legend = TRUE, legend = "bottom") %>%
  ggexport(filename = "img/PredBackDERR.pdf", width = 12, height = 10)


dat.Positions <- readRDS("R/DATA/Data_built/dat.Positions.rds")


## Plot points 1 to 3 (of `dat.Positions`) on map
(p.back.pred.sub2 <- p.back.pred.sub +
    geom_point(data = dat.Positions, aes(x = Lon.GK3, y = Lat.GK3), 
               pch = 19, colour = "blue", size = 2) +
    geom_text(data = dat.Positions, aes(x = Lon.GK3, y = Lat.GK3, label = c("1", "2", "3")),
              colour = "blue", nudge_x = -2500, nudge_y = 2500, size = 5, fontface = "bold"))

(p.tr.ind.pred.sub2 <- p.tr.ind.pred.sub +
    geom_point(data = dat.Positions, aes(x = Lon.GK3, y = Lat.GK3), 
               pch = 19, colour = "blue", size = 2) +
    geom_text(data = dat.Positions, aes(x = Lon.GK3, y = Lat.GK3, label = c("1", "2", "3")),
              colour = "blue", nudge_x = -2500, nudge_y = 2500, size = 5, fontface = "bold"))


png("img/PredBackTrIndRRWithPointsSP.png", width = 900, height = 600)
#pdf("img/PredBackTrIndRRWithPointsSP.pdf", width = 12, height = 6)
ggarrange(p.back.pred.sub2, p.tr.ind.pred.sub2, widths = c(1,1), common.legend = TRUE, legend = "bottom")
#plot_grid(p.back.pred.sub2, p.tr.ind.pred.sub2, rel_widths = c(0.51, 0.45))
dev.off()

ggarrange(p.back.pred.sub2, p.tr.ind.pred.sub2, widths = c(1,1), common.legend = TRUE, legend = "bottom") %>%
  ggexport(filename = "img/PredBackTrIndRRWithPointsSP.pdf", width = 12, height = 6)

#ggarrange(p.back.pred.sub2, p.tr.ind.pred.sub2, widths = c(1,1), common.legend = TRUE, legend = "bottom") %>%
#  ggexport(filename = "img/PredBackTrIndRRWithPointsSP.png", width = 900, height = 600)
