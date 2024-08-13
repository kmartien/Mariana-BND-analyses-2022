library(maps)
library(mapdata)
library(marmap)
library(sp)
library(dplyr)
library(viridis)
library(RColorBrewer)
library(TursiopsDataPkg)

data("archive.data")
samp.dat <- read.csv("data-raw/CNMI-hap-and-sex.csv") %>% left_join(select(archive.data, c(LABID, Latitude, Longitude, YR, MO, DA)), by = "LABID")

sighting.dat <- read.csv("data-raw/PIFSC Marianas bottlenose dolphin sightings.csv")
sightings.wo.samples <- filter(sighting.dat, BiopsySamples == FALSE)

MI.bathy <- getNOAA.bathy(lon1 = 141, lon2 = 151,
                          lat1 = 11, lat2 = 24, resolution = 1, keep = TRUE, path = "data/")

tracklines <- read.csv("data-raw/PIFSC_tracklines.csv")

discrete_palette <- viridis(n = 5)[5:1]
samp.dat$pch <- 24
samp.dat$col <- "grey60"
samp.dat$size <- 0.8
samp.dat$col[which(samp.dat$Haplotype == 32)] <- discrete_palette[2]
samp.dat$col[which(samp.dat$Haplotype %in% c("Lh1","Lh11"))] <- discrete_palette[1]

loc.labels <- data.frame(matrix(c(145.5, 13.5, "Guam", 1,
                                  145.8, 14.1, "Rota", 1,
                                  #147, 15.3, "Saipan", 1,
                                  147, 15, "3-Islands", 1,
                                  #146.6, 14.7, "Aguijan", 1,
                                  146.5, 18, "Pagan", 1,
                                  146.9, 16.35, "Anatahan", 1,
                                  145.7, 20.6, "Farallon de\nParajos", 1,
                                  146.4, 18.8, "Agrihan", 1),
                                ncol = 4, byrow = TRUE))
names(loc.labels) <- c("long","lat","text","size")

jpeg(file="results/Mariana-sighting-sample-map_500m-isobath_viridis-tracklinesUPDATE.jpg", width = 1000, height = 1500, pointsize = 30)
map('worldHires', xlim=c(141,151), ylim=c(11,24), fill=TRUE, col="darkgrey")
compassRose(x = 143, y = 13, cex = 0.4)
blues <- colorRampPalette(brewer.pal(9, "Blues")[9:1])
plot(MI.bathy, image = TRUE, land = TRUE, 
     bpal = list(c(0, max(MI.bathy), "grey85"), c(min(MI.bathy), 0, blues(100))),
     deep = c(-10000, -2000, 0),
     shallow = c(-3000, -500, 0),
     step = c(1000, 500, 0),
     lwd = c(0.8, 0.8, 1), lty = c(1, 1, 1),
     col = c("lightgrey", "darkgrey", "black"),
     drawlabel = c(FALSE, FALSE, FALSE), 
     xlab = "Longitude (\u00B0E)", ylab = "Latitude (\u00B0N)")
for (i in 1:nrow(tracklines)){
  lines(x=c(tracklines$lon1[i],tracklines$lon2[i]), y = c(tracklines$lat1[i], tracklines$lat2[i]), col = "grey25")
}
points(x = sightings.wo.samples$Long, y = sightings.wo.samples$Lat, pch = 21, bg = "grey15", cex = 0.8)
points(x=samp.dat$Longitude, y=samp.dat$Latitude, pch = samp.dat$pch, bg=samp.dat$col, col = "black", cex=samp.dat$size)
text(x=as.numeric(loc.labels$long),y=as.numeric(loc.labels$lat),labels=loc.labels$text, cex=as.numeric(loc.labels$size))
compassRose(x = 150, y = 13, cex = 0.4)
scaleBathy(MI.bathy, deg = .91, x = "bottomright", inset = 5)
dev.off()

jpeg(file = "results/world-map-with-rectangle-around-MI.jpg")
map('worldHires', xlim=c(100,180), ylim=c(-19,45), fill=TRUE, col="lightgrey")
rect(141,11,150,24, lwd = 5, border = "#4292C6")
box()
dev.off()
