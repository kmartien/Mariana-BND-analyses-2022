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

MI.bathy <- getNOAA.bathy(lon1 = 142, lon2 = 147,
                           lat1 = 12, lat2 = 21, resolution = 1, keep = TRUE, path = "data/")

discrete_palette <- viridis(n = 5)[5:1]
samp.dat$pch <- 24
#samp.dat$col <- "darkviolet"
samp.dat$col <- "grey60"
samp.dat$size <- 0.8
samp.dat$col[which(samp.dat$Haplotype == 32)] <- discrete_palette[2]
samp.dat$col[which(samp.dat$Haplotype %in% c("Lh1","Lh11"))] <- discrete_palette[1]
#samp.dat$col[which(samp.dat$Haplotype == 32)] <- "#F0E442" #yellow triangles
#samp.dat$col[which(samp.dat$Haplotype %in% c("Lh1","Lh11"))] <- "chartreuse2" #green triangles

loc.labels <- data.frame(matrix(c(145.5, 13.5, "Guam", 1,
                                  145.8, 14.1, "Rota", 1,
                                  144.6, 15.2, "Saipan, Tinian,\nAguijan\n(3-Islands)", 1,
                                  145, 18, "Pagan", 1,
                                  145, 16.35, "Anatahan", 1,
                                  145.7, 20.6, "Farallon de\nParajos", 1,
                                  144.8, 18.8, "Agrihan", 1),
                                ncol = 4, byrow = TRUE))
names(loc.labels) <- c("long","lat","text","size")

jpeg(file="results/Mariana-sighting-sample-map_500m-isobath_viridis.jpg", width = 1000, height = 1500, pointsize = 30)
map('worldHires', xlim=c(142,147), ylim=c(12,21), fill=TRUE, col="darkgrey")
#axis(1, at = seq(110,150,10), parse(text=degreeLabelsEW(seq(110,150,10))), cex.axis=0.6)
#axis(2, at = seq(-45,-10,5), parse(text=degreeLabelsNS(seq(-45,-10,5))), cex.axis=0.6)
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
points(x = sighting.dat$Long, y = sighting.dat$Lat, pch = 21, bg = "grey15", cex = 0.8)
points(x=samp.dat$Longitude, y=samp.dat$Latitude, pch = samp.dat$pch, bg=samp.dat$col, col = "black", cex=samp.dat$size)
text(x=as.numeric(loc.labels$long),y=as.numeric(loc.labels$lat),labels=loc.labels$text, cex=as.numeric(loc.labels$size))
compassRose(x = 143, y = 13, cex = 0.4)
scaleBathy(MI.bathy, deg = .918, x = "bottomleft", inset = 5)
dev.off()

jpeg(file = "results/world-map-with-rectangle-around-MI.jpg")
map('worldHires', xlim=c(60,180), ylim=c(-45,60), fill=TRUE, col="lightgrey")
rect(142,12,147,21, lwd = 5, border = "#4292C6")
box()
dev.off()