library(dplyr)
library(ggplot2)
library(TursiopsDataPkg)
library(RColorBrewer)
library(grid)
library(gridExtra)

pal <- brewer.pal(8, "Greys")
sighting.dat <- read.csv("data-raw/Sighting_data.csv")
#sighting.dat$col <- "black"
#sighting.dat$col[which(sighting.dat$Haplotype %in% c("9","43"))] <- pal[1]
#sighting.dat$col[which(sighting.dat$Haplotype %in% c("39","40"))] <- pal[2]
#sighting.dat$col[which(sighting.dat$Haplotype %in% c("33","34"))] <- pal[3]
#sighting.dat$col[which(sighting.dat$Haplotype == "32")] <- pal[4]
sighting.dat$pat <- "point"
sighting.dat$pat[which(sighting.dat$Haplotype %in% c("32","34","40","9","LH1"))] <- "none"

g1 <- ggplot(data = sighting.dat, aes(x = NumYearsSeen, fill = Haplotype)) +
  geom_bar()

g1 <- ggplot(data = sighting.dat, aes(x = NumYearsSeen, fill = Haplotype, pattern = pat)) +
  geom_bar_pattern(color = "black",
                   pattern_fill = "black",
                   pattern_angle = 45,
                   pattern_density = 0.1,
                   pattern_spacing = 0.025,
                   pattern_key_scale_factor = 0.6) + 
  scale_fill_manual(name = "Haplotype", 
                    values = c("white", pal[1], pal[2], pal[3], pal[4], pal[5], pal[6], "black"),
                    labels = c("32", "33", "34", "39", "40", "43", "9", "LH1")) +
  scale_pattern_manual(name = "Haplotype",
                       values = c("none", "stripe", "point", "none", "stripe", "point", "stripe", "none", "none"),
                       labels = c("32", "33", "34", "39", "40", "43", "9", "LH1")) +
  labs(x = "No. of Years Encountered", y = "No. of Individuals") +
  theme_minimal() + 
  theme(plot.title =  element_text(size=30), axis.title.x =  element_text(size=30), 
        axis.title.y = element_blank(), axis.text = element_text(size=25), legend.position = "none")
g1

g2 <- ggplot(data = sighting.dat, aes(x = SightingHistoryLength, fill = Haplotype, pattern = pat)) +
  geom_bar_pattern(color = "black",
                   pattern_fill = "black",
                   pattern_angle = 45,
                   pattern_density = 0.1,
                   pattern_spacing = 0.025,
                   pattern_key_scale_factor = 0.6) + 
  scale_fill_manual(name = "Haplotype", 
                    values = c("white", pal[1], pal[2], pal[3], pal[4], pal[5], pal[6], "black"),
                    labels = c("32", "33", "34", "39", "40", "43", "9", "LH1")) +
  scale_pattern_manual(name = "Haplotype",
                    values = c("none", "stripe", "point", "none", "stripe", "point", "stripe", "none", "none"),
                    labels = c("32", "33", "34", "39", "40", "43", "9", "LH1")) +
  labs(x = "Length of Encounter History (Years)", y = "No. of Individuals") +
  theme_minimal() + 
  theme(plot.title =  element_text(size=30), axis.title.x =  element_text(size=30), 
        axis.title.y = element_blank(), axis.text = element_text(size=25), legend.position = "none")
g2

tiff(file = "results/resights-by-haplotype.tiff", height = 1000, width = 2000)
grid.arrange(g1, g2, nrow = 1)
dev.off()
