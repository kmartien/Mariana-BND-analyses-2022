library(swfscMisc)
library(strataG)
library(dplyr)
library(viridis)

discrete_palette <- viridis(n=3)[c(3,1,2)]
load("../Mariana-BND-STRUCTURE-2014/December Ahi runs/Run1/Ttru_Run1_all.rdata")

run1.k2.res <- clumpp2
# renaming origins so that samples are sorted as I want
run1.k2.res$orig.pop[which(run1.k2.res$orig.pop == "CNMI")] <- "B.CNMI"
run1.k2.res$orig.pop[which(run1.k2.res$orig.pop == "WPac")] <- "C.WPac"
run1.k2.res$orig.pop[which(run1.k2.res$orig.pop == "Pelagic")] <- "D.Pelagic"
run1.k2.res$orig.pop[which(run1.k2.res$orig.pop == "HIArch")] <- "E.HIArch"

jpeg("results/Run1.k2.B&W.jpg", width = 2000, height = 1000)
structurePlot(run1.k2.res,sort.probs=FALSE, horiz = FALSE, type = "bar", 
              col = c("grey50", "grey90"), #col = c(discrete_palette[3], discrete_palette[1]), 
              label.pops = TRUE, legend.position = "none")
dev.off()

load("../Mariana-BND-STRUCTURE-2014/December Ahi runs/Run3/Ttru_Run3_all.rdata")

#Need to sort run3.k3.res so that Lhos is first, then CNMI, WPac, Pelagic, HI
run3.k3.res <- clumpp3
# renaming origins so that samples are sorted as I want
run3.k3.res$orig.pop[which(run3.k3.res$orig.pop == "Lhos")] <- "A.Lhos"
run3.k3.res$orig.pop[which(run3.k3.res$orig.pop == "CNMI")] <- "B.CNMI"
run3.k3.res$orig.pop[which(run3.k3.res$orig.pop == "WPac")] <- "C.WPac"
run3.k3.res$orig.pop[which(run3.k3.res$orig.pop == "Pelagic")] <- "D.Pelagic"
run3.k3.res$orig.pop[which(run3.k3.res$orig.pop == "HIArch")] <- "E.HIArch"

jpeg("results/Run3.k3.B&W.jpg", width = 2000, height = 1000)
structurePlot(run3.k3.res,sort.probs=FALSE, horiz = FALSE, type = "bar", 
              col = c("grey90", "grey30", "grey50"), #col = c(discrete_palette), 
              label.pops = TRUE, legend.position = "none")
dev.off()