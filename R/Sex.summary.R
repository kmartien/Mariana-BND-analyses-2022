library(TursiopsDataPkg)
library(dplyr)

Lhos.ind.info <- read.csv("data-raw/Lhos.ind.info.csv")
data("AS179.haps")
Lhos.sexes <- left_join(AS179.haps, Lhos.ind.info)
table(Lhos.sexes$Sex)

data("archive.data")
data("AS183.haps")
names(AS183.haps)[1] <- "LABID"
sexes.2012 <- read.csv("data-raw/sexes.2012.samps.csv")
cnmi.strata <- c("CNMI - Aguijan","CNMI - Guam", "CNMI - Rota", "CNMI - Saipan")
non.cnmi.samps <- AS183.haps[-which(AS183.haps$Location %in% cnmi.strata),] %>%
  left_join(select(archive.data, c("LABID","SEX"))) %>% 
  left_join(sexes.2012)
non.cnmi.samps$SEX[which(non.cnmi.samps$Sex %in% c("M","F"))] <- non.cnmi.samps$Sex[which(non.cnmi.samps$Sex %in% c("M","F"))]
table(non.cnmi.samps$SEX)

HI.eez.samps <- filter(non.cnmi.samps, Location %in% c("MHI", "NWHI"))

WNP.samps <- filter(non.cnmi.samps, Location %in% c("Philippines", "TAIWAN","Korea"))
