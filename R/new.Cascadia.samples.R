library(TursiopsDataPkg)

data("AS.2012.paper.hap.samps")
data("AS183.haps")
data("AS183.msats")
data("archive.data")

new.mtDNA.samps <- AS183.haps[-which(AS183.haps$AnimalID %in% AS.2012.paper.hap.samps$LABID),]
names(new.samps)[1] <- "LABID" 
new.mtDNA.samp.data <- archive.data[which(archive.data$LABID %in% new.mtDNA.samps$LABID),]

new.msat.samps <- AS183.msats[-which(AS183.msats$LABID %in% AS.2012.paper.hap.samps$LABID),]
new.msat.samp.data <- archive.data[which(archive.data$LABID %in% new.msat.samps$LABID),]

write.csv(new.mtDNA.samp.data, file = "results/new.mtDNA.samples.archive.data.csv")
write.csv(new.msat.samp.data, file = "results/new.msat.samples.archive.data.csv")
