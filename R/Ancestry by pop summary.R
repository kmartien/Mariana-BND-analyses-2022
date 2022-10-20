strata <- unique(clumpp2$orig.pop)
#CNMI.samps <- subset(clumpp2,orig.pop=="CNMI")
#other.samps <- subset(clumpp2,orig.pop %in% strata[1:6])
CNMI.samps <- subset(clumpp2,orig.pop==2)
other.Ttru.samps <- subset(clumpp2,orig.pop > 2)
HIArch.samps <- subset(clumpp2,orig.pop==5)
Lhos.samps <- subset(clumpp2,orig.pop==1)
hybrids <- subset(CNMI.samps,id %in% c("104066","108207","108208","116867","116868"))
westPac.samps <- subset(clumpp2,orig.pop ==3)

CNMI.mean <- colMeans(CNMI.samps[,4:5])
other.Ttru.mean <- colMeans(other.Ttru.samps[,4:5])
HIArch.mean <- colMeans(HIArch.samps[,4:5])
Lhos.mean <- colMeans(Lhos.samps[,4:5])
hybrids.mean <- colMeans(hybrids[,4:5])
westPac.mean <- colMeans(westPac.samps[,4:5])
CNMI.ids <- CNMI.samps$id


CNMI.samps.8 <- subset(run8,id %in% CNMI.ids)
other.samps <- subset(run8,orig.pop=="Ttru")
other.samps <- other.samps[-which(other.samps[,1] %in% CNMI.ids),]
Lhos.samps <- subset(run8,orig.pop == "Lhos")
hybrids <- subset(CNMI.samps,id %in% c("104066","108207","108208","116867","116868"))

