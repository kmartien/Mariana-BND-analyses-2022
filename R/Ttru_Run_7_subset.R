rm(list = ls())
library(swfscMisc)
library(strataG)
library(tidyverse)
library(TursiopsDataPkg)
source(file="R/CombineTtruLhosData.R")
source(file="R/Combine.STRUCTURE.runs.R")

#strata names: 2-All,3-Fine, 4-Broad, 5-HI.Fine, 6-CNMI_HIArch, 7-CNMI_Other, 8-CNMI_NoHybrids, 9-STRUCTURE_strata
strat.num <- 2
run.label <- "Ttru_Run7_subset"
num.subsamples <- 10

data("AS183.msats")
data("AS183.strata")
data("AS179.msats")
data("AS179.strata")

comb.data <- combine.Ttru.Lhos.data(AS183.msats,AS183.strata,AS179.msats,AS179.strata)
msats <- comb.data$msats
strata <- comb.data$strata
loc.col <- ncol(strata)+1

msat.merge <- right_join(strata, msats, by = "LabID") %>% data.frame() %>% arrange(CNMI_Other)
n.CNMI.Lhos <- length(which(msat.merge$CNMI_Other != "Ttru"))

sr <- lapply(1:num.subsamples, function(i){
  to.keep <- sort(c(1:n.CNMI.Lhos, sample((n.CNMI.Lhos+1):nrow(msat.merge),25)))
  msat.merge.subset <- msat.merge[to.keep,]
  msat.gtypes <- df2gtypes(msat.merge.subset, ploidy = 2, id.col = 1, strata.col = strat.num, loc.col = loc.col, description = run.label)
  
  popflag <- data.frame(LabID = as.numeric(getIndNames(msat.gtypes))) %>% 
    left_join(strata, by= "LabID") %>% transmute(flag = 1)
  
  # Run STRUCTURE
  structureRun(msat.gtypes, k.range = 2:2, num.k.rep = 1, label = paste0(run.label,i), delete.files = FALSE, num.cores = 2, 
               burnin = 100000, numreps = 500000, noadmix = FALSE, freqscorr = FALSE, 
               pop.prior = "usepopinfo", popflag=popflag$flag)
})
names(sr) <- paste0(run.label, 1:num.subsamples)
save.image(file=paste(run.label,"_sr.rdata",sep=""))

group.1.ancestry <- combine.STRUCTURE.runs(sr, strat = select(strata, c(id = LabID, CNMI_Other)))

save.image(file=paste(run.label,"_all.rdata",sep=""))

# Calculate Evanno metrics
#evno <- structure.evanno(sr)
#print(evno)

##### CLUMPP currently won't run with strataG
# Run CLUMPP to combine runs for K = 2
#clumpp2 <- clumpp(sr, k = 2)
##clumpp2$orig.pop <- strata.num
##print(clumpp2)
## Plot CLUMPP results
#x <- rownames(msat.gtypes$genotypes)
#orig.pop <- subset(strata,LabID %in% x)
#clumpp2$orig.pop <- orig.pop[order(as.numeric(orig.pop$LabID)),9]
#orig.pop <- sapply(1:nrow(clumpp2), function(x){
#  switch(clumpp2[x,3],Lhos=1,CNMI=2,WPac=3,Pelagic=4,HIArch=5)
#})
#clumpp2$orig.pop <- orig.pop
#structure.plot(clumpp2,sort.probs=FALSE)

#save.image(file=paste(run.label,"_all.rdata",sep=""))