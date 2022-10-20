rm(list = ls())
library(swfscMisc)
library(strataG)
library(tidyverse)
library(TursiopsDataPkg)
source(file="R/CombineTtruLhosData.R")

#strata names: 2-All,3-Fine, 4-Broad, 5-HI.Fine, 6-CNMI_HIArch, 7-CNMI_Other, 8-CNMI_NoHybrids, 9-STRUCTURE_strata
strat.num <- 2
run.label <- "Ttru_Run8_subset"

data("AS183.msats")
data("AS183.strata")
data("AS179.msats")
data("AS179.strata")

comb.data <- combine.Ttru.Lhos.data(AS183.msats,AS183.strata,AS179.msats,AS179.strata)
msats <- comb.data$msats
strata <- comb.data$strata
loc.col <- ncol(strata)+1

msat.merge <- right_join(strata, msats, by = "LabID") %>% data.frame()
msat.gtypes <- df2gtypes(msat.merge, ploidy = 2, id.col = 1, strata.col = strat.num, loc.col = loc.col, description = run.label)

popflag <- data.frame(LabID = as.numeric(getIndNames(msat.gtypes))) %>% 
  left_join(strata, by= "LabID") %>% transmute(flag = ifelse(CNMI_Other == "CNMI",0,1))

# Run STRUCTURE
sr <- structureRun(msat.gtypes, k.range = 2:2, num.k.rep = 2, label = run.label, delete.files = FALSE, num.cores = 2, 
                    burnin = 10000, numreps = 50000, noadmix = FALSE, freqscorr = FALSE, 
                    pop.prior = "usepopinfo", popflag=popflag$flag)

save.image(file=paste(run.label,"_sr.rdata",sep=""))

# Calculate Evanno metrics
#evno <- structure.evanno(sr)
#print(evno)

# Run CLUMPP to combine runs for K = 2
clumpp2 <- clumpp.run(sr, k = 2)
#clumpp2$orig.pop <- strata.num
#print(clumpp2)
# Plot CLUMPP results
x <- rownames(msat.gtypes$genotypes)
orig.pop <- subset(strata,LabID %in% x)
clumpp2$orig.pop <- orig.pop[order(as.numeric(orig.pop$LabID)),9]
orig.pop <- sapply(1:nrow(clumpp2), function(x){
  switch(clumpp2[x,3],Lhos=1,CNMI=2,WPac=3,Pelagic=4,HIArch=5)
})
clumpp2$orig.pop <- orig.pop
structure.plot(clumpp2,sort.probs=FALSE)

save.image(file=paste(run.label,"_all.rdata",sep=""))