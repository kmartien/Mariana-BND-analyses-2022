rm(list = ls())
library(swfscMisc)
library(tidyverse)

load("../Mariana-BND-STRUCTURE-2014/December Ahi runs/Run8/Ttru_Run8_all.rdata")

CNMI.inds <- msat.merge$LabID[which(msat.merge$CNMI_Other == "CNMI")]

run8.q.mat <- clumpp2[which(clumpp2$id %in% CNMI.inds),c(1,5)]

load("results/Ttru_Run8_subset_all.rdata")

run8.q.mat$Subset <- 1-as.numeric(t(group.1.ancestry[[2]][2,]))
names(run8.q.mat)[1:2] <- c("LABID","AllSamps")

# Summarize Run 7
load("../Mariana-BND-STRUCTURE-2014/December Ahi runs/Run7/Ttru_Run7_all.rdata")

prior.ancestry <- lapply(sr, function(r){
  anc <- r$prior.anc[which(names(r$prior.anc) %in% CNMI.inds)]
  Lhos.anc.row <- ifelse((r$q.mat[which(r$q.mat$orig.pop == "All")[1],4] > r$q.mat[which(r$q.mat$orig.pop == "All")[1],5]), 2, 1)
  anc.sum <- do.call('rbind',lapply(anc, function(i){
    i[Lhos.anc.row,]
  }))
})

prior.ancestry <- do.call('cbind', lapply(prior.ancestry, function(x){x}))
run7.prior.anc.smry <- clumpp2$prob.2[which(clumpp2$id %in% CNMI.inds)] %>% 
  cbind(rowSums(prior.ancestry[,seq(from = 2, to = 29, by = 3)])/10) %>%
  cbind(rowSums(prior.ancestry[,seq(from = 3, to = 30, by = 3)])/10)
colnames(run7.prior.anc.smry) <- c("Lhos.prob","Gen.1","Gen.2")

load("results/Ttru_Run7_subset_all.rdata")

run7.prior.anc.smry <- cbind(run7.prior.anc.smry, (1-as.numeric(group.1.ancestry[[2]][2,]))) %>%
  cbind(group.1.ancestry[[3]])
colnames(run7.prior.anc.smry) <- c("Lhos.prob","Gen.1","Gen.2","Lhos.prob.sub","Gen.1.sub","Gen.2.sub")

save(CNMI.inds, run8.q.mat, run7.prior.anc.smry, file = "results/Summary.run7.run8.rda")
write.csv(run8.q.mat, file = "results/run8.q.mat.csv")
write.csv(run7.prior.anc.smry, file = "results/run7.prior.ancestry.summary.csv")
