library(strataG)
library(dplyr)
library(TursiopsDataPkg)

seqs <- read.fasta("data-raw/All_Ttru_sequences.fasta")
seqs <- c(seqs, read.fasta("data-raw/Lhos unique haps.fasta"))

Ttru.strata <- read.csv("data-raw/Ttru_strata.csv")
all.ttru <- read.csv("data-raw/All_Ttru_hap_and_location.csv") %>%
  select(-Broad) %>%
  filter(Use==TRUE) %>% left_join(Ttru.strata, by="AnimalID")
all.gtype <- df2gtypes(all.ttru[,c("LabID","Broad","Haplotype")], ploidy = 1, sequences = seqs)
strat.gtype <- strataSplit(all.gtype)

#summaries with all CNMI samples
diversity.CNMI <- strataG::summarizeLoci(strat.gtype$CNMI)
diversity.CNMI$nucDiv <- mean(nucleotideDiversity(strat.gtype$CNMI))

#summaries without Lhos haps
MI.df.noLhos <- all.ttru[-which(all.ttru$Haplotype %in% c("Lh1","Lh11")),] %>%
  filter(Broad=="CNMI")

MI.gtype.noLhos <- df2gtypes(MI.df.noLhos[,c("LabID","Broad","Haplotype")], ploidy = 1, sequences = seqs)
diversity.CNMI.noLhos <- summarizeLoci(MI.gtype.noLhos)
diversity.CNMI.noLhos$nucDiv <- mean(nucleotideDiversity(MI.gtype.noLhos))

#summaries of all BNDs
diversity.Ttru <- summarizeLoci(all.gtype)
diversity.Ttru$nucDiv <- mean(nucleotideDiversity(all.gtype))

#summaries of all BNDs without Lhos haps
Ttru.noLhos.df <- all.ttru[-which(all.ttru$Haplotype %in% c("Lh1","Lh11")),]
Ttru.noLhos.gtype <- df2gtypes(Ttru.noLhos.df[,c("LabID","Broad","Haplotype")], ploidy = 1, sequences = seqs)
diversity.Ttru.noLhos <- summarizeLoci(Ttru.noLhos.gtype)
diversity.Ttru.noLhos$nucDiv <- mean(nucleotideDiversity(Ttru.noLhos.gtype))

#summaries with all HI samples
diversity.HI <- summarizeLoci(strat.gtype$HI)
diversity.HI$nucDiv <- mean(nucleotideDiversity(strat.gtype$HI))

overall.sum <- rbind(diversity.CNMI.noLhos, diversity.CNMI,diversity.HI, diversity.Ttru.noLhos, diversity.Ttru)
overall.sum$locus <- c("CNMInoLhos","allCNMI","HI","Ttru.noLhos","allTtru")

stats <- pairwiseTest(all.gtype,nrep=10000)

save.image(file="CNMI.diversity.summary.Rdata")
