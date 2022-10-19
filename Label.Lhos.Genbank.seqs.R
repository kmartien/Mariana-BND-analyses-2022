rm(list=ls())
library(strataG)
library(dplyr)

setwd("/Users/Shared/KKMDocuments/Documents/Karen/Structure/Tursiops/Marianas")

#read in data files
Lhos.seq.dat <- read.fasta("Lhos KKM aligned to all Genbank.fasta")
Chen.haps <- read.csv("Chen hap labels.csv") %>% select(c(Field.ID, mtDNA.haplotype))
genbank.ids <- read.fasta("Lhos GenBank seqs Sep2022.fasta")

#parse genbank names into accession numbers and field IDs, then add hap ids from Chen et al.
genbank.ids <- names(genbank.ids) %>% strsplit(split=" ")
genbank.ids <- do.call('rbind',lapply(genbank.ids, function(i){
  c(i[1], i[which(i %in% c("voucher","clone","specimen_voucher:","haplotype"))+1])
})) %>% data.frame()
names(genbank.ids) <- c("accession.no","Field.ID")

Chen.haps <- left_join(Chen.haps, genbank.ids, by="Field.ID")

# add Chen et al. haplotype designations to names of the DNAbin object
genbank.names <- data.frame(accession.no = names(Lhos.seq.dat)) %>% left_join(Chen.haps, by="accession.no")
genbank.names$final.name <- 
  ifelse(is.na(genbank.names$mtDNA.haplotype), genbank.names$accession.no, 
               paste(genbank.names$accession.no,genbank.names$mtDNA.haplotype,sep="."))

names(Lhos.seq.dat) <- genbank.names$final.name

# identify unique haplotypes, retain my hap designation (LhXX) as the primary
Lhos.labeled <- labelHaplotypes(Lhos.seq.dat, prefix = "Lh")

final.labels <- do.call('rbind',lapply(unique(Lhos.labeled$haps), function (h){
  samps <- names(Lhos.labeled$haps)[which(Lhos.labeled$haps==h)]
  return(cbind(samps, h, samps[1]))
}))

write.csv(final.labels, file = "Lhos.KKM.and.Genbank.labels.csv")
