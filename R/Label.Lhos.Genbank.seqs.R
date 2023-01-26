library(strataG)
library(dplyr)
library(TursiopsDataPkg)

data(AS179.haps)
AS179.haps$LABID <- AS179.haps$AnimalID

#read in data files
Lhos.seq.dat <- read.fasta("data-raw/Lhos KKM aligned to all Genbank.fasta")
Chen.haps <- read.csv("data-raw/Chen hap labels.csv") %>% 
  select(c(FIELDID, Archive.institution, mtDNA.haplotype)) %>%
  filter(mtDNA.haplotype != "")
genbank.ids <- read.fasta("data-raw/Lhos GenBank seqs Sep2022.fasta")
archive.data <- read.csv("data-raw/Pac-and-IO-Frasers-dolphins.csv")

#parse genbank names into accession numbers and field IDs, then add hap ids from Chen et al.
genbank.ids <- names(genbank.ids) %>% strsplit(split=" ")
genbank.ids <- do.call('rbind',lapply(genbank.ids, function(i){
  c(i[1], i[which(i %in% c("voucher","clone","specimen_voucher:","haplotype"))+1])
})) %>% data.frame()
names(genbank.ids) <- c("accession.no","FIELDID")

Chen.haps <- left_join(Chen.haps, genbank.ids, by="FIELDID")

# add Chen et al. haplotype designations to names of the DNAbin object
genbank.names <- data.frame(accession.no = names(Lhos.seq.dat)) %>% left_join(Chen.haps, by="accession.no")
genbank.names$final.name <- 
  ifelse(is.na(genbank.names$mtDNA.haplotype), genbank.names$accession.no, 
               paste(genbank.names$accession.no,genbank.names$mtDNA.haplotype,sep="-"))

names(Lhos.seq.dat) <- genbank.names$final.name

# identify unique haplotypes, retain my hap designation (LhXX) as the primary
Lhos.labeled <- labelHaplotypes(Lhos.seq.dat, prefix = "Lh")

final.labels <- data.frame(do.call('rbind',lapply(unique(Lhos.labeled$haps), function (h){
  samps <- names(Lhos.labeled$haps)[which(Lhos.labeled$haps==h)]
  chen.haps <- do.call('rbind',lapply(strsplit(samps, "-"), function(s){s[[length(s)]]}))
  ind.ids <- do.call('rbind',lapply(strsplit(samps, "-"), function(s){s[[1]]}))
  return(cbind(samps, accession.no = ind.ids, h, kkm.hap = samps[1], chen.hap = chen.haps))
})))
names(final.labels) <- c("final.name","accession.no","h","kkm.hap","chen.hap")

##################
swfsc.samps.used.by.chen <- filter(Chen.haps, FIELDID %in% archive.data$FIELDID) %>%
  left_join(select(archive.data, c(LABID, FIELDID)), by = "FIELDID") %>%
  left_join(final.labels, by = "accession.no") %>%
  left_join(AS179.haps, by = "LABID")
write.csv(swfsc.samps.used.by.chen, file = "results/SWFSC.Chen.hap.comparison.csv")

hap.key <- final.labels %>% transmute(comb.hap = paste0(kkm.hap,"-",chen.hap)) %>% 
  unique() 
hap.key <- data.frame(do.call('rbind', strsplit(hap.key$comb.hap, "-" )))
names(hap.key) <- c("KKM.hap","Chen.hap")

write.csv(final.labels, file = "results/Lhos.KKM.and.Genbank.labels.csv")
write.csv(hap.key, file = "results/Lhos-KKM-to-Chen-hap-key.csv", row.names = FALSE)





