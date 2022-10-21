#' Combine assignments across structure runs
#' 
#' This was written as an alternative to CLUMPP, which currently doesn't compile
#' correctly with strataG, according to Eric. This function was specifically
#' designed to summarize run 8 of the Ttru-Lhos hybrid project and SHOULD NOT
#' BE USED for any other STRUCTURE runs without testing and modification!

combine.STRUCTURE.runs <- function(sr, strat){

  
  col.order <- list(1:5, c(1:3,5,4))
  CNMI.inds <- strat$id[which(strat$CNMI_Other=="CNMI")]
  
  grp1.ancestry <- lapply(sr, function(r){
    q <- r[[1]]$q.mat
    q$id <- as.numeric(q$id)
    
    # Reorder columns so that Group.1 is always Tursiops and Group.2 is Lhos
    grp1 <- ifelse(q[1,4] > q[1,5], 1, 2)
    q <- q[,col.order[[grp1]]]
    names(q)[4:5] <- c("Group.1","Group.2")
    
    #extract data for CNMI individuals
    CNMI.q <- q[which(q$id %in% CNMI.inds),]
    
    #calculate mean, max, and min ancestry to Group.1 for Lhos, CNMI, and Ttru
    mean.grp1 <- left_join(q, strat, by= "id") %>%
      group_by(CNMI_Other) %>% summarise(mean = mean(Group.1))
    max.grp1 <- left_join(q, strat, by= "id") %>%
      group_by(CNMI_Other) %>% summarise(max = max(Group.1))
    min.grp1 <- left_join(q, strat, by= "id") %>%
      group_by(CNMI_Other) %>% summarise(min = min(Group.1))
    grp1 <- left_join(mean.grp1, max.grp1, by="CNMI_Other") %>% left_join(min.grp1, by= "CNMI_Other")
    return(list(grp1=grp1, CNMI.q=CNMI.q))
  })
  
  ancestry.sum <- data.frame(cbind(
    mean = rowMeans(do.call('cbind', lapply(grp1.ancestry, function(i){i$grp1$mean}))),
    max = rowMeans(do.call('cbind', lapply(grp1.ancestry, function(i){i$grp1$max}))),
    min = rowMeans(do.call('cbind', lapply(grp1.ancestry, function(i){i$grp1$min})))
  ))
  rownames(ancestry.sum) <- grp1.ancestry[[1]]$grp1$CNMI_Other

  CNMI.ancestry.sum <- rowMeans(do.call('cbind', lapply(grp1.ancestry, function(i){
    i$CNMI.q$Group.1
  })))
  CNMI.ancestry.sum <- data.frame(rbind(as.character(grp1.ancestry[[1]]$CNMI.q$id), CNMI.ancestry.sum))
  
  return(list(ancestry.sum, CNMI.ancestry.sum))  
}