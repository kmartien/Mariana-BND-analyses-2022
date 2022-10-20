combine.Ttru.Lhos.data <- function(Ttru.msats,Ttru.strata,Lhos.msats,Lhos.strata){
  
  #merge strata
  diff.cols <- setdiff(colnames(Ttru.strata),colnames(Lhos.strata))
  na.mat <- matrix(nrow=nrow(Lhos.strata),ncol=length(diff.cols))
  colnames(na.mat) <- diff.cols
  Lhos.strata <- cbind(Lhos.strata,na.mat)
  strat.cols <- intersect(colnames(Ttru.strata),colnames(Lhos.strata))
  strata <- rbind(Ttru.strata[,strat.cols],Lhos.strata[,strat.cols])
  colnames(strata)[1] <- "LabID"

  #merge.msats
  msats <- rbind(Ttru.msats[,colnames(Ttru.msats)],Lhos.msats[,colnames(Ttru.msats)])
  colnames(msats)[1] <- "LabID"

  return(list(strata=strata, msats=msats))
}
