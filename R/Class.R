
# COMPLETE -> LAST UPDATED 9/13/15

#class "metanet" for effective storage/manipulation/analysis of individual metagenomic networks

metanet <- function(data.list=list(),
                    metadata=list(),
                    lpv.map=list(),
                    networks=list(),
                    combined.networks=list()){
  UseMethod("metanet")
}
metanet.default <- function(data.list=list(),
                           metadata=list(),
                           lpv.map=list(),
                           networks=list(),
                           combined.networks=list()){ 
  #class structure
  mn <- list(data.list=data.list,
             metadata=metadata,
             lpv.map=lpv.map,
             networks=networks,
             combined.networks=combined.networks)
  class(mn) <- "metanet"
  return(mn)
}
