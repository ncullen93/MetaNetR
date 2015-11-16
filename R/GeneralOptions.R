
# COMPLETE -> 9/13/15

# GOOD
get.plot.layout.options <- function(net){
  to.return <- vector("list",2)
  if (igraph::vcount(net) > 750){
    to.return$values <- list(igraph::layout.auto,
                             igraph::layout.fruchterman.reingold.grid(net),
                             igraph::layout.fruchterman.reingold(net),
                             igraph::layout.reingold.tilford(net),
                             igraph::layout.reingold.tilford(net,circular=T))
    to.return$explanation <- c("layout.auto",
                               "layout.fruchterman.reingold.grid",
                               "layout.fruchterman.reingold",
                               "layout.reingold.tilford",
                               "layout.reingold.tilford(circular=T)")
  }
  else{
    to.return$values <- list(igraph::layout.auto,
                             igraph::layout.circle(net),
                             igraph::layout.fruchterman.reingold(net),
                             igraph::layout.fruchterman.reingold.grid(net),
                             igraph::layout.reingold.tilford(net,circular=T),
                             igraph::layout.reingold.tilford(net),
                             igraph::layout.sphere(net),
                             igraph::layout.kamada.kawai(net),
                             igraph::layout.sugiyama(net)$layout)
    to.return$explanation <- c("layout.auto",
                               "layout.circle",
                               "layout.fruchterman.reingold",
                               "layout.fruchterman.reingold.grid",
                               "layout.reingold.tilford(circular=T)",
                               "layout.reingold.tilford",
                               "layout.sphere",
                               "layout.kamada.kawai",
                               "layout.sugiyama")
  }
  return(to.return)
}