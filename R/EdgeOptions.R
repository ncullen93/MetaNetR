
# NOT COMPLETE -> 9/13/15

# GOOD
get.edge.color.options <- function(net){
  to.return <- vector("list", 2)
  
  #LEVEL COLOR
  color.map <- c('black','black','blue','green','red')
  
  sink <- as.numeric(V(net)[V(net)$name=="SINK"])  
  paths<-get.all.shortest.paths(net,from=sink, weights=rep(1,ecount(net)))
  levels<-unlist(lapply(paths$res, length))[1:vcount(net)]
  level.list <- list(c(),c(),c())
  for (i in 1:length(levels)){
    for (idx in 1:max(levels)){
      if (levels[i]==idx){
        level.list[[idx]] <- c(level.list[[idx]],i)
        break
      }
    }
  }
  v.idx <- 1:vcount(net)
  for (v in v.idx){ 
    for (lvl in 1:length(level.list)){ 
      if (v %in% level.list[[lvl]]){
        E(net)[from(v)]$level.color <- color.map[lvl]
        break
      }
    }
  }
  
  level.color <- E(net)$level.color
  
  #BETWEENNESS COLOR
  d <- igraph::edge.betweenness(net,weights=rep(1,igraph::ecount(net)))
  d <- (d - min(d)) / (max(d) - min(d))
  d <- as.numeric(factor(d))
  d <- ifelse(d >= 11,11,d)
  pal <- rev(RColorBrewer::brewer.pal(11,"RdYlBu"))
  pal1 <- RColorBrewer::brewer.pal(11,'RdYlBu')
  between.color.1 <- pal[d]
  between.color.2 <- pal1[d]
  
  
  
 to.return$values <- list("black",
                         level.color,
                         between.color.1,
                         between.color.2)
 to.return$explanation <- c("All=Black",
                            "Hierarchy-Based",
                            "Betweenness-Based",
                            "Rev. Betweenness-Based")
 return(to.return)
 
}

# GOOD
get.edge.width.options <- function(net){
  
  to.return <- vector("list",2)
  ## Hierarchy level
  width.map.1 <- c(1,3,2,1,1,1)
  width.map.2 <- c(1,1,2,3,4,5)
  
  sink <- as.numeric(V(net)[V(net)$name=="SINK"])  
  paths<-get.all.shortest.paths(net, from=sink,weights=rep(1,ecount(net)))
  levels<-unlist(lapply(paths$res, length))[1:vcount(net)]
  level.list <- list(c(),c(),c())
  for (i in 1:length(levels)){
    for (idx in 1:max(levels)){
      if (levels[i]==idx){
        level.list[[idx]] <- c(level.list[[idx]],i)
        break
      }
    }
  }
  v.idx <- 1:vcount(net)
  for (v in v.idx){ 
    for (lvl in 1:length(level.list)){ 
      if (v %in% level.list[[lvl]]){
        E(net)[from(v)]$level.width.1 <- width.map.1[lvl]
        E(net)[from(v)]$level.width.2 <- width.map.2[lvl]
        break
      }
    }
  }
  
  level.width.1 <- E(net)$level.width.1
  level.width.2 <- E(net)$level.width.2
  
  ## Betweenness
  d <- igraph::edge.betweenness(net,weights=rep(1,igraph::ecount(net)))
  d <- (d - min(d)) / (max(d) - min(d))
  d <- d*4
  d <- ifelse(d < 1,1,d)
  between.width <- d
  
  
  to.return$values <- list(1,
                           level.width.1,
                           level.width.2,
                           between.width)
  to.return$explanation <- c("All=2",
                             "Hierarch-Based",
                             "Reverse Hierarchy-Based",
                             "Betweenness-Based")
  return(to.return)
  
}

#GOOD
get.edge.curved.options <- function(net){
  to.return <- vector("list",2)
  ## Hierarchy level
  curve.map.1 <- c(F,F,T)
  curve.map.2 <- c(T,T,F)
  
  sink <- as.numeric(V(net)[V(net)$name=="SINK"])  
  paths<-get.all.shortest.paths(net, from=sink,weights=rep(1,ecount(net)))
  levels<-unlist(lapply(paths$res, length))[1:vcount(net)]
  level.list <- list(c(),c(),c())
  for (i in 1:length(levels)){
    for (idx in 1:max(levels)){
      if (levels[i]==idx){
        level.list[[idx]] <- c(level.list[[idx]],i)
        break
      }
    }
  }
  v.idx <- 1:vcount(net)
  for (v in v.idx){ 
    for (lvl in 1:length(level.list)){ 
      if (v %in% level.list[[lvl]]){
        E(net)[from(v)]$level.curve.1 <- curve.map.1[lvl]
        E(net)[from(v)]$level.curve.2 <- curve.map.2[lvl]
        break
      }
    }
  }
  
  level.curve.1 <- E(net)$level.curve.1
  
  ## Betweenness
  d <- igraph::edge.betweenness(net,weights=rep(1,igraph::ecount(net)))
  d <- (d - min(d)) / (max(d) - min(d))
  d <- ifelse(d>mean(d),T,F)
  between.curve <- d
  
  
  to.return$values <- list(F,
                           level.curve.1,
                           between.curve)
  to.return$explanation <- c("All=F",
                             "Hierarch-Based",
                             "Betweenness-Based")
  return(to.return)
}

#GOOD
get.edge.lty.options <- function(net){
  to.return <- vector("list",2)
  ## Hierarchy level
  lty.map.1 <- c(1,1,2,3,4,5)
  lty.map.2 <- c(3,5,4,3,2,1)
  
  sink <- as.numeric(V(net)[V(net)$name=="SINK"])  
  paths<-get.all.shortest.paths(net, from=sink,weights=rep(1,ecount(net)))
  levels<-unlist(lapply(paths$res, length))[1:vcount(net)]
  level.list <- list(c(),c(),c())
  for (i in 1:length(levels)){
    for (idx in 1:max(levels)){
      if (levels[i]==idx){
        level.list[[idx]] <- c(level.list[[idx]],i)
        break
      }
    }
  }
  v.idx <- 1:vcount(net)
  for (v in v.idx){ 
    for (lvl in 1:length(level.list)){ 
      if (v %in% level.list[[lvl]]){
        E(net)[from(v)]$level.lty.1 <- lty.map.1[lvl]
        E(net)[from(v)]$level.lty.2 <- lty.map.2[lvl]
        break
      }
    }
  }
  
  level.lty.1 <- E(net)$level.lty.1
  level.lty.2 <- E(net)$level.lty.2
  
  ## Betweenness
  d <- igraph::edge.betweenness(net,weights=rep(1,igraph::ecount(net)))
  d <- (d - min(d)) / (max(d) - min(d))
  d <- ifelse(d>mean(d),1,2)
  between.lty <- d
  
  
  to.return$values <- list(1,
                           level.lty.1,
                           level.lty.2,
                           between.lty)
  to.return$explanation <- c("All=1",
                             "Hierarch-Based",
                             "Reverse Hierarchy-Based",
                             "Betweenness-Based")
  return(to.return)
  
}
