
# NOT COMPLETE -> 9/13/15

# DECISION - NORMALIZE "SINK" BUT LEAVE THE REST BE


# GOOD
get.vertex.size.options <- function(net){
  to.return <- vector("list",2)  
  ## LEVEL SIZE   
  sink <- as.numeric(V(net)[V(net)$name=="SINK"])
  paths<-get.all.shortest.paths(net, from=sink,weights=rep(1,ecount(net)))  
  levels<-unlist(lapply(paths$res, length))[1:vcount(net)]
  level.size <- 10*abs(((levels - min(levels))/(max(levels) - min(levels)))-1)
  level.size <- ifelse(level.size<2,2,level.size)
  ## DEGREE SIZE
  d <- degree(net) 
  idx<-which.max(d)
  d[which.max(d)] <- 0  
  d <- (d - min(d)) / (max(d) - min(d))
  d['SINK'] <- 0.5
  degree.size <- d * 10 #normalize to max of 10  
  degree.size[idx]<-10
  degree.size[degree.size < 2] <- 2
  
  to.return$values <- list(1,
                           4,
                           level.size,
                           degree.size)
  to.return$explanation <- c("All=1",
                             "All=4",
                             "Hierarchy-Based",
                             "Degree-Based")
  return(to.return)
}

# GOOD
get.vertex.color.options <- function(net){
  to.return <- vector("list",2)
  ## LEVEL COLOR
  sink <- as.numeric(V(net)[V(net)$name=="SINK"])
  paths<-get.all.shortest.paths(net, from=sink,weights=rep(1,ecount(net)))  
  levels<-unlist(lapply(paths$res, length))
  pal <- RColorBrewer::brewer.pal(max(levels),"RdBu") 
  pal <- c("darkgreen","blue","red")
  level.color <- pal[levels]
  
  ## DEGREE COLOR
  d <- degree(net)
  d["SINK"] <- 1
  d <- (d - min(d)) / (max(d) - min(d)) # between 0 and 1
  d <- as.numeric(factor(d))
  d <- ifelse(d >= 11,11,d)
  pal <- rev(RColorBrewer::brewer.pal(11,"RdYlBu"))
  degree.color <- pal[d]
  
  to.return$values <- list('blue',
                           level.color,
                           degree.color)
  to.return$explanation <- c("All Blue",
                             "Hierarchy-Based",
                             "Degree-Based")
  return(to.return)
}

# GOOD
get.vertex.shape.options <- function(net){
  to.return <- vector("list",2)
  ## LEVEL SHAPE
  sink <- as.numeric(V(net)[V(net)$name=="SINK"])
  paths<-get.all.shortest.paths(net, from=sink,weights=rep(1,ecount(net)))  
  levels<-unlist(lapply(paths$res, length))
  core.shapes <- c("square","circle")
  s.levels <- rep(core.shapes,max(levels))
  level.shape <- s.levels[levels]
  
  ## DEGREE SHAPE
  d <- degree(net)
  d["SINK"] <- 1
  d[which.max(d)] <- 1
  d <- (d - min(d)) / (max(d) - min(d))
  degree.shape <- vector(length=vcount(net))
  degree.shape <- ifelse(d>0.5,"circle","square")
  
  
  to.return$values <- list('circle',
                           level.shape,
                           degree.shape)
  to.return$explanation <- c("All Circle",
                             "Hierarchy-Based",
                             "Degree-Based")
  return(to.return)
}

# GOOD
get.vertex.label.options <- function(net){
  to.return <- vector("list",2)
  ## LEVEL LABEL
  sink <- as.numeric(V(net)[V(net)$name=="SINK"])
  paths<-get.all.shortest.paths(net, from=sink,weights=rep(1,ecount(net)))  
  levels<-unlist(lapply(paths$res, length))
  level.label <- ifelse(levels<=2,V(net)$name,NA)
  
  ## DEGREE LABEL
  d <- degree(net)
  d["SINK"] <- 1
  d[which.max(d)] <- 1
  d <- (d - min(d)) / (max(d) - min(d))
  degree.label <- ifelse(d>mean(d),V(net)$name,NA)
  
  to.return$values <- list(NA,
                           level.label,
                           degree.label)
  to.return$explanation <- c("All NA",
                             "Hierarchy-Based",
                             "Degree-Based")
  return(to.return)
}