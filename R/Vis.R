
# COMPLETE -> 9/13/15

# CONTAINS VISUALIZATION ENGINE PARAMETERS

# PARAMETER CATEGORY MENU
# TAKES IN USER INPUT
# RETURNS THE CATEGORY TO BE EXPLORED
menu.category.input <- function(menu.set,
                                explored.set)
{
  cat("--Visualization Options-- \n\n")
  cat('0 : Exit', '\n')
  idx<-1
  for (m in menu.set){
    if (m %in% explored.set) cat(idx, ":", m, '(ALREADY EXPLORED)', '\n')
    else cat(idx, ':', m, '\n')
    idx<-idx+1
  }
  response <- readline("Choose a Category: ")
  return(as.integer(response))
}

# VERY IMPORTANT FOR ENGINE (HIDDEN FROM USER)
# THIS IS WHERE YOU ADD ANY PARAMS OR PARAM CATEGORIES
get.param.df <- function(){
  
  
  # ADD SPECIFIC PARAMETERS TO EXPLORE WITHIN EACH CATEGORY
  # MUST WORK WITH "igraph.options(...)"
  layout.p <- c("plot.layout")
  vertex.p <- c("vertex.size","vertex.color","vertex.label")
  edge.p <- c("edge.color","edge.width","edge.curved", "edge.lty")
  
  # ADD GENERAL GRAPHICAL PARAMETER CATEGORIES HERE
  menu.set <- c("Layout", 
                "Vertex Properties", 
                "Edge Properties")
  
  # DF THAT COMBINES THE CATEGORIES AND THEIR RESPECTIVE PARAMS
  param.df <- data.frame(parameter=c(layout.p,
                                     vertex.p,
                                     edge.p
                                    ),
                        category=as.factor(c(rep("Layout",length(layout.p)),
                                             rep("Vertex Properties",length(vertex.p)),
                                             rep("Edge Properties",length(edge.p))
                                            )
                                           )
                       )
  return(param.df)  
}


# TAKES IN A PARAMETER NAME (ALL HIDDEN FROM USER)
# RETURNS THE VALUES TO BE EXPLORED FOR THAT PARAM & GRAPH
# THESE FUNCTIONS (I.E. get.plot.layout.options) EXIST IN "OPTIONS" SCRIPTS (i.e. GeneralOptions.R, etc)
get.param.options <- function(param.name,
                              my.network){
  
  to.return <- switch(param.name,
                      "plot.layout"=get.plot.layout.options(my.network),
                      "vertex.size"=get.vertex.size.options(my.network),
                      "vertex.color"=get.vertex.color.options(my.network),
                      "vertex.shape"=get.vertex.shape.options(my.network),
                      "vertex.label"=get.vertex.label.options(my.network),
                      "edge.color"=get.edge.color.options(my.network),
                      "edge.width"=get.edge.width.options(my.network),                      
                      "edge.curved"=get.edge.curved.options(my.network),
                      "edge.lty"=get.edge.lty.options(my.network)
  )
  
  return(to.return) #returns list ($values and $explanation)
}









