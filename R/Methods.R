
run.vis <- function(mn,
                      network.index=1,
                      new.window=F,
                      single.plot=F){
  graphics.off()
  #open new graphics window for better plot viewing
  if (new.window) {
    tryCatch(x11(), 
             error = function(e) {cat('x11() Graphics Device Didnt Work...','\n', 'Trying windows()...','\n')
             tryCatch(windows(),
                              error=function(e) cat("windows() didnt work",'\n',"Continuing in Default Device",'\n'),
                              finally=new.window<-F)}
             )
    cat("Opening New Window ..", "\n")
  }
                           
  my.network <- mn$combined.networks[[network.index]] #get (combined) network to explore
  network.count <- length(mn$networks[[network.index]]) #get constituent network count for color, etc
  param.df <- get.param.df() # contains all parameters ("parameter") and their categories ("category")
  menu.set <- c("Layout", 
                "Vertex Properties", 
                "Edge Properties")
  explored.set <- c()
  default.param.list <- list(vertex.label=NA,vertex.color="skyblue",
                             vertex.size=2,edge.size=1,
                             edge.color="grey",plot.layout=layout.fruchterman.reingold.grid(my.network),
                             edge.label=NA,vertex.shape="circle",
                             edge.width=1, vertex.label.font=2,
                             vertex.label.cex=0.6,vertex.label.color="black",
                             vertex.label.dist=0.2,
                             edge.curved=F,
                             edge.lty=1)
  saved.param.list<-list(default.param.list) #list of a list of params -> length(saved.param.list) = num. plots the user currently likes
  
  while(TRUE){  
    user.input <- menu.category.input(menu.set,explored.set)
    if (user.input == 0) break
    chosen.param.category <- menu.set[user.input]  # get general param category from user input (readlines)    
    
    tmp.param.df <- param.df[param.df$category==chosen.param.category,] # get subset of param.df for chosen.param.category
    #cat("You Choose : " , chosen.param.category,'\n')
    for (param.to.explore in tmp.param.df$parameter){
      cat('Loading Param: ', param.to.explore, '\n')
      if (param.to.explore == "plot.layout" && vcount(my.network)) cat("Layout May Take A Minute..",'\n')
      param.list.return <- get.param.options(param.to.explore,my.network) # get param values/explanation list
      param.options <- param.list.return$values #list of possible param values
      param.explanation <- param.list.return$explanation #vector of corresponding param explanations
      
      num.plots <- length(param.options)*length(saved.param.list) # num.plots needed = param_options*past_plots_preferred
      cat("---", num.plots, "Plots To Create---",'\n')
      
      tmp.plot.list <- vector("list",num.plots) #stores param options      
      user.break <- FALSE # used if user wants to skip ahead
      
      if (!single.plot) par(mfrow=c(ceiling(sqrt(num.plots)),max(ceiling(num.plots/ceiling(sqrt(num.plots))),2))) # set graphical param
      else {par(mfrow=c(1,1)); user.response <- c()}     
      
      par(mar=c(0,0,0,0)) # set margin      
      idx<-1      
      # PLOTTING
      for (preferred.plot in saved.param.list){
        last.opt <- NA
        for (p.opt in param.options){
          
          tmp.plot <- preferred.plot
          tmp.plot[[param.to.explore]] <- p.opt # set tmp param val in e.list 
                    
          igraph.options(tmp.plot) # pass new list (w/ tmp val) to change options            
          plot(my.network)
          
          tmp.plot.list[[idx]] <- tmp.plot
          
          if (single.plot){
            cat("Like This Plot? 'y','n', or '0' for next category",'\n')
            if (idx %% length(param.explanation) != 0){
              answer <- readline(paste(param.explanation[idx %% length(param.explanation)], ': ',sep=""))
            }
            else answer <- readline(paste(param.explanation[length(param.explanation)],': ',sep=""))
            if (answer=="y") user.response <- c(user.response,idx)
            if (answer=="0") {user.break <- TRUE; break}            
          }
          idx<-idx+1
        }
        if (user.break) break
      }      
      # CHOOSING
      if (!single.plot){ 
        cat (" Type 'y' to see explanations \n",
             "OR Type indices of preferred plots (e.g. '1 3 7')")
        response <- readline("> ")
        if (response == "y" || response == "Y"){
          for (i in  1:length(param.explanation)) {
            indices <- seq(i,num.plots,by=length(param.explanation))
            cat(cat(indices,sep=","),':', param.explanation[i],'\n')
          }
          response <- readline("Preferred Plots: ")
        }
        pref.plots <- tmp.plot.list[as.integer(unlist(strsplit(response,split=" ")))]
      }
      else{
        if (length(user.response) == 0) user.response <- c(1)
        pref.plots <- tmp.plot.list[user.response]
      }
      saved.param.list <- pref.plots
    }
    
    # NEXT CATEGORY OR EXIT
    explored.set <- c(explored.set,chosen.param.category)
    cat("--Category Complete--", "\n", "Type 'y' to pick another category",
        '\n',"Type 'n' to exit & save preferred plots.")
    continue <- readline('> ')
    if (continue == 'n'){
      #save plot? 
      idx.save <- 1
      par(mfrow=c(1,1))
      par(mar=c(0,0,0,0))
      for (p in saved.param.list){
        igraph.options(p)
        str <- paste('metanetr_plot_',
                     idx.save,'.pdf',sep="")
        pdf(str)
        plot(my.network)
        dev.off()
        idx.save<-idx.save+1
      }
      break
    }
    
  } 
  graphics.off()
}


# IMPORTANT: expects RELATIVE lpv arguments .. hmm
#creates networks
# note: 'label' argument doesn't function right now
# note: 'add' argument doesn't function right now

#'  Map Networks from an MN Object
#' 
#' @param mn A MetaNet Object.
#' @param datasets A Vector
#' @param lpv A list - LPV Mapc
#' @param multigraph A Boolean
#' @param standard A Boolean
#' @param in.place A Boolean
#' @param add A Vector
#' @return An MN Object if in.place=F, else Nothing - eval in place.
#' @examples
#' data(dataset1)
#' data(dataset2)
#' data(dataset3)
#' files <- list(dataset1,dataset2,dataset3)
#' lpv <- list(list(1,2:4,5:7),list(1,2:4,5:7),list(1,2:4,5:7))
#' mn <- load.data(f=files,lpv=lpv)
#' map.network(mn,datasets=c(1,2,3),lpv=list(list(1,1:2,1:2),list(1,1:2,1),list(1,1:2,2:3)))
map.network <- function(mn,
                        datasets=c(),
                        lpv=list(),
                        multigraph=F,
                        standard=F,
                        in.place=T,
                        add=NA){
  
  if (standard) datasets <- 1:length(mn$data.list)
  abs.labels <- vector("list",length(datasets))
  abs.paths <- vector("list",length(datasets))
  abs.values <- vector("list",length(datasets))
  #check if lpv is a nested list or just one list
  double.list.lpv <- TRUE
  if(length(datasets)==1) double.list.lpv<-FALSE
  
  idx1<-1
  for (ds.index in datasets){
    #get relative lpv values
    if (standard){
      rel.labels <- 1:length(mn$lpv.map[[ds.index]]$label.cols)
      rel.paths <- 1:length(mn$lpv.map[[ds.index]]$path.cols)
      rel.values <- 1:length(mn$lpv.map[[ds.index]]$value.cols)
    }
    #user does NOT want standard -> gives LPV mapping
    else{
      if (length(lpv) == 0){
        rel.labels <- 1:length(mn$lpv.map[[ds.index]]$label.cols)
        rel.paths <- 1:length(mn$lpv.map[[ds.index]]$path.cols)
        rel.values <- 1:length(mn$lpv.map[[ds.index]]$value.cols)
      }
      else{
        if (double.list.lpv){
          rel.labels <- lpv[[idx1]][[1]]
          rel.paths <- lpv[[idx1]][[2]]
          rel.values <- lpv[[idx1]][[3]]
        }
        else{
          rel.labels <- lpv[[1]]
          rel.paths <- lpv[[2]]
          rel.values <- lpv[[3]]
        }
      }
    }
    
    #map relative values to absolute
    # NEED ERROR HANDLING FOR "NA"
    abs.labels[[idx1]] <- mn$lpv.map[[ds.index]]$label.cols[rel.labels]
    abs.paths[[idx1]] <- mn$lpv.map[[ds.index]]$path.cols[rel.paths]
    abs.values[[idx1]] <- mn$lpv.map[[ds.index]]$value.cols[rel.values]
    
    idx1<-idx1+1
  }
  net.count <- sum(sapply(abs.values,length))
  net.list <- vector("list",net.count)
  # good up to here
  #loop through datasets, create networks for each one (#nets=length(abs.values[[ds.index]])),
  #add flat (not nested) to net.list
  net.counter<-1
  for (ds.index in datasets){
    
    num.paths <- length(abs.paths[[ds.index]])
    v.idx <- max(length(abs.values[[ds.index]]),1)
    
    for (net.idx in 1:v.idx){      
      # Less than 2 paths -> can't map a network (as of now)
      if (num.paths < 2){
        cat("Error: Must Include At Least 2 Paths \n")
        return(NULL)
      } 
      # Exactly 2 paths
      else if (num.paths == 2){
        #no legit value column is given (i.e. '0' is given) -> grab frequency counts of paths instead
        #map.network(mn,datasets=c(1,2),lpv=list(list(1,1:2,0),list(1,1:2,0)))
        # ^ above would go through this twice, making 2 total networks, both w/ freq as only edge attribute
        if (length(abs.values[[ds.index]])==0){
          get.path.col <- abs.paths[[ds.index]]
          df <- mn$data.list[[ds.index]][,get.path.col]
          
          net <- graph.data.frame(df, directed=F)
          if (!multigraph){
            E(net)$weight <- count.multiple(net)
            net <- simplify(net)
          }
          net.list[[net.counter]] <- net
        }
        #legit value column is given -> just grab and process
        #map.network(mn,datasets=c(1,2),lpv=list(list(1,1:2,1),list(1,1:2,1:2))
        # ^ above would go through this 3 times (1 time, then 2 times)
        else{         
          get.path.col <- abs.paths[[ds.index]]
          get.val.col <- abs.values[[ds.index]][net.idx]
          
          sink.df <- add.sink(mn$data.list[[ds.index]][,get.path.col[1]])  #create sink df  
          if (!multigraph) sink.df <- reduce.df(sink.df)
          
          df <- mn$data.list[[ds.index]][,c(get.path.col,get.val.col)]                        
          names(df)[length(names(df))] <- "weight" 
          net <- graph.data.frame(sink.df,directed=F) + graph.data.frame(df,directed=F)
          
          if (!multigraph){
            E(net)$weight <- count.multiple(net)
            net <- simplify(net)
          }
          
          net.list[[net.counter]] <- net      
        }
      }
      #good up to here
      # More than 2 paths
      else {        
        # no legit value given -> use frequency again
        # currently no edge values used
        if (length(abs.values[[ds.index]])==0){
          df.list <- vector("list",(length(abs.paths[[ds.index]])-1))
          #orig. network, then just add to it
          orig.df <- add.sink(mn$data.list[[ds.index]][,c(abs.paths[[ds.index]][1])])
          net <- graph.data.frame(orig.df, directed=F)
          
          #adding networks together
          for (i in 1:length(df.list)){
            get.path.col <- abs.paths[[ds.index]][c(i,i+1)]
            df <- mn$data.list[[ds.index]][,get.path.col]
            net <- net + graph.data.frame(df,directed=F)
          }
          if (!multigraph){
            E(net)$weight <- count.multiple(net)
            net <- simplify(net)
          }
          net.list[[net.counter]] <- net
        }
        # legit values given -> just grab and process
        # currently no edge values used
        else{
          df.list <- vector("list",(length(abs.paths[[ds.index]])-1))
          #orig. network, then just add to it
          sink.df <- add.sink(mn$data.list[[ds.index]][,c(abs.paths[[ds.index]][1])])
          #sink.df <- reduce.df(sink.df)
          net <- graph.data.frame(sink.df,directed=F)
          #adding networks together
          for (i in 1:length(df.list)){
            get.path.col <- abs.paths[[ds.index]][c(i,i+1)]
            df <- reduce.df(mn$data.list[[ds.index]][,get.path.col])
            net <- net + graph.data.frame(df,directed=F)
          }       
          if (!multigraph){
            E(net)$weight <- count.multiple(net)
            net <- simplify(net)
          }
          net.list[[net.counter]] <- net          
        }
      }
      #end of inner for-loop, but still within
      net.counter <- net.counter+1
    }
  }
 
  new.net <- mn$networks
  new.idx <- length(new.net)+1
  new.net[[new.idx]] <- net.list
  
  comb.net.list<-combine.networks(mn=mn,
                   n.list=net.list,
                   idx=new.idx) #combine networks in net.list and add to mn$combined.networks
  
  if (in.place){
    eval.parent(substitute(mn$networks<-new.net))
    eval.parent(substitute(mn$combined.networks<-comb.net.list))
  }
  else{
    mn$networks[[length(mn$networks)+1]] <- net.list
    mn$combined.networks <- comb.net.list
    return(mn)
  }
}

# creates the net for mn$combined.networks[[idx]]
# ALL HIDDEN FROM THE USER - NO NEED FOR DOCUMENTATION
combine.networks <- function(mn,
                             n.list,
                             idx){
  exist.comb.net <- mn$combined.networks
  comb.net  <- n.list[[1]]
  if(length(n.list)>1){
    for (i in 2:length(n.list)){
      comb.net <- comb.net + n.list[[i]]
    }
  }
  total_weight <- rep(0,ecount(comb.net))
  for (n in n.list){
    tmp <- E(n)$weight
    length(tmp) <- ecount(comb.net)
    tmp[is.na(tmp)] <- 0
    total_weight <- total_weight + tmp
  }
  
  E(comb.net)$weight <- total_weight
  
  exist.comb.net[[idx]]<-comb.net
  
  
  return(exist.comb.net)
}

  


