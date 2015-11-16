
# COMPLETE -> 9/13/15


add.data <- function(mn,
                     f,
                     lpv,
                     in.place=T,
                     sep='\t',
                     quote="\"",
                     header=T){
  d.len <- length(mn$data.list)
  index <- 1
  new.data.list <- mn$data.list
  new.lpv.map <- mn$lpv.map
  for (file in f){ 
    if (class(file)[1] == 'character'){
      data <- read.table(file,
                         sep=sep,
                         header=header,
                         quote=quote)
    }
    else{
      data <- file
    }
    new.data.list[[d.len+1]] <- dplyr::tbl_df(data)
    if (length(f) > 1){
      new.lpv.map[[d.len+1]]<-list(label.cols=lpv[[index]][[1]],
                                path.cols=lpv[[index]][[2]],
                                value.cols=lpv[[index]][[3]],
                                total.cols=ncol(data))
    }
    else{
      new.lpv.map[[d.len+1]] <- list(label.cols=lpv[[1]],
                                     path.cols=lpv[[2]],
                                     value.cols=lpv[[3]],
                                     total.cols=ncol(data))
    }
    d.len <- d.len + 1
    index <- index + 1
  }
  
  if (in.place){
    eval.parent(substitute(mn$data.list <- new.data.list))
    eval.parent(substitute(mn$lpv.map <- new.lpv.map))
  }
  else{
    mn$data.list <- new.data.list
    mn$lpv.map <- new.lpv.map
    return(mn)
  }
}

#load metanet from data file(s)
# note: has almost NO error/file handling right now


#' Load Annotated Data.
#' 
#' @param f A vector of file paths.
#' @param lpv A list of LPV mappings.
#' @param sep Any additional params
#' @param quote The qUOTE
#' @param header The header
#' @return A MetaNet object.
#' @examples
#' data(dataset1)
#' data(dataset2)
#' data(dataset3)
#' files <- list(dataset1,dataset2,dataset3)
#' lpv <- list(list(1,2:4,5:7),list(1,2:4,5:7),list(1,2:4,5:7))
#' mn <- load.data(f=files,lpv=lpv)
load.data <- function(f,
                      lpv,
                      sep='\t',
                      quote="\"",
                      header=T){
  
  if (class(lpv) != "list"){
    cat("ERROR: LPV must be of type list")
    return(NULL)
  }
  if (length(lpv) != length(f)){
    cat("ERROR: length(f) != length(lpv)")
  }
  
  lpv.map <- vector("list",length(f))
  data.list <- vector("list",length(f))
  
  index<-1
  for (file in f){
    # read.table(f,sep="\t",header=T,quote="\"")
    #data <- loader(file,...)
    if (class(file)[1] == 'character'){
      tryCatch(data <- read.table(file,
                                  sep=sep,
                                  header=header,
                                  quote=quote),
               warning = function(w) w,
               error = function(e) cat(" ERROR IN FILE READ.",'\n',
                                       "File:", file,
                                       "\n","MetaNetR uses 'read.table' for reference",'\n',
                                       "Add necessary params in load.data (sep, quote, etc)",
                                       '\n', 'Returned NULL')
      )
    }
    else{
      data <- file
    }
    #if (class(data) != "data.frame") return(NULL)
    data.list[[index]] <- dplyr::tbl_df(data)
    
    lpv.map[[index]]<-list(label.cols=lpv[[index]][[1]],
                          path.cols=lpv[[index]][[2]],
                          value.cols=lpv[[index]][[3]],
                          total.cols=ncol(data))
    
    index<-index+1 
  }
  #call metanet constructor
  mn <- metanet(data.list=data.list,
               lpv.map=lpv.map)
  
  return(mn)
}

######################################
