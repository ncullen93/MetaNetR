

add.sink <- function(df){
  num.rows <- nrow(df)
  df$sink <- rep("SINK",num.rows)
  df <- df %>% dplyr::select(2,1)
  return(df)
}


reduce.df <- function(my_tbl){
  x<-names(my_tbl)
  my_tbl <- my_tbl %>% 
    dplyr::group_by_(as.name(x[1]),as.name(x[2])) %>%
    dplyr::summarise(freq=n())
  return(my_tbl[,c(1,2)])
}

prep.freq.data <- function(my_tbl){
  if (ncol(my_tbl) != 2){
    cat("Error in 'prep.freq.data' -> ncol(my_tbl) > 2 \n")
    cat("Returned NULL")
    return(NULL)
  }
  x<-names(my_tbl)
  my_tbl <- my_tbl %>% 
    dplyr::group_by_(as.name(x[1]),as.name(x[2])) %>%
    dplyr::summarise(freq=n())
  return(my_tbl)
}

prep.raw.data <- function(my_tbl){
  x<-names(my_tbl)
  val <- x[3]
  my_tbl<- my_tbl %>%
    dplyr::filter_(as.name(x[3])>0) %>%
    dplyr::group_by_(as.name(x[1]),as.name(x[2])) %>%
    dplyr::summarise_(sum_val = interp(~sum(var, na.rm = TRUE), var = as.name(val)))
  #cat(nrow(my_tbl), '\n')
  return(my_tbl)
}

network.count <- function(mn){
  if (length(mn$networks) > 0){
    return(paste(sum(sapply(mn$networks, length)), " (",
                 length(mn$networks), " sets)", sep=""))
  }
  else{
    return("0")
  }
}

dataset.count <- function(mn) return (length(mn$data.list))


get.LPV <- function(mn,index){
  cat("Total Cols: ", mn$lpv.map[[index]]$total.cols, "\n")
  cat("Labels: ", mn$lpv.map[[index]]$label.cols, "\n")
  cat("Paths: ", mn$lpv.map[[index]]$path.cols, "\n")
  cat("Values: ", mn$lpv.map[[index]]$value.cols, "\n")
}

#########################################

# ANY FUNCTION BELOW HERE ARE FOR THE USER



view.net <- function(mn,
                     set=NA){
  if (is.na(set)){
    break
  }
  else{
    cat("-- Network Set", set, "-- \n\n")
    for (n in mn$networks[[set]])
    {
      print(n)
      cat("\n")
    }
  }
}

#' Get a Data Set from a MN Object
#' 
#' @param mn A MetaNet Object.
#' @param index An Integer
#' @return A Data Frame
#' @examples
#' data(dataset1)
#' data(dataset2)
#' data(dataset3)
#' files <- list(dataset1,dataset2,dataset3)
#' lpv <- list(list(1,2:4,5:7),list(1,2:4,5:7),list(1,2:4,5:7))
#' mn <- load.data(f=files,lpv=lpv)
#' map.network(mn,datasets=c(1,2,3),lpv=list(list(1,1:2,1:2),list(1,1:2,1),list(1,1:2,2:3)))
#' set.2 <- get.data(mn,2)
get.data <- function(mn,index) return(mn$data.list[[index]])


#' Set new LPV mapping for an MN Object's Data Set
#' 
#' @param mn A MetaNet Object.
#' @param index A Number
#' @param label.cols A Vector
#' @param path.cols A Vector
#' @param value.cols A Vector 
#' @return Nothing - Evaluated in Place
#' @examples
#' data(dataset1)
#' data(dataset2)
#' data(dataset3)
#' files <- list(dataset1,dataset2,dataset3)
#' lpv <- list(list(1,2:4,5:7),list(1,2:4,5:7),list(1,2:4,5:7))
#' mn <- load.data(f=files,lpv=lpv)
#' map.network(mn,datasets=c(1,2,3),lpv=list(list(1,1:2,1:2),list(1,1:2,1),list(1,1:2,2:3)))
#' set.LPV(mn,index=2,label.cols=1,path.cols=3:5,value.cols=6:7)
set.LPV <-function(mn,
                  index,
                  label.cols=NA,
                  path.cols=NA,
                  value.cols=NA){
  
  new.lpv.map<-list(label.cols=label.cols,
                path.cols=path.cols,
                value.cols=value.cols)
  
  eval.parent(substitute(mn$lpv.map[[index]]<-new.lpv.map))
}

#' Set MetaData for an MN Object's Data Set
#' 
#' @param mn A MetaNet Object.
#' @param pass.in A string
#' @param value A string
#' @return Nothing - Evaluated in Place
#' @examples
#' data(dataset1)
#' data(dataset2)
#' data(dataset3)
#' files <- list(dataset1,dataset2,dataset3)
#' lpv <- list(list(1,2:4,5:7),list(1,2:4,5:7),list(1,2:4,5:7))
#' mn <- load.data(f=files,lpv=lpv)
#' map.network(mn,datasets=c(1,2,3),lpv=list(list(1,1:2,1:2),list(1,1:2,1),list(1,1:2,2:3)))
#' set.metadata(mn,'location', 'bahamas')
set.metadata <-function(mn,
                       pass.in,
                       value){
  mnd<-mn$metadata
  mnd[[pass.in]] <- value
  eval.parent(substitute(mn$metadata<-mnd))
  rm(mnd)
}

#' Erase all metadata for an MN Object
#' 
#' @param mn A MetaNet Object.
#' @return Nothing - Evaluated in Place
#' @examples
#' data(dataset1)
#' data(dataset2)
#' data(dataset3)
#' files <- list(dataset1,dataset2,dataset3)
#' lpv <- list(list(1,2:4,5:7),list(1,2:4,5:7),list(1,2:4,5:7))
#' mn <- load.data(f=files,lpv=lpv)
#' map.network(mn,datasets=c(1,2,3),lpv=list(list(1,1:2,1:2),list(1,1:2,1),list(1,1:2,2:3)))
#' clear.metadata(mn)
clear.metadata <- function(mn) eval.parent(substitute(mn$metadata<-list()))

#' Erase all Networks for an MN Object
#' 
#' @param mn A MetaNet Object.
#' @return Nothing - Evaluated in Place
#' @examples
#' data(dataset1)
#' data(dataset2)
#' data(dataset3)
#' files <- list(dataset1,dataset2,dataset3)
#' lpv <- list(list(1,2:4,5:7),list(1,2:4,5:7),list(1,2:4,5:7))
#' mn <- load.data(f=files,lpv=lpv)
#' map.network(mn,datasets=c(1,2,3),lpv=list(list(1,1:2,1:2),list(1,1:2,1),list(1,1:2,2:3)))
#' clear.networks(mn)
clear.networks <- function(mn) {
  eval.parent(substitute(mn$combined.networks<-list()))
  eval.parent(substitute(mn$networks<-list()))
}

#' Erase all Data for an MN Object
#' 
#' @param mn A MetaNet Object.
#' @return Nothing - Evaluated in Place
#' @examples
#' data(dataset1)
#' data(dataset2)
#' data(dataset3)
#' files <- list(dataset1,dataset2,dataset3)
#' lpv <- list(list(1,2:4,5:7),list(1,2:4,5:7),list(1,2:4,5:7))
#' mn <- load.data(f=files,lpv=lpv)
#' map.network(mn,datasets=c(1,2,3),lpv=list(list(1,1:2,1:2),list(1,1:2,1),list(1,1:2,2:3)))
#' clear.data(mn)
clear.data <- function(mn) eval.parent(substitute(mn$data.list <- list()))

print.metanet <- function(x,...){
  cat("--Meta'omic Network-- \n")
  cat("Mapped Networks: ", network.count(x),'\n')
  cat("Data Sets: ", dataset.count(x),'\n')
  
  if (length(x$metadata)==0){
    cat("No Metadata Available\n")
  }
  else{
    cat("-Metadata-\n")
    for (i in 1:length(x$metadata)){
      if (!names(x$metadata)[i] %in% c("sample.count")){
        if (!is.na(x$metadata[[i]])){
          cat(names(x$meta)[i], ': ' , x$metadata[[i]], '\n')
        }
      }
    }
  }
}


