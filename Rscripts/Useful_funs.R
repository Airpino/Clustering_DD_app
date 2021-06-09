library(HistDAWass)
library(tidyverse)

from_DFg_to_MH <- function(DFg, # a grouped data.frame or tibble
                           labs_rows, # the name of the variables that defines groups
                           typeH = "base", # this will be used when several histogram estimation methods will be available
                           br = 20) {
  require(tidyverse)
  rows <- length(DFg)
  name_rows <- character()
  name_cols <- DFg[[1]] %>%
    select(where(is.numeric)) %>%
    colnames()
  M <- HistDAWass::MatH(
    nrows = rows,
    ncols = length(name_cols),
    varnames = name_cols
  )
  for (i in 1:rows) {
    name_rows <- c(
      name_rows,
      as.character((DFg[[i]] %>% dplyr::select(labs_rows) %>%
                      unique() %>% pull()))[1]
    )
    for (j in 1:length(name_cols)) {
      h <- hist(DFg[[i]] %>% dplyr::select(name_cols[j]) %>% pull(), plot = FALSE)
      x <- h$breaks
      cdf <- cumsum(c(0, h$counts)) / sum(h$counts)
      tmp <- HistDAWass::distributionH(x = x, p = cdf)
      M@M[i, j][[1]] <- tmp
    }
  }
  row.names(M@M) <- name_rows
  return(M)
}


from_LONG_to_WIDE_MH <- function(DF, # a long format
                                 val_from, #column with numbers
                                 vars_from, #factor, where to take variable names
                                 rows_labs, #where to take row names
                                 typeH = "base", # this will be used when several histogram estimation methods will be available
                                 br = 10){
  var_names<-as.character(sort(unique(DF[,vars_from])))
  row_names<-as.character(unique(DF[,rows_labs]))
  MAT<-MatH(nrows=length(row_names),ncols = length(var_names),
            rownames=row_names,varnames = var_names)
  
  for (i in 1:length(row_names)) {
    for (j in 1:length(var_names)) {
      tmp<-DF %>% filter((.[[rows_labs]]==row_names[[i]])&
                           (.[[vars_from]]==var_names[[j]]))  %>% 
        select(all_of(val_from))
      
      x <- unname(quantile(tmp[,1],probs=c(0:br)/br))
      cdf <- c(0:br)/br
      if(typeH=="base"){
        h <- hist(tmp[,1], plot = FALSE)
        x <- h$breaks
        cdf <- cumsum(c(0, h$counts+1e-6))/max(cumsum(c(0, h$counts+1e-6)))
      }else{
        x <- unname(quantile(tmp[,1],probs=c(0:br)/br))
        cdf <- c(0:br)/br
      }
      
      MAT@M[i, j][[1]] <- HistDAWass::distributionH(x = x, p = cdf)
    }
  }
  
  
  return(MAT)
  
}



