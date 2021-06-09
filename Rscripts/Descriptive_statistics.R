#Descriptive statistics of BLOOD dataset
library(HistDAWass)
library(tidyverse)

#data<-tt
 ROUND<-TRUE
 pos1<-2
 pos2<-2
#transform into a dataframe
Rows<-Cols<-bins<-character()
start_x<-end_x<-p<-numeric()
for(i in 1:get.MatH.nrows(data)){
  for (j in 1:get.MatH.ncols(data)){
    tmp_x<-data[i,j]@M[[1]]@x
    tmp_p<-diff(data[i,j]@M[[1]]@p)
    if(ROUND){
    tmp_x<-round(data[i,j]@M[[1]]@x,pos1)
    tmp_p<-round(diff(data[i,j]@M[[1]]@p),pos2)
    }
    start_x<-c(start_x,tmp_x[1:(length(tmp_x)-1)])
    end_x<-c(end_x,tmp_x[2:length(tmp_x)])
    p<-c(p,tmp_p)
    Rows<-c(Rows,rep(rownames(data@M)[i],length(tmp_p)))
    Cols<-c(Cols,rep(colnames(data@M)[j],length(tmp_p)))
    tmp_b<-paste0("[",tmp_x[1:(length(tmp_x)-1)],";",
                  tmp_x[2:length(tmp_x)],"]")
    bins<-c(bins,c(paste0("[",tmp_x[1:(length(tmp_x)-2)],";",
                          tmp_x[2:(length(tmp_x)-1)],")"),paste0("[",tmp_x[(length(tmp_x)-1)],";",
                                                                 tmp_x[(length(tmp_x))],"]")))
  }
}

DF<-data.frame(Rows,Cols,bins,p=as.character(round(p,3)),start_x,end_x)
## a data table made of data tables
DT<-DF %>% select(-c(start_x,end_x)) %>% group_by(Rows,Cols) %>% 
  nest() %>% 
  pivot_wider(names_from = Cols, values_from = data)
for(i in 1:nrow(DT)){
  MAX_b<-0
  for(j in 2:ncol(DT)){
    if(nrow(DT[i,j][[1]][[1]])>MAX_b) MAX_b<-nrow(DT[i,j][[1]][[1]])
  }
  for(j in 2:ncol(DT)){
    if(nrow(DT[i,j][[1]][[1]])<MAX_b){
      nr<-MAX_b-nrow(DT[i,j][[1]][[1]])
      #print(nr)
      DT[i,j][[1]][[1]]<-DT[i,j][[1]][[1]] %>% 
        add_row(bins=rep("",nr),p=rep("",nr))
    }
  }
}
 
#rownames(data@M)<-gsub("u","s",rownames(data@M))

# mean according to L2 for blood dataset
n=get.MatH.nrows(data)
v=get.MatH.ncols(data)
MEA=MatH(nrows = 2,ncols = v,varnames = get.MatH.varnames(data),
         rownames =c('M_L','M_W'))# c(TeX('$M_L$'), TeX('$M_W$')));
for (VV in 1:v){
  #genera dominio
  D=numeric(0)
  for (II in 1:n){
    D=c(D, data@M[II,VV][[1]]@x)
  }
  D=sort(unique(D))
  P=numeric(0)
  for (DD in 1:length(D)){
    els=numeric()
    for (II in 1:n){
      els=c(els,compP(data@M[II,VV][[1]],D[DD]))
    }
    P=c(P,mean(els))
  }
  
  tmp_distr=distributionH(D,P)
  MEA@M[1,VV][[1]]=tmp_distr
}
# mean according L2 Wasserstein
for (VV in 1:v){
  MEA@M[2,VV][[1]]=WH.vec.mean(data[,VV])
}
plot(MEA)

library(matrixStats)
#some other statistics
bb<-rbind(
colMeans(get.MatH.stats(data)$mat),
colSds(get.MatH.stats(data)$mat),
get.MatH.stats(MEA)$mat,
colMeans(get.MatH.stats(data,stat="std")$mat),
colSds(get.MatH.stats(data,stat="std")$mat),
get.MatH.stats(MEA,stat="std")$mat,
colMeans(get.MatH.stats(data,stat="skewness")$mat),
colSds(get.MatH.stats(data,stat="skewness")$mat),
get.MatH.stats(MEA,stat="skewness")$mat,
colMeans(get.MatH.stats(data,stat="kurtosis")$mat),
colSds(get.MatH.stats(data,stat="kurtosis")$mat),
get.MatH.stats(MEA,stat="kurtosis")$mat)
               