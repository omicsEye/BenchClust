
#########################################
### Fitting RaceID3 (Partition-based) ###
#########################################

  if(!require("sincell")){
    BiocManager::install("sincell")
  }
suppressPackageStartupMessages(library(sincell))

#############################
### Initializing function ###
#############################

run.sincell<-function(rawData,Truth){
  
#########################
### Running Algorithm ###
#########################

  sinc <- sincell::sc_InitializingSincellObject(rawData)
  sinc <- sincell::sc_DimensionalityReductionObj(sinc, method="tSNE")
  sinc <- sincell::sc_clusterObj(sinc, clust.method="k-medoids") 
  clusters <- igraph::clusters(sinc[["cellsClustering"]])$membership
  pred.Clusters<-as.numeric(length(unique(clusters)))

#############################
### Measuring Performance ###
#############################

true_labels<-Truth$cluster
predicted_labels<-clusters
performance<-cluster_eval(true_labels=true_labels,
                          predicted_labels=predicted_labels)
performance<-data.frame(pred.Clusters=pred.Clusters,performance,check.names=F)
return(performance)
}  