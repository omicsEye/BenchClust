
#########################################
### Fitting RaceID3 (Partition-based) ###
#########################################

  if(!require("TSCAN")){
    BiocManager::install("TSCAN")
  }
suppressPackageStartupMessages(library(TSCAN))

#############################
### Initializing function ###
#############################

run.TSCAN<-function(rawData,Truth){
  
#########################
### Running Algorithm ###
#########################

  data <- TSCAN::preprocess(rawData, takelog = TRUE, logbase = 2, cvcutoff = 0.1)
  tsc <- TSCAN::exprmclust(data, reduce = T)
  clusters <- as.vector(tsc$clusterid)
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