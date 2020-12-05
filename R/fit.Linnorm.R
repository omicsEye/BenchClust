
#########################################
### Fitting RaceID3 (Partition-based) ###
#########################################

  if(!require("Linnorm")){
    BiocManager::install("Linnorm")
  }
suppressPackageStartupMessages(library(Linnorm))

#############################
### Initializing function ###
#############################

run.Linnorm<-function(rawData,Truth){
  
#########################
### Running Algorithm ###
#########################

  linn <- Linnorm::Linnorm.tSNE(rawData)
  clusters <- linn$k_means$cluster
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