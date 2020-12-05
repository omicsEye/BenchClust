
#########################################
### Fitting RaceID3 (Partition-based) ###
#########################################

if(!require("pcaReduce")){
  if(!require("pcaReduce")){
  BiocManager::install("pcaMethods")
  }
  install.packages("/gpfs/gsfs11/users/chatterjees7/omeClust/SingleCell_analysis/Rpackages/pcaReduce_1.0.tar.gz",repos=NULL,type="source")
}
suppressPackageStartupMessages(library(pcaReduce)) # pcaReduce function

#############################
### Initializing function ###
#############################

run.pcaReduce<-function(rawData,Truth){
  
#########################
### Running Algorithm ###
#########################

pcar <- pcaReduce::PCAreduce(t(rawData),nbt=1,q=30,method='M')
clusters <- as.vector(pcar)[[1]][,1]
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