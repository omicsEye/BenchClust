
######################################
### Fitting Dbscan (Density-Based) ###
######################################

if(!require("cidr")){
  devtools::install_github("VCCRI/CIDR")
  }
suppressPackageStartupMessages(library(cidr)) # CIDR function

#############################
### Initializing function ###
#############################

run.CIDR<-function(rawData,Truth){
  
#########################
### Running Algorithm ###
#########################

cid <- cidr::scDataConstructor(rawData,tagType = "raw") 
cid <- cidr::determineDropoutCandidates(cid)
cid <- cidr::wThreshold(cid)
cid <- cidr::scDissim(cid, threads = 1)
cid <- cidr::scPCA(cid, plotPC=FALSE)
cid <- cidr::scCluster(object = cid, nCluster = NULL, nPC = 4) 
clusters <- as.numeric(cid@clusters)
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