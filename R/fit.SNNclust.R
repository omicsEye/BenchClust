
##################################################
### Fitting SNNclust (Shared Nearest Neighbor) ###
##################################################

if(!require("purrr")){
install.packages("purrr")
}
if(!require("dbscan")){
install.packages("dbscan")
}
suppressPackageStartupMessages(library(purrr)) # MAP function
suppressPackageStartupMessages(library(dbscan)) # SNNclust function inside Dbscan library

#############################
### Initializing function ###
#############################

run.SNNclust<-function(DF,Truth){
  
######################
### Gathering Data ###
######################
  
DF<-scale(DF)
distDF<-dist(DF)

#####################################
### Checking Optimal value of eps ###
#####################################

#dbscan::kNNdistplot(DF,k=5) # Elbow at 12<eps<15 for most data hence chose eps=14

#########################
### Fitting Algorithm ###
#########################

resF<-sNNclust(DF,k=20,eps=14,minPts=5)$cluster
resF<-data.frame(Sample=Truth$sampleID,cluster=as.numeric(resF))
pred.Clusters<-length(unique(table(resF$cluster)))
             
#############################
### Measuring Performance ###
#############################

true_labels<-Truth$cluster
predicted_labels<-resF$cluster
performance<-cluster_eval(true_labels=true_labels,
                          predicted_labels=predicted_labels,
                          distDF=distDF)
performance<-data.frame(pred.Clusters=pred.Clusters,performance,check.names=F)
return(performance)
}  