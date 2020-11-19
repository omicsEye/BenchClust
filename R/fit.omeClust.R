
########################################################
### Fitting omeClust algorithm (Discrete Silhouette) ###
########################################################

suppressPackageStartupMessages(library(data.table)) # fread function
suppressPackageStartupMessages(library(gtools)) # mixed sort function

#############################
### Initializing function ###
#############################

run.omeClust<-function(DF,Truth,omeClusters){
  
######################
### Gathering Data ###
######################
  
distDF<-dist(DF)

#####################################
### Extracting Estimated Clusters ###
#####################################

resF<-NULL
for(k in seq_along(omeClusters$members)){
  tmp<-omeClusters$members[k]
  tmp<-unlist(strsplit(as.character(tmp),";"))
  tmp<-data.frame(sampleID=tmp,cluster=rep(k,length(tmp)))
  resF<-data.frame(rbind(resF,tmp))
}
resF$sampleID<-factor(resF$sampleID,levels=gtools::mixedsort(resF$sampleID))
resF<-resF[order(resF$sampleID),]
pred.Clusters<-nrow(omeClusters)

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