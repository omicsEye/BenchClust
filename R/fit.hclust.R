
################################################
### Fitting Hierarchical algorithm (Linkage) ###
################################################

if(!require("purrr")){
install.packages("purrr")
}
if(!require("cluster")){
install.packages("cluster")
}
suppressPackageStartupMessages(library(purrr)) # MAP function
suppressPackageStartupMessages(library(cluster)) # Hierarchical (agnes) function

#############################
### Initializing function ###
#############################

set.seed(1234)
run.hclust<-function(DF,Truth){
  
######################
### Gathering Data ###
######################
  
DF<-scale(DF)
distDF<-dist(DF)

###################################
### Estimating Optimal Clusters ###
###################################

k.values<-2:15
avg_sil<-function(k){
res<-cutree(agnes(DF,metric='euclidean',stand=F,method='average'),k=k)
ss<-silhouette(res,distDF)
mean(ss[,3])}
avg_sil_values<-map_dbl(k.values,avg_sil)
bestK<-as.numeric(which(avg_sil_values==max(avg_sil_values)))+1

###################################
### Fitting Optimized Algorithm ###
###################################

resF<-cutree(agnes(DF,metric='euclidean',stand=F,method='average'),k=bestK)
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