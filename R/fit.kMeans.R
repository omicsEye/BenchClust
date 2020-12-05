
###############################################
### Fitting k-means algorithm (Partitional) ###
###############################################

if(!require("purrr")){
install.packages("purrr")
}
if(!require("stats")){
install.packages("stats")
}
suppressPackageStartupMessages(library(purrr)) # MAP function
suppressPackageStartupMessages(library(stats)) # k-means function

#############################
### Initializing function ###
#############################

set.seed(1234) 
run.kmeans<-function(DF,Truth){
  
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
res<-kmeans(DF,centers=k,nstart=25,iter.max=10,algorithm='Hartigan-Wong')
ss<-silhouette(res$cluster,distDF)
mean(ss[,3])}
avg_sil_values<-map_dbl(k.values,avg_sil)
bestK<-as.numeric(which(avg_sil_values==max(avg_sil_values)))+1

###################################
### Fitting Optimized Algorithm ###
###################################

resF<-kmeans(DF,centers=bestK,nstart=25,iter.max=10,algorithm='Hartigan-Wong')$cluster
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