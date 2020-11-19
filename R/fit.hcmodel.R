
###################################################
### Fitting Gaussian Multivariate (Model-Based) ###
###################################################

if(!require("purrr")){
install.packages("purrr")
}
if(!require("mclust")){
install.packages("mclust")
}
suppressPackageStartupMessages(library(purrr)) # MAP function
suppressPackageStartupMessages(library(mclust)) # hcmodel function

#############################
### Initializing function ###
#############################

run.hcmodel<-function(DF,Truth){
  
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
res<-hclass(hc(DF,modelName='VVV',use='VARS'),k)
ss<-silhouette(res,distDF)
mean(ss[,3])}
avg_sil_values<-map_dbl(k.values,avg_sil)
bestK<-as.numeric(which(avg_sil_values==max(avg_sil_values)))+1

###################################
### Fitting Optimized Algorithm ###
###################################

resF<-hclass(hc(DF,modelName='VVV',use='VARS'),bestK)
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

