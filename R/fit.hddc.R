
###################################################
### Fitting Subspace algorithm (Subspace-based) ###
###################################################

if(!require("purrr")){
install.packages("purrr")
}
if(!require("HDclassif")){
install.packages("HDclassif")
}
suppressPackageStartupMessages(library(purrr)) # MAP function
suppressPackageStartupMessages(library(HDclassif)) # Subspace clustering function

#############################
### Initializing function ###
#############################

set.seed(1234)
run.hddc<-function(DF,Truth){
  
######################
### Gathering Data ###
######################
  
DF<-scale(DF)
distDF<-dist(DF)

########################################
### Fitting Auto-Optimized Algorithm ###
########################################

resF<-hddc(DF,K=1:15,model=c(1),algo='EM',init ='kmeans',
             mini.nb=c(5,10),min.individuals=2,d_max=100)$class
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