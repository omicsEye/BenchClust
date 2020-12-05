
#############################################
### Fitting louvain (Community Detection) ###
#############################################

if(!require("purrr")){
install.packages("purrr")
}
if(!require("igraph")){
install.packages("igraph")
}
if(!require("psych")){
install.packages("psych")
}
suppressPackageStartupMessages(library(purrr)) # MAP function
suppressPackageStartupMessages(library(igraph)) # louvain function inside igraph library
suppressPackageStartupMessages(library(psych)) # cor2dist function

#############################
### Initializing function ###
#############################

run.louvain<-function(DF,Truth){
  
######################
### Gathering Data ###
######################

distDF<-dist(DF)
DFL<-scale(t(DF))
corDF<-cor(DFL)

#####################################
### Cleaning for Low correlations ###
#####################################

distDFL<-cor2dist(corDF)
distDFL<-as.matrix(distDFL)
distDFL[abs(corDF)<0.2]=0

#########################
### Fitting Algorithm ###
#########################

G1<-graph.adjacency(distDFL,mode="undirected",weighted=T,diag=T)
resF<-cluster_louvain(G1)$membership
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