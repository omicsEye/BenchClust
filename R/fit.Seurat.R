
######################################
### Fitting Dbscan (Density-Based) ###
######################################

if(!require("Seurat")){
install.packages("Seurat")
}
suppressPackageStartupMessages(library(Seurat)) # Seurat function

#############################
### Initializing function ###
#############################

run.Seurat<-function(rawData,Truth){
  
#########################
### Running Algorithm ###
#########################

cso <- Seurat::CreateSeuratObject(counts=rawData,min.cells=5)  #filter
cso <- Seurat::NormalizeData(cso,normalization.method = "LogNormalize") #normalize
cso <- Seurat::ScaleData(object = cso)
cso <- Seurat::FindVariableFeatures(cso,selection.method = "vst")
cso <- Seurat::RunPCA(cso,features=VariableFeatures(object = cso))
cso <- Seurat::FindNeighbors(cso, dims = 1:10)
cso <- Seurat::FindClusters(cso, resolution = 0.5)
clusters <- as.numeric(Idents(cso))
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