
#########################################
### Fitting RaceID3 (Partition-based) ###
#########################################

  if(!require("sscClust")){
    BiocManager::install("ComplexHeatmap")
    devtools::install_github("Japrin/sscVis")
    devtools::install_github("Japrin/sscClust")
  }
suppressPackageStartupMessages(library(sscClust))

#############################
### Initializing function ###
#############################

run.sscClust<-function(rawData,Truth){
  
#########################
### Running Algorithm ###
#########################

  sce <- SingleCellExperiment::SingleCellExperiment(assays = list(counts = rawData))
  rowData(sce)$feature_symbol <- rownames(rawData)
  sscc <- sscClust::ssc.run(sce, assay.name="counts", method.reduction="pca", 
                            method.clust = "SNN") 
  clusters <- as.numeric(factor(colData(sscc)[[1]]))
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