
#########################################
### Fitting RaceID3 (Partition-based) ###
#########################################

  if(!require("monocle")){
    BiocManager::install("monocle")
  }
suppressPackageStartupMessages(library(reticulate))
suppressPackageStartupMessages(library(monocle))

#############################
### Initializing function ###
#############################

run.Monocle3<-function(rawData,Truth){
  
#########################
### Running Algorithm ###
#########################

  col_info=data.frame(colnames(rawData))
  rownames(col_info)<- colnames(rawData)
  pd <- new("AnnotatedDataFrame", data = col_info)
  gene_info=data.frame(rownames(rawData))
  names(gene_info)="gene_short_name"
  rownames(gene_info)<- rownames(rawData)
  fd <- new("AnnotatedDataFrame", data = gene_info)
  cds <- monocle::newCellDataSet(cellData = rawData, phenoData = pd, featureData = fd)
  cds <- estimateSizeFactors(cds)
  cds <- estimateDispersions(cds)
  cds <- monocle::reduceDimension(cds, reduction_method = "tSNE")
  cds <- monocle::clusterCells(cds, num_clusters = NULL, method = "louvain")
  clusters <- as.numeric(levels(cds$Cluster))[cds$Cluster]
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