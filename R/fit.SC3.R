
#########################################
### Fitting RaceID3 (Partition-based) ###
#########################################

if(!require("SC3")){
  BiocManager::install("SC3")
}
suppressPackageStartupMessages(library(SC3)) # RaceID function

#############################
### Initializing function ###
#############################

run.SC3<-function(rawData,Truth){
  
#########################
### Running Algorithm ###
#########################

sce <- SingleCellExperiment::SingleCellExperiment(assays = list(counts = rawData))
rowData(sce)$feature_symbol <- rownames(rawData)
assay(sce, "logcounts") <- counts(sce)
sce <- SC3::sc3_prepare(sce, gene_filter=TRUE) 
sce <- SC3::sc3_estimate_k(sce)  
k_input <- metadata(sce)$sc3$k_estimation
sce <- SC3::sc3_calc_dists(sce)
sce <- SC3::sc3_calc_transfs(sce)
sce <- SC3::sc3_kmeans(sce, ks = k_input)
sce <- SC3::sc3_calc_consens(sce)
clusters<-as.numeric(colData(sce)[paste0("sc3_",k_input, "_clusters")][[1]])
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