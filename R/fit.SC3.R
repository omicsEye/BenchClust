library(devtools)
# install from Github
#devtools::install_github("hemberg-lab/SC3")


## install from download code
install.packages('~/Downloads/SC3-master', repos = NULL, type = 'source', force = T)


## install from Biocondacture
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

#BiocManager::install("SC3")
BiocManager::install("scater")

library(SingleCellExperiment)
library(SC3)
library(scater)

head(ann)
yan[1:3, 1:3]

sce <- SingleCellExperiment(
  assays = list(
    counts = as.matrix(yan),
    logcounts = log2(as.matrix(yan) + 1)
  ), 
  colData = ann
)

# define feature names in feature_symbol column
rowData(sce)$feature_symbol <- rownames(sce)
# remove features with duplicated names
sce <- sce[!duplicated(rowData(sce)$feature_symbol), ]
sce <- runPCA(sce)
sce <- sc3(sce, ks = 2:4, biology = TRUE)
sc3_interactive(sce)
sc3_plot_consensus(
  sce, k = 3, 
  show_pdata = c(
    "cell_type1", 
    "log10_total_features",
    "sc3_3_clusters", 
    "sc3_3_log2_outlier_score"
  )
)



sce <- SingleCellExperiment(
  assays = list(
    counts = as.matrix(raw_data),
    logcounts = log2(as.matrix(raw_data) + 1)
  ), 
  colData = colnames(raw_data)
)

# define feature names in feature_symbol column
rowData(sce)$feature_symbol <- rownames(sce)
# remove features with duplicated names
sce <- sce[!duplicated(rowData(sce)$feature_symbol), ]
runPCA(sce)
sce <- sc3(sce, ks = 2:4, biology = F)
sc3_consensus(sce) 
sc3_export_results_xls(sce)
