install.packages(devtools)

devtools::install_github("xu-lab/SINCERA")
library(SINCERA)
sc <- construct(exprmatrix=raw_data)
data("E16.5")
raw_data <- as.data.frame(t(raw_data)) 
sc <- construct(exprmatrix=raw_data,
                samplevector=colnames(raw_data))
sc <- normalization.zscore(sc, pergroup=F)
sc <- cluster.geneSelection(sc, method="specificity", pergroup=T, min.samples=2)
sc <- doPCA(sc, genes=getGenesForClustering(sc), use.fast = T, center=T, scale.=T)


