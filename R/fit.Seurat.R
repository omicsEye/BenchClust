library(Seurat)
library(deepath)

##############################
### read syntheics data ######
##############################

syn_data <-read.delim(
  "6_1500_180_1.2_1.txt",
  sep = '\t',
  header = T,
  fill = F,
  comment.char = "" ,
  check.names = F,
  row.names = 1
)

##############################
### run Seurat on HMP1-SPecies data ##########
##############################


raw_data <- data_in
pbmc <- CreateSeuratObject(counts = raw_data)
pbmc <- NormalizeData(object = pbmc)
pbmc <- FindVariableFeatures(object = pbmc)
pbmc <- ScaleData(object = pbmc)
pbmc <- RunPCA(object = pbmc)
pbmc <- FindNeighbors(object = pbmc)
pbmc <- FindClusters(object = pbmc)
pbmc <- RunTSNE(object = pbmc, check_duplicates = FALSE)
tsne_plot <- DimPlot(object = pbmc, reduction = "tsne") + theme_omicsEye() 
ggsave(filename = 'Seurat_tsne.png', 
       plot=tsne_plot, width = 3.6, height = 3.25, units = "in", dpi = 350)




