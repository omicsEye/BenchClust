library(Seurat)
library(deepath)

##############################
### read syntheics data ######
##############################

syn_data <-read.delim(
  "~/Downloads/6_1500_180_1.2_1.txt",
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


####RAceID test #######

library(RaceID)
library(Hmisc)

m <- syn_data #as.data.frame(matrix(rexp(200, rate=.01), ncol=20))
sc <- SCseq(abs(m))
#sc <- filterdata(sc,mintotal=2000)
#fdata <- getfdata(sc)
#sc <- compdist(sc,metric="pearson")
x_corr <- rcorr(as.matrix(m), type="pearson")
attr(sc, "distances") <- as.matrix(dist(m, method = "euclidean"))
#attr(sc, "distances") <-as.matrix(vegdist(m, method="bray"))
attr(sc, "distances") <- 1.0 - x_corr$r
sc <- clustexp(sc, clustnr = 10)

##### viz functions  ##########
plotsaturation(sc,disp=FALSE)
plotsaturation(sc,disp=TRUE)
plotjaccard(sc)
#############################

#cluster labels
member.df <- data.frame(attr(sc,'cluster')$kpart)

member.df$members <- rownames(member.df)
colnames(member.df) <- c("cluster", "members")
#number of clusters
attr(sc,'cluster')$clb$nc



