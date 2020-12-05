#####################
### Clear Console ###
#####################

rm(list=ls())

#########################
### Loading Libraries ###
#########################

workDirectory<-"/gpfs/gsfs11/users/chatterjees7/omeClust/SingleCell_analysis"
suppressPackageStartupMessages(library(SingleCellExperiment))
suppressPackageStartupMessages(library(scater))
suppressPackageStartupMessages(library(Rfast))
suppressPackageStartupMessages(library(gtools))
suppressPackageStartupMessages(library(Seurat))

#################                                             
### List File ###
#################

scDatalist<-list.files(paste(workDirectory,"Data",sep='/'),".rds")

##########################                                             
### Looping over Files ###
##########################

for(i in seq_along(scDatalist)){
  
#############################                                             
### Getting Dataset names ###
#############################

scDataName<-tools::file_path_sans_ext(scDatalist[i])

#################################                                           
### Quality Control Real Data ###
#################################

print(paste("Dataset",scDataName,"running",sep=" "))
DF<-readRDS(paste(workDirectory,"Data",scDatalist[i],sep="/"))
DF<-DF[!duplicated(rowData(DF)$feature_symbol),] ## Filtering Duplicated Gene names
keep_feature<-rowSums(counts(DF)>0)>5 ## Filtering features with very low overall expression
DF<-DF[keep_feature,]
DF<-addPerCellQC(DF)
libsize.drop <- isOutlier(DF$total,nmads=3, type="lower", log=TRUE)
feature.drop <- isOutlier(DF$detected, nmads=3, type="lower", log=TRUE)
DF<-DF[,!(libsize.drop | feature.drop)] ## Removing low-quality cells based on outliers
data.frame(ByLibSize=sum(libsize.drop),
           ByFeature=sum(feature.drop),
           Remaining=ncol(DF))

###########################################                                           
### Getting Variable Features By Seurat ###
###########################################

counts<-counts(DF)
cso <- Seurat::CreateSeuratObject(counts=counts,min.cells=5)  #filter
cso <- Seurat::NormalizeData(cso,normalization.method = "LogNormalize") #normalize
cso <- Seurat::ScaleData(object = cso)
cso <- Seurat::FindVariableFeatures(cso,selection.method = "vst",nfeatures = 2000)
varFeats <- VariableFeatures(cso)
extract.varFeats<- counts[row.names(counts) %in% varFeats,]
  
######################                                          
### Gathering Data ###
######################
             
rawData<-extract.varFeats ## Rows genes and columns Cells
metaData<-colData(DF)
metaData<-metaData$cell_type1
nclusters<-length(unique(metaData))
  
################################                                           
### Creating Distance Matrix ###
################################
  
distDF<-as.matrix(t(rawData))
rownames(distDF)<-paste("S",1:nrow(distDF),"_",metaData,sep="")
adist<-Dist(distDF)
rownames(adist)<-rownames(distDF)
colnames(adist)<-rownames(distDF)
  
#############################                                           
### Creating Truth Matrix ###
#############################
  
Truth<-NULL
for(j in seq_along(unique(metaData))){
  tmp<-distDF[grepl(unique(metaData)[j],rownames(distDF)),]
  tmp1<-data.frame(Sample=sub("_.*","",rownames(tmp)),
                   cluster=rep(j,nrow(tmp)))
  Truth<-data.frame(rbind(Truth,tmp1))
  }
Truth<-Truth[gtools::mixedorder(Truth$Sample), ]
Truth_meta<-data.frame(Ground.Truth=Truth$cluster)
rownames(Truth_meta)<-rownames(adist)
  
#################################                                           
### Creating Output Directory ###
#################################

outputDirectory<-paste(workDirectory,"Input",sep="/")
if (!dir.exists(outputDirectory))
  {
  dir.create(outputDirectory)
  }

#####################################                                           
### Creating Output String Labels ###
#####################################

outputString1<-paste(outputDirectory,paste("Counts_",scDataName,'.RData',sep=''),sep='/')
outputString2<-paste(outputDirectory,paste("Truth_",scDataName,'.RData',sep=''),sep='/')
outputString3<-paste(outputDirectory,paste("adist_",scDataName,'.txt',sep=''),sep='/')
outputString4<-paste(outputDirectory,paste("omeMeta_",scDataName,'.txt',sep=''),sep='/')

###################                                           
### Saving Data ###
###################

save(rawData,file=outputString1)
save(Truth,file=outputString2)
write.table(adist,outputString3,sep="\t")
write.table(Truth_meta,outputString4,sep="\t")
}
