
#########################################
### Fitting RaceID3 (Partition-based) ###
#########################################

if(!require("RaceID")){
  suppressPackageStartupMessages(library(devtools))
  install_github("dgrun/RaceID3_StemID2_package")
}
suppressPackageStartupMessages(library(RaceID)) # RaceID function

#############################
### Initializing function ###
#############################

run.RaceID3<-function(rawData,Truth){
  
#########################
### Running Algorithm ###
#########################

scs <- SCseq(rawData)
scs <- filterdata(scs,minexpr=5,minnumber=1)
scs<-RaceID::CCcorrect(scs, mode="pca", dimR=TRUE)
scs <- compdist(scs)
race3<-RaceID::clustexp(scs, sat=TRUE, FUNcluster="kmeans")
clusters <- as.numeric(race3@cluster$kpart)
missing_ids=which( colnames(scs@expdata) %in% setdiff(colnames(scs@expdata),
                                                      names(race3@cluster$kpart)))
if(length(missing_ids)==0){
  clusters<-clusters
  true_labels<-Truth$cluster
  predicted_labels<-clusters
} else{
  true_labels<-Truth$cluster
  true_labels<-true_labels[1:length(clusters)]
  predicted_labels<-clusters
}
pred.Clusters<-as.numeric(length(unique(clusters)))

#############################
### Measuring Performance ###
#############################


performance<-cluster_eval(true_labels=true_labels,
                          predicted_labels=predicted_labels)
performance<-data.frame(pred.Clusters=pred.Clusters,performance,check.names=F)
return(performance)
}  