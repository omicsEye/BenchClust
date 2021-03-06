
###########################################################
### Evaluating Clustering Performance to simulated Data ###
###########################################################

if(!require("mclustcomp")){
  install.packages("mclustcomp")
}
if(!require("sabre")){
  install.packages("sabre")
}
suppressPackageStartupMessages(library(mclustcomp))
suppressPackageStartupMessages(library(sabre))
<<<<<<< HEAD
suppressPackageStartupMessages(library(cluster))
=======
>>>>>>> 58130d51055631420f557aea35f405f93b3fa9d9

#############################
### Initializing function ###
#############################

<<<<<<< HEAD
cluster_eval<-function(true_labels,predicted_labels)
=======
cluster_eval<-function(true_labels,predicted_labels,distDF)
>>>>>>> 58130d51055631420f557aea35f405f93b3fa9d9
{
true_labels<-as.numeric(true_labels)
predicted_labels<-as.numeric(predicted_labels)

###########################
### Calculating Metrics ###
###########################

m2<-mclustcomp(true_labels,predicted_labels)
<<<<<<< HEAD
=======
silhoutte.score<-silhouette(predicted_labels,distDF)
if(is.na(silhoutte.score)){
  silhoutte.score<-0
}else if(round(mean(silhoutte.score[,"sil_width"]),3)<0){
  silhoutte.score<-0
}else{
  silhoutte.score<-round(mean(silhoutte.score[,"sil_width"]),3)
}
>>>>>>> 58130d51055631420f557aea35f405f93b3fa9d9

#########################
### Gathering Outputs ###
#########################

NMI<-m2[m2$types=="nmi1",2]
NVI<-m2[m2$types=="nvi",2]
ARI<-m2[m2$types=="adjrand",2]
if(ARI<0){
  ARI<-0
}
Jaccard<-m2[m2$types=="jaccard",2]
Fowlkes_mallows<-m2[m2$types=="fmi",2]
F1_score<-m2[m2$types=="sdc",2]
Mirkin<-m2[m2$types=="mirkin",2]
Vmeasure<-vmeasure(true_labels,predicted_labels,z=NULL,B=1)$v_measure
<<<<<<< HEAD
measures.df<-data.frame(NMI=NMI,
=======
measures.df<-data.frame(silhoutte.score=silhoutte.score,NMI=NMI,
>>>>>>> 58130d51055631420f557aea35f405f93b3fa9d9
                        NVI=NVI,ARI=ARI,Jaccard=Jaccard,F1_score=F1_score,
                        Fowlkes_mallows=Fowlkes_mallows,
                        Mirkin=Mirkin,Vmeasure=Vmeasure)
return(measures.df)
}
