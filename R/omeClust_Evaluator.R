#####################
### Clear Console ###
#####################

rm(list=ls())

#########################
### Loading Libraries ###
#########################

workDirectory<-"D:/Repos/omeClust_analysis/R"
suppressPackageStartupMessages(library(pacman))
pkgmaker::source_files(paste(workDirectory,'Library_CorePackages',sep='/'),'*.R')
suppressPackageStartupMessages(pacman::p_load(pkgmaker,data.table,stringr))

##################                                             
### List Files ###
##################

DFlist<-list.files(paste(workDirectory,"ome_results","Distance",sep='/'))
TRlist<-list.files(paste(workDirectory,"ome_results","Truth",sep='/'))

########################################
### Initializing Evaluation function ###
########################################

resDF<-NULL
for(NUM in seq_along(DFlist)){
  print(paste("Scenario:",NUM,"Running",sep=" "))
  
#######################                                             
### Input Main Data ###
#######################

DF<-data.frame(read.table(paste(workDirectory,"ome_results","Distance",DFlist[NUM],sep='/')))
Truth<-data.frame(read.table(paste(workDirectory,"ome_results","Truth",TRlist[NUM],sep='/')))

##########################################                                             
### Input OmeClust Clusters by Linkage ###
##########################################

  omeClusters<-data.frame(read.table(paste(workDirectory,"ome_results",
                                        DFlist[NUM],
                                        "feature_cluster_label.txt",
                                         sep='/'),h=T))
                                              
##################################                                             
### Gathering Input Parameters ###
##################################

DataName<-DFlist[NUM]
DataName<-tools::file_path_sans_ext(DataName)
DataName<-str_split(DataName,pattern="_")[[1]][[2]]
True.Clusters<-length(unique(Truth$Ground.Truth))

######################                                           
### Running Method ###
######################

output<-run.omeClust(DF=DF,Truth=Truth,omeClusters=omeClusters)

###############################
### Shaping Output's format ###
###############################

Method<-"omeClust"
resDF.tmp<-data.frame(Method=Method,DataName=DataName,
                      True.Clusters=True.Clusters,
                      output,check.names=F)
resDF<-data.frame(rbind(resDF,resDF.tmp),check.names=F)
}

#######################
### Printing Output ###
#######################

outputDirectory<-paste(workDirectory,'Outputs',sep="/")
if (!dir.exists(outputDirectory))
{
  dir.create(outputDirectory)
}
outputStringR<-paste(outputDirectory,paste(Method,'.csv',sep=''),sep='/')
write.table(resDF,file=outputStringR,sep=",",row.names=F)




