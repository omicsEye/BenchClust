#####################
### Clear Console ###
#####################

rm(list=ls())

########################################
### Initializing Evaluation function ###
########################################

core.methods<-function(NUM){
  print(paste("Scenario:",NUM,"Running",sep=" "))

#########################
### Loading Libraries ###
#########################

workDirectory<-"D:/Repos/omeClust_analysis/R"
suppressPackageStartupMessages(library(pacman))
pkgmaker::source_files(paste(workDirectory,'Library',sep='/'),'*.R')
suppressPackageStartupMessages(pacman::p_load(pkgmaker,data.table,stringr))

##################                                             
### List Files ###
##################

DFlist<-list.files(paste(workDirectory,"Input","Raw_Data",sep='/'))
TRlist<-list.files(paste(workDirectory,"Input","Truth_Data",sep='/'))
  
##################                                             
### Input Data ###
##################

DF<-data.frame(read.table(paste(workDirectory,"Input","Raw_Data",DFlist[NUM],sep='/')))
Truth<-data.frame(read.table(paste(workDirectory,"Input","Truth_Data",TRlist[NUM],sep='/')))

##################################                                             
### Gathering Input Parameters ###
##################################

inputStringlabels<-c('clusters','features','sample','alpha','sd')
input<-DFlist[NUM]
input<-tools::file_path_sans_ext(input)
input<-str_split(input,pattern="_")[[1]]
names(input)<-inputStringlabels
clusters<-as.numeric(input["clusters"])
features<-as.numeric(input["features"])
sample<-as.numeric(input["sample"])
alpha<-as.numeric(input["alpha"])
sd<-as.numeric(input["sd"])

############################                                            
### Initializing Methods ###
############################

resDF<-NULL
methods<-c("CIDR","Seurat","kmeans","Hierarchical","hcmodel","Dbscan",
           "hddc","louvain","SNNclust")
for(i in seq_along(methods)){
  run.method<-methods[i]
  print(paste("Algorithm:",run.method,"Running",sep=" "))

#######################                                            
### Running Methods ###
#######################

if(run.method=="kmeans"){  
  output<-run.kmeans(DF=DF,Truth=Truth)
  }else if(run.method=="Hierarchical"){  
    output<-run.hclust(DF=DF,Truth=Truth)
    }else if(run.method=="hcmodel"){  
      output<-run.hcmodel(DF=DF,Truth=Truth)
      }else if(run.method=="Dbscan"){  
        output<-run.Dbscan(DF=DF,Truth=Truth)
        }else if(run.method=="hddc"){  
          output<-run.hddc(DF=DF,Truth=Truth)
          }else if(run.method=="louvain"){  
            output<-run.louvain(DF=DF,Truth=Truth)
            }else if(run.method=="SNNclust"){  
              output<-run.SNNclust(DF=DF,Truth=Truth)
              }
if (dim(output)[1]<1){
  print(paste('No Output for:',run.method,sep=" "))
  }

###############################
### Shaping Output's format ###
###############################

outputString<-paste(clusters,features,sample,alpha,sd,sep='_')
resDF.tmp<-data.frame(Method=run.method,Features=features,
                      Samples=sample,Clust.Separation=alpha,
                      Variability=sd,True.Clusters=clusters,output,check.names=F)
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
outputStringR<-paste(outputDirectory,paste(outputString,'.csv',sep=''),sep='/')
write.table(resDF,file=outputStringR,sep=",",row.names=F)
}



