#####################
### Clear Console ###
#####################

rm(list=ls())

#########################
### Loading Libraries ###
#########################

workDirectory<-"D:/Repos/omeClust_analysis/R"
pkgmaker::source_files(paste(workDirectory,'Library',sep='/'),'*.R')
suppressPackageStartupMessages(pacman::p_load(pkgmaker,data.table,stringr))

##################                                             
### List Files ###
##################

countsList<-list.files(paste(workDirectory,"Input",sep='/'),"*Counts")
truthList<-list.files(paste(workDirectory,"Input",sep='/'),"*Truth")

########################################
### Initializing Evaluation function ###
########################################

for(NUM in seq_along(countsList)){
  print(paste("Scenario:",NUM,"Running",sep=" "))
  
##################                                             
### Input Data ###
##################

load(paste(workDirectory,"Input",countsList[NUM],sep='/'))
load(paste(workDirectory,"Input",truthList[NUM],sep='/'))

##################################                                             
### Gathering Input Parameters ###
##################################

inputStringlabels<-c('DataName','clusters','features','sample')
DataName<-countsList[NUM]
DataName<-tools::file_path_sans_ext(DataName)
DataName<-str_split(DataName,pattern="_")[[1]][2]
clusters<-as.numeric(length(unique(Truth$cluster)))
features<-as.numeric(dim(rawData)[1])
sample<-as.numeric(dim(rawData)[2])
input<-c(DataName,clusters,features,sample)
names(input)<-inputStringlabels

############################                                            
### Initializing Methods ###
############################

res<-NULL
methods<-c("Seurat","Monocle3","CIDR","sincell",
           "pcaReduce","SC3","RaceID3","sscClust","TSCAN")
for(i in seq_along(methods)){
  run.method<-methods[i]
  print(paste("Algorithm:",run.method,"Running",sep=" "))

#######################                                            
### Running Methods ###
#######################

if(run.method=="Seurat"){  
  output<-run.Seurat(rawData=rawData,Truth=Truth)
  }else if(run.method=="Monocle3"){  
    output<-run.Monocle3(rawData=rawData,Truth=Truth)
    }else if(run.method=="Linnorm"){  
      output<-run.Linnorm(rawData=rawData,Truth=Truth)
      }else if(run.method=="CIDR"){  
        output<-run.CIDR(rawData=rawData,Truth=Truth)
        }else if(run.method=="sincell"){  
          output<-run.sincell(rawData=rawData,Truth=Truth)
          }else if(run.method=="pcaReduce"){  
            output<-run.pcaReduce(rawData=rawData,Truth=Truth)
            }else if(run.method=="SC3"){  
              output<-run.SC3(rawData=rawData,Truth=Truth)
            }else if(run.method=="RaceID3"){  
              output<-run.RaceID3(rawData=rawData,Truth=Truth)
            }else if(run.method=="sscClust"){  
              output<-run.sscClust(rawData=rawData,Truth=Truth)
            }else if(run.method=="TSCAN"){  
              output<-run.TSCAN(rawData=rawData,Truth=Truth)
            }
if (dim(output)[1]<1){
  print(paste('No Output for:',run.method,sep=" "))
  next
  }

###############################
### Shaping Output's format ###
###############################

outputString<-paste(DataName,clusters,features,sample,sep='_')
res.tmp<-data.frame(Method=run.method,Features=features,
                      Samples=sample,DataName=DataName,
                      True.Clusters=clusters,output,check.names=F)
res<-data.frame(rbind(res,res.tmp),check.names=F)
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
write.table(res,file=outputStringR,sep=",",row.names=F)
}



