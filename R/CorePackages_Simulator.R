
#########################
### Loading Libraries ###
#########################

start.time<-Sys.time()
reqpkg<-c("stringr","stats","psych","clusterlab","data.table","ggplot2")
for (q in reqpkg)
{
print(q)
print(packageVersion(q))
suppressPackageStartupMessages(library(q,character.only=T,quietly=T)) 
}
workDirectory<-"D:/Repos/omeClust_analysis/R"
if(!require("clusterlab")){
install.packages(paste(inputDirectory,"Rpackages","clusterlab_0.0.2.8.tar.gz",sep="/")
                ,repos=NULL,type="source")
}
library(clusterlab)

###########################
### Locking Directories ###
###########################

inputDirectory<-file.path(workDirectory,'Input')
if (!dir.exists(inputDirectory))
{
dir.create(inputDirectory)
} 

###############################
### Initializing parameters ###
###############################

clusters<-c(4,6,8)
features<-c(500,1000,1500) 
sample<-c(10,20,40)
alpha<-c(0.05,0.10,0.25,0.5,1)
sd<-c(1)
inputString<-apply(expand.grid(clusters,features,sample,alpha,sd),
                   1,paste0,collapse="_")
inputStringlabels<-c('clusters','features','sample','alpha','sd')
      
#######################################
### Creating Simulation Environment ###
#######################################

for(i in 1:length(inputString))
{
print(paste("Simulation",i,"running",sep=" "))
input<-str_split(inputString[i],pattern="_")[[1]]
names(input)<-inputStringlabels
clusters<-as.numeric(input["clusters"])
features<-as.numeric(input["features"])
sample<-as.numeric(input["sample"])
alpha<-as.numeric(input["alpha"])
sd<-as.numeric(input["sd"])

#######################
### Simulating data ###
#######################

alphas<-rep(alpha,clusters)
samples<-rep(sample,clusters)
sds<-rep(sd,clusters)
synthetic<-clusterlab(centers=clusters,alphas=alphas,mode='circle',
                      numbervec=samples,features=features,
                      sdvec=sds,seed=1234,showplots=F)

######################
### Gathering data ###
######################

sim.df<-data.frame(t(synthetic$synthetic_data))
dist.df<-as.matrix(dist(sim.df))
truth.df<-synthetic$identity_matrix
truth.meta<-data.frame(Ground.Truth=truth.df$cluster,
                       row.names=truth.df$sampleID)

#################################
### Creating plot Environment ###
#################################

pcs<-prcomp(sim.df)
scores<-data.frame(pcs$x)
fontsize<-10
title<-paste("Simulated Data: Clusters=",clusters,",Features=",features,
             ",Samples=",sample*clusters,",Dist=",alpha,",SD=",sd,sep="")

#####################
### Plotting data ###
#####################

p<-ggplot(data=scores,aes(x=PC1,y=PC2,fill=truth.df$cluster))+
  geom_point(alpha=0.75,size=1.5,shape=21)+theme_bw()+ 
  theme(legend.position = "none", panel.grid.major=element_blank(), 
  panel.grid.minor = element_blank(),
  axis.text.y = element_text(size=fontsize, colour = 'black'),
  axis.text.x = element_text(size=fontsize, colour = 'black'),
  axis.title.x = element_text(size=fontsize),
  axis.title.y = element_text(size=fontsize))+
  ggtitle(title)

#############################
### Creating data folders ###
#############################

truthDirectory <- paste(inputDirectory,'Truth_Data',sep="/")
if (!dir.exists(truthDirectory))
{
dir.create(truthDirectory)
}
MetatruthDirectory <- paste(inputDirectory,'Truth_meta',sep="/")
if (!dir.exists(MetatruthDirectory))
{
dir.create(MetatruthDirectory)
}
rawDirectory <- paste(inputDirectory,'Raw_Data',sep="/")
if (!dir.exists(rawDirectory))
{
dir.create(rawDirectory)
}
DistDirectory <- paste(inputDirectory,'Distance_Data',sep="/")
if (!dir.exists(DistDirectory))
{
dir.create(DistDirectory)
}
plotDirectory <- paste(inputDirectory,'Plot_Data',sep="/")
if (!dir.exists(plotDirectory))
{
dir.create(plotDirectory)
}

###################
### Saving data ###
###################

outstringData<-paste(paste(clusters,features,
                     sample*clusters,alpha,sd,sep="_"),".txt",sep="")
outstringPlot<-paste(paste(clusters,features,
                     sample*clusters,alpha,sd,sep="_"),".pdf",sep="")
write.table(sim.df,paste(rawDirectory,outstringData,sep="/"),
            sep="\t",row.names=T)
write.table(dist.df,paste(DistDirectory,outstringData,sep="/"),
            sep="\t",row.names=T)
write.table(truth.df,paste(truthDirectory,outstringData,sep="/"),
       sep="\t",row.names=T)
write.table(truth.meta,paste(MetatruthDirectory,outstringData,sep="/"),
       sep="\t",row.names=T)
ggsave(paste(plotDirectory,outstringPlot,sep="/"),
       width=8,height=4,dpi=350)
}

############################
### Data generation Time ###
############################

stop.time<-Sys.time()
minutes<-round(difftime(stop.time,start.time,units="min"),1)
print(paste("Data generation time:",minutes,"minutes",sep=" "))



