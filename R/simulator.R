
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

workingDirectory<-'D:\\research\\Projects\\m2clust\\cluster_simulator'
if(!require("clusterlab")){
install.packages(paste(workingDirectory,"clusterlab_0.0.2.8.tar.gz",sep="\\")
                  ,repos=NULL,type="source")
}
library(clusterlab)

###########################
### Locking Directories ###
###########################

inputDirectory<-file.path(workingDirectory,'Simulated_data')
if (!dir.exists(inputDirectory))
{
  dir.create(inputDirectory)
} 
outputDirectory<-file.path(inputDirectory,'Input')
if (!dir.exists(outputDirectory))
{
  dir.create(outputDirectory)
} 

###############################
### Initializing parameters ###
###############################

clusters<-c(4,6,8)
features<-c(500,1000,1500) 
sample<-c(10,30)
alpha<-c(0.4,0.8,1.2)
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

sim.df<-synthetic$synthetic_data
cors.sim<-data.frame(cor(sim.df,use="pairwise.complete.obs",
                         method="pearson"))
dist.df<-data.frame(cor2dist(cors.sim))
truth.df<-synthetic$identity_matrix
truth.meta<-data.frame(Ground.Truth=truth.df$cluster,
                       row.names=truth.df$sampleID)

#################################
### Creating plot Environment ###
#################################

pcs<-prcomp(data.frame(t(sim.df)))
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

truthDirectory <- paste(outputDirectory,'Truth_Data',sep="\\")
if (!dir.exists(truthDirectory))
{
  dir.create(truthDirectory)
}
MetatruthDirectory <- paste(outputDirectory,'Truth_meta',sep="\\")
if (!dir.exists(MetatruthDirectory))
{
  dir.create(MetatruthDirectory)
}
distDirectory <- paste(outputDirectory,'Distance_Data',sep="\\")
if (!dir.exists(distDirectory))
{
  dir.create(distDirectory)
}
corDirectory <- paste(outputDirectory,'Corr_Data',sep="\\")
if (!dir.exists(corDirectory))
{
  dir.create(corDirectory)
}
rawDirectory <- paste(outputDirectory,'Raw_Data',sep="\\")
if (!dir.exists(rawDirectory))
{
  dir.create(rawDirectory)
}
plotDirectory <- paste(outputDirectory,'Plot_Data',sep="\\")
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
write.table(dist.df,paste(distDirectory,outstringData,sep="\\"),
       sep="\t",row.names=T)
write.table(cors.sim,paste(corDirectory,outstringData,sep="\\"),
            sep="\t",row.names=T) 
write.table(sim.df,paste(rawDirectory,outstringData,sep="\\"),
            sep="\t",row.names=T)
write.table(truth.df,paste(truthDirectory,outstringData,sep="\\"),
       sep="\t",row.names=T)
write.table(truth.meta,paste(MetatruthDirectory,outstringData,sep="\\"),
       sep="\t",row.names=T)
ggsave(paste(plotDirectory,outstringPlot,sep="\\"),
       width=8,height=4,dpi=350)
}

############################
### Data generation Time ###
############################

stop.time<-Sys.time()
minutes<-round(difftime(stop.time,start.time,units="min"),1)
print(paste("Data generation time:",minutes,"minutes",sep=" "))



