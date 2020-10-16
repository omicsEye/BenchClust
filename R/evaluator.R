
#########################
### Loading Libraries ###
#########################

workingDirectory<-"D:\\research\\Projects\\m2clust\\cluster_simulator\\Simulated_data"
suppressPackageStartupMessages(library(pacman))
suppressPackageStartupMessages(pacman::p_load(pkgmaker,cluster,factoextra,sabre,
                                              fpc,NbClust,dendextend,dplyr,
                                              data.table,plyr,ggplot2,Hmisc,
                                              gridExtra,ggpubr,grid,stringr,
                                              reshape,rlist,ClusterR))
                                                               
pkgmaker::source_files(paste(workingDirectory,'Library',sep='\\'),'*.R')

##############################################
### Running m2clust for best Configuration ###
##############################################

resolution<-c("low","medium","high")
linkage<-c("single","average","complete","centroid","median","weighted","ward")
results.df<-NULL
for(j in 1:length(resolution))
{
print(paste("Resolution",resolution[j],"running",sep=" "))
for(k in 1:length(linkage))
{
print(paste("Linkage",linkage[k],"running",sep=" "))
m2clust<-fit.m2clust.config(resolution=resolution[j],linkage=linkage[k],
                     workingDirectory=workingDirectory)
results.df<-data.frame(rbind(results.df,m2clust))
}
}
fwrite(results.df,paste(workingDirectory,"m2clust","m2clust_performance.csv",sep="\\"))
results.df$sampleSize.Level<-ifelse(results.df$sample<=120,"N = Small","N = Large")
                                    
############################################
### Plotting m2clust default performance ###
############################################

plotDirectory<-paste(workingDirectory,"plots",sep="\\")
results.df$resolution<-ifelse(results.df$resolution=="low",
                              "Low resolution",
                              ifelse(results.df$resolution=="medium",
                                     "Medium resolution",
                                     ifelse(results.df$resolution=="high",
                                            "High resolution",F)))
results.df$resolution<-factor(results.df$resolution ,
                              levels=c("Low resolution",
                                       "Medium resolution",
                                       "High resolution"))
                
##################
### Best Power ###
##################

q<-ggplot(results.df,aes(x=reorder(linkage,Silhoutte.score,median),
  y=sensitivity,fill=resolution))+geom_boxplot(
    ggplot2::aes(fill = resolution),
    outlier.alpha = 0.0,
    na.rm = TRUE,
    alpha = .5,
    show.legend = FALSE
  ) +
  geom_point(
    ggplot2::aes(fill = resolution),
    alpha = 0.75 ,
    size = 2,
    shape = 21,
    stroke = 0.15,
    color = 'black',
    position = ggplot2::position_jitterdodge()
  ) +
  scale_fill_brewer(palette = "Spectral", direction=-1)+
  theme_bw()+
  theme(legend.position="top",axis.title.x=element_blank(),
        axis.text.x=element_blank(),axis.ticks.x=element_blank(),
        legend.title=element_blank())+
  ggtitle("a, Sensitivity")+labs(y=NULL)+labs(x=NULL)+
  facet_wrap(~sampleSize.Level)

#############################
### Best Silhouette Score ###
#############################

r<-ggplot(results.df,aes(x=reorder(linkage,Silhoutte.score,median),
                         y=Silhoutte.score,fill=resolution))+geom_boxplot(
                           ggplot2::aes(fill = resolution),
                           outlier.alpha = 0.0,
                           na.rm = TRUE,
                           alpha = .5,
                           show.legend = FALSE) +
    geom_point(
    ggplot2::aes(fill = resolution),
    alpha = 0.75 ,
    size = 2,
    shape = 21,
    stroke = 0.15,
    color = 'black',
    position = ggplot2::position_jitterdodge()
  ) +
  scale_fill_brewer(palette = "Spectral", direction=-1)+
  theme_bw()+
  theme(legend.position="none",axis.title.x=element_blank(),
        axis.text.x=element_blank(),axis.ticks.x=element_blank())+
  ggtitle("b, Silhoutte Score")+labs(y=NULL,x=NULL)+
  facet_wrap(~sampleSize.Level)

#########################################
### Best Normalized Mutual Info Score ###
#########################################

s<-ggplot(results.df,aes(x=reorder(linkage,Silhoutte.score,median),
                         y=NMI,fill=resolution))+geom_boxplot(
                           ggplot2::aes(fill = resolution),
                           outlier.alpha = 0.0,
                           na.rm = TRUE,
                           alpha = .5,
                           show.legend = FALSE) +
  geom_point(
    ggplot2::aes(fill = resolution),
    alpha = 0.75 ,
    size = 2,
    shape = 21,
    stroke = 0.15,
    color = 'black',
    position = ggplot2::position_jitterdodge()
  ) +
  scale_fill_brewer(palette = "Spectral", direction=-1)+
  theme_bw()+
  theme(legend.position="none")+
  ggtitle("c, Normalized Mutual Information")+labs(y=NULL,x="Linkage")+
  facet_wrap(~sampleSize.Level)

###################################
### Combined Configuration plot ###
###################################

qrs<-arrangeGrob(q,r,s,ncol=1,nrow=3)
ggsave(paste(plotDirectory,"m2clust.config.png",sep="\\"),qrs,
       width=15,height=8,dpi=350)

##########################
### Methods Evaluation ###
##########################

######################                                                              
### Running Cutree ###
######################

inputDirectory<-"D:\\research\\Projects\\m2clust\\cluster_simulator\\Simulated_data"
outputDirectory<-"D:\\research\\Projects\\m2clust\\cluster_simulator\\Simulated_data\\Cutree"
cut.tree<-fit.cutree(inputDirectory,outputDirectory)

names(cut.tree)[6]<-"method"
cutree.fig<-cut.tree[,c("clusters","features","sample","method",
                        "major.clusters","Silhoutte.score",
                        "specificity","sensitivity","precision","Fmeasure",
                        "NMI","Accuracy","fowlkes.mallows","Vmeasure",
                        "FDR","FPR","residual")]
fwrite(cutree.fig,paste(workingDirectory,"Cutree","Cutree_evaluation.csv",sep="\\"))

#######################
### Running m2clust ###
#######################

resolution<-"low"
m2clust<-fit.m2clust.eval(resolution=resolution,
                     workingDirectory=workingDirectory)
m2clust.fig<-m2clust[,c("clusters","features","sample","method",
                        "major.clusters","Silhoutte.score",
                        "specificity","sensitivity","precision","Fmeasure",
                        "NMI","Accuracy","fowlkes.mallows","Vmeasure",
                        "FDR","FPR","residual")]
fwrite(m2clust.fig,paste(workingDirectory,"m2clust","m2clust_evaluation.csv",sep="\\"))

#######################
### Running Louvain ###
#######################

inputDirectory<-"D:\\research\\Projects\\m2clust\\cluster_simulator\\Simulated_data"
outputDirectory<-"D:\\research\\Projects\\m2clust\\cluster_simulator\\Simulated_data\\Louvain"
Louvain<-fit.Louvain(inputDirectory,outputDirectory)
Louvain.fig<-Louvain[,c("clusters","features","sample","method",
                        "major.clusters","Silhoutte.score",
                        "specificity","sensitivity","precision","Fmeasure",
                        "NMI","Accuracy","fowlkes.mallows","Vmeasure",
                        "FDR","FPR","residual")]
fwrite(Louvain.fig,paste(workingDirectory,"Louvain","Louvain_evaluation.csv",sep="\\"))

#######################
### Running Infomap ###
#######################

inputDirectory<-"D:\\research\\Projects\\m2clust\\cluster_simulator\\Simulated_data"
outputDirectory<-"D:\\research\\Projects\\m2clust\\cluster_simulator\\Simulated_data\\Infomap"
Infomap<-fit.Infomap(inputDirectory,outputDirectory)
Infomap.fig<-Infomap[,c("clusters","features","sample","method",
                        "major.clusters","Silhoutte.score",
                        "specificity","sensitivity","precision","Fmeasure",
                        "NMI","Accuracy","fowlkes.mallows","Vmeasure",
                        "FDR","FPR","residual")]
fwrite(Infomap.fig,paste(workingDirectory,"Infomap","Infomap_evaluation.csv",sep="\\"))

########################
### Plotting Results ###
########################

p.df<-data.frame(rbind(cutree.fig,m2clust.fig,Louvain.fig,Infomap.fig))
plotDirectory<-paste(workingDirectory,"plots",sep="\\")
p.df[p.df$method=="cut_0.25","method"]="cutree 0.25"
p.df[p.df$method=="cut_0.5","method"]="cutree 0.5"
p.df[p.df$method=="cut_0.6","method"]="cutree 0.6"
p.df[p.df$method=="cut_0.75","method"]="cutree 0.75"
p.df[p.df$method=="m2clust","method"]="omeClust"

########################
### Trigger OmicsEye ###
########################

omicsEye_theme <- function() {
  # set default text format based on categorical and length
  angle = NULL
  hjust = NULL
  size = 8
  axis_title_size = 10
  return ( ggplot2::theme_bw() + ggplot2::theme(
    axis.text.x = ggplot2::element_text(size = size, vjust = 1, hjust = hjust, angle = angle),
    axis.text.y = ggplot2::element_text(size = 8, hjust = 1),
    axis.title = ggplot2::element_text(size = axis_title_size),
    plot.title = ggplot2::element_text(size = 7, face = 'bold'),
    legend.title = ggplot2::element_text(size = 6, face = 'bold'),
    legend.text = ggplot2::element_text(size = 6),
    axis.line = ggplot2::element_line(colour = 'black', size = .25),
    axis.line.x = ggplot2::element_line(colour = 'black', size = .25),
    axis.line.y = ggplot2::element_line(colour = 'black', size = .25),
    panel.border = ggplot2::element_blank(),
    panel.grid.major = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank())
  )
}

##################
### Best Power ###
##################

a<-ggplot(p.df,aes(x=reorder(method,sensitivity,median),
    y=sensitivity,fill=method))+geom_boxplot(
    ggplot2::aes(fill = method),
    outlier.alpha = 0.0,
    na.rm = TRUE,
    alpha = .5,
    show.legend = FALSE
  ) +
  geom_point(
    ggplot2::aes(fill = method),
    alpha = 0.75 ,
    size = 0.75,
    shape = 21,
    stroke = 0.15,
    color = 'black',
    position = ggplot2::position_jitterdodge()
  ) +
  scale_fill_brewer(palette = "Spectral", direction=-1)+
  theme_bw()+ggtitle("a, Sensitivity")+labs(x=NULL,y=NULL)+
  omicsEye_theme()+theme(legend.position="none",axis.ticks.x=element_blank(),
                         axis.text.x=element_blank())
 
###################################
### Best Misclassification Rate ###
###################################

b<-ggplot(p.df,aes(x=reorder(method,sensitivity,median),
  y=residual,fill=method))+
  geom_boxplot(
    ggplot2::aes(fill = method),
    outlier.alpha = 0.0,
    na.rm = TRUE,
    alpha = .5,
    show.legend = FALSE
  ) +
  geom_point(
    ggplot2::aes(fill = method),
    alpha = 0.75 ,
    size = 0.75,
    shape = 21,
    stroke = 0.15,
    color = 'black',
    position = ggplot2::position_jitterdodge()
  ) +
  scale_fill_brewer(palette = "Spectral", direction=-1)+
  theme_bw()+ggtitle("b, Misclassification Rate")+labs(x=NULL,y=NULL)+
  omicsEye_theme()+theme(legend.position="none",axis.text.x = element_text(size = 7, 
                                                vjust = 1, hjust = 1, angle = 45))

################
### Best NMI ###
################

c<-ggplot(p.df,aes(x=reorder(method,sensitivity,median),
  y=NMI,fill=method))+
  geom_boxplot(
    ggplot2::aes(fill = method),
    outlier.alpha = 0.0,
    na.rm = TRUE,
    alpha = .5,
    show.legend = FALSE
  ) +
  geom_point(
    ggplot2::aes(fill = method),
    alpha = 0.75 ,
    size = 0.75,
    shape = 21,
    stroke = 0.15,
    color = 'black',
    position = ggplot2::position_jitterdodge()
  ) +
  scale_fill_brewer(palette = "Spectral", direction=-1)+
  theme_bw()+ggtitle("c, Normalized Mutual Information")+labs(x=NULL,y=NULL)+
  omicsEye_theme()+theme(legend.position="none",axis.ticks.x=element_blank(),
                         axis.text.x=element_blank())

#####################
### Best Fmeasure ###
#####################

d<-ggplot(p.df,aes(x=reorder(method,sensitivity,median),
  y=Fmeasure,fill=method))+
  geom_boxplot(
    ggplot2::aes(fill = method),
    outlier.alpha = 0.0,
    na.rm = TRUE,
    alpha = .5,
    show.legend = FALSE
  ) +
  geom_point(
    ggplot2::aes(fill = method),
    alpha = 0.75 ,
    size = 0.75,
    shape = 21,
    stroke = 0.15,
    color = 'black',
    position = ggplot2::position_jitterdodge()
  ) +
  scale_fill_brewer(palette = "Spectral", direction=-1)+
  theme_bw()+ggtitle("d, F Measure")+labs(x=NULL,y=NULL)+
  omicsEye_theme()+theme(legend.position="none",axis.text.x = element_text(size = 7, 
                                                                            vjust = 1, hjust = 1, angle = 45))


############################
### Best Fowlkes.mallows ###
############################

e<-ggplot(p.df,aes(x=reorder(method,sensitivity,median),
  y=fowlkes.mallows,fill=method))+
  geom_boxplot(
    ggplot2::aes(fill = method),
    outlier.alpha = 0.0,
    na.rm = TRUE,
    alpha = .5,
    show.legend = FALSE
  ) +
  geom_point(
    ggplot2::aes(fill = method),
    alpha = 0.75 ,
    size = 0.75,
    shape = 21,
    stroke = 0.15,
    color = 'black',
    position = ggplot2::position_jitterdodge()
  ) +
  scale_fill_brewer(palette = "Spectral", direction=-1)+
  theme_bw()+ggtitle("e, Fowlkes Mallows Index")+labs(x=NULL,y=NULL)+
  omicsEye_theme()+theme(legend.position="none",axis.ticks.x=element_blank(),
                         axis.text.x=element_blank())

#####################
### Best Vmeasure ###
#####################

f<-ggplot(p.df,aes(x=reorder(method,sensitivity,median),
  y=Silhoutte.score,fill=method))+
  geom_boxplot(
    ggplot2::aes(fill = method),
    outlier.alpha = 0.0,
    na.rm = TRUE,
    alpha = .5,
    show.legend = FALSE
  ) +
  geom_point(
    ggplot2::aes(fill = method),
    alpha = 0.75 ,
    size = 0.75,
    shape = 21,
    stroke = 0.15,
    color = 'black',
    position = ggplot2::position_jitterdodge()
  ) +
  scale_fill_brewer(palette = "Spectral", direction=-1)+
  theme_bw()+ggtitle("f, Silhoutte Score")+labs(x=NULL,y=NULL)+
  omicsEye_theme()+theme(legend.position="none",axis.text.x = element_text(size = 7, 
                                                                           vjust = 1, hjust = 1, angle = 45))

#######################
### Combining Plots ###
#######################

acebdf<-arrangeGrob(a,c,e,b,d,f,ncol=3,nrow=2)
ggsave(paste(plotDirectory,"MainPlot_evaluation.png",sep="\\"),acebdf,
       width=7.2,height=3,dpi=350)


