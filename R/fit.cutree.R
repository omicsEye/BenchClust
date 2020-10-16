
###############################
### clustering using hclust ### 
###############################

fit.cutree<-function(inputDirectory,outputDirectory)
{
files<-list.files(paste(inputDirectory,"Input","Distance_Data",sep="\\"))
files.truth<-list.files(paste(inputDirectory,"Input","Truth_Data",sep="\\"))
all.sil.scores<-NULL
for(j in 1:length(files))
{
print(paste("simulation data",j,"running",sep=" "))  
dist_mat<-data.frame(read.table(paste(inputDirectory,"Input","Distance_Data",files[j],sep="\\")
                               ,sep="\t",h=T),check.names=F)
truth_mat<-data.frame(read.table(paste(inputDirectory,"Input","Truth_Data",files.truth[j],sep="\\")
                                ,sep="\t",h=T),check.names=F)
members<-names(dist_mat)
dist_mat<-as.dist(dist_mat,diag=T)
hclust_avg<-hclust(dist_mat,method='complete')

##############################
### Creating Output Labels ###
##############################

new.filename<-substr(files[j],1,nchar(files[j])-4)
stringLabel<-c('clusters','features','sample','alpha','sd')
named.files<-str_split(new.filename,pattern="_")[[1]]
names(named.files)<-stringLabel
clusters<-as.numeric(named.files["clusters"])
features<-as.numeric(named.files["features"])
sample<-as.numeric(named.files["sample"])
alpha<-as.numeric(named.files["alpha"])
sd<-as.numeric(named.files["sd"])

##################
### Dendograms ###
##################

list.dendo<-paste(new.filename,".png",sep="")
ifelse(!dir.exists(file.path(outputDirectory,"Dendogram_plot")), dir.create(file.path(outputDirectory, "Dendogram_plot")),FALSE)
outputstring1<-paste(outputDirectory, "Dendogram_plot",list.dendo,sep="\\")
png(file=outputstring1,width=1920,height=864)
par(mfrow=c(2,2))
myplots <- list()
  
#########################################   
### Evaluating Clustering Performance ###
#########################################
    
sil.avg.cut<-NULL
cut.levels<-c(.25,.5,.6,.75)
for(k in 1:length(cut.levels))
{
i<-as.numeric(quantile(hclust_avg$height,cut.levels[k]))
cut_avg<-cutree(hclust_avg,h=i)
cl.mem<-data.frame(cluster=cut_avg,Sample=members)
cl.mem<-cl.mem[order(cl.mem$cluster),]
names<-paste("Cut=",cut.levels[k],sep="")
plot(hclust_avg,main =names,labels=FALSE, xlab = "", sub = "")
mtext(paste("simulated data:",new.filename,sep=" "), 
      side=1,cex=1.5,line=-2,font=1.5,outer = TRUE)
abline(h = i, col = 'darkblue')

########################
### Silhouette Score ###
########################

sil<-silhouette(cut_avg,dist_mat)
sil.sc<-data.frame(score=sil[, "sil_width"])
neg_sil_index <- which(sil.sc < 0)
sil.sc<-sil.sc[!row.names(sil.sc) %in% neg_sil_index,]
sil.sc<-round(mean(sil.sc),2)

########################
### Extrinsic Scores ###
########################

major.clusters<-length(unique(cl.mem[,'cluster']))
Overlap.info<-truth_mat$overlaps[1]
mis_rate<-abs(clusters-major.clusters)/sample
measure.df<-cluster_eval(truth=truth_mat$cluster,predicted=cl.mem$cluster,
                         clusters,major.clusters)
sil.avg.temp<-data.frame(clusters=clusters,features=features,
                         sample=sample,Dist=alpha,SD=sd,
                         cut.level=paste("cut_",cut.levels[k],sep=""),
                         major.clusters=major.clusters,
                         Silhoutte.score=sil.sc,residual=mis_rate)
sil.avg.temp<-data.frame(cbind(sil.avg.temp,measure.df),check.names=F)
sil.avg.cut<-data.frame(rbind(sil.avg.cut,sil.avg.temp),check.names = F)
p<-fviz_silhouette(sil,print.summary=FALSE)+theme_bw()
title<-paste(paste("[Cut=",cut.levels[k],"]",sep=""),
             paste("[Silhoutte.Score=",round(sil.sc,2),"]",sep=""),sep=" ")
p<-ggpar(p,title = title,legend = "none")+theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())
myplots[[k]]<-p
}
dev.off()
all.sil.scores<-data.frame(rbind(all.sil.scores,sil.avg.cut))
myplots<-list.clean(myplots, fun = is.null, recursive = FALSE)
g<-grid.arrange(grobs = myplots,ncol = 4,top = textGrob(paste("simulated data:",new.filename,sep=" "),gp=gpar(fontsize=12,font=2)))
list.gg<-paste("silhoutte.plot_",new.filename,".png",sep="")
ifelse(!dir.exists(file.path(outputDirectory, "Silhoutte_plot")), dir.create(file.path(outputDirectory, "Silhoutte_plot")),FALSE)
outputstring2<-paste(outputDirectory, "Silhoutte_plot",list.gg,sep="\\")
suppressMessages(ggsave(outputstring2,g,width=20,height=7,dpi=300))
try(dev.off(dev.list()["RStudioGD"]),silent=TRUE)
}
return(all.sil.scores)
}


