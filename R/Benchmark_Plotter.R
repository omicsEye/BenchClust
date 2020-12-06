#########################
### Loading Libraries ###
#########################

suppressPackageStartupMessages(library(ggplot2)) # ggplot2 function
suppressPackageStartupMessages(library(data.table)) # fread function
suppressPackageStartupMessages(library(RColorBrewer)) # brewer.pal function
suppressPackageStartupMessages(library(gridExtra)) # arrangeGrob function
suppressPackageStartupMessages(library(deepath))
workDirectory<-"D:/Repos/omeClust_analysis/R"

##########################
### Output for Figures ###
##########################

outputDirectory<-paste(workDirectory,"Plots","Figures",sep='/')
if (!dir.exists(outputDirectory))
{
  dir.create(outputDirectory)
}

#####################
### Input Results ###
#####################

Files<-list.files(paste(workDirectory,"Outputs",sep='/'),".csv")
DF<-lapply(paste(workDirectory,"Outputs",Files,sep='/'),fread,h=T)
comb.DF<-do.call(rbind,DF)
comb.DF<-comb.DF[!(comb.DF$Method %in% c("omeClust.centroid","omeClust.average")),]
comb.DF[comb.DF=="omeClust.complete"]<-"omeClust"

###########################
### Adjusted Rand Index ###
###########################

a<-ggplot(comb.DF,aes(x=reorder(Method,ARI,median),
                   y=ARI,fill=reorder(Method,ARI,median)))+geom_boxplot(
                     ggplot2::aes(fill = reorder(Method,ARI,median)),
                     outlier.alpha = 0.0,
                     na.rm = TRUE,
                     alpha = .5,
                     show.legend = FALSE
                   ) +
  geom_point(
    ggplot2::aes(fill = reorder(Method,ARI,median)),
    alpha = 0.75 ,
    size = 0.75,
    shape = 21,
    stroke = 0.15,
    color = 'black',
    position = ggplot2::position_jitterdodge()
  ) +
  scale_fill_brewer(palette = "Spectral", direction=-1)+
  theme_bw()+ggtitle("a, Adjusted Rand Index")+labs(x=NULL,y=NULL)+
  theme_omicsEye()+theme(legend.position="none",axis.ticks.x=element_blank(),
                         axis.text.x=element_blank())


#####################
### Jaccard Index ###
#####################

b<-ggplot(comb.DF,aes(x=reorder(Method,ARI,median),
                      y=Jaccard,fill=reorder(Method,ARI,median)))+geom_boxplot(
                        ggplot2::aes(fill = reorder(Method,ARI,median)),
                        outlier.alpha = 0.0,
                        na.rm = TRUE,
                        alpha = .5,
                        show.legend = FALSE
                      ) +
  geom_point(
    ggplot2::aes(fill = reorder(Method,ARI,median)),
    alpha = 0.75 ,
    size = 0.75,
    shape = 21,
    stroke = 0.15,
    color = 'black',
    position = ggplot2::position_jitterdodge()
  ) +
  scale_fill_brewer(palette = "Spectral", direction=-1)+
  theme_bw()+ggtitle("b, Jaccard Index")+labs(x=NULL,y=NULL)+
  theme_omicsEye()+theme(legend.position="none",axis.ticks.x=element_blank(),
                         axis.text.x=element_blank())

#######################
### Fowlkes_mallows ###
#######################

c<-ggplot(comb.DF,aes(x=reorder(Method,ARI,median),y=Fowlkes_mallows,fill=reorder(Method,ARI,median)))+
  geom_boxplot(
    ggplot2::aes(fill = reorder(Method,ARI,median)),
    outlier.alpha = 0.0,
    na.rm = TRUE,
    alpha = .5,
    show.legend = FALSE
  ) +
  geom_point(
    ggplot2::aes(fill = reorder(Method,ARI,median)),
    alpha = 0.75 ,
    size = 0.75,
    shape = 21,
    stroke = 0.15,
    color = 'black',
    position = ggplot2::position_jitterdodge()
  ) +
  scale_fill_brewer(palette = "Spectral", direction=-1)+
  theme_bw()+ggtitle("c, Fowlkes_mallows")+labs(x=NULL,y=NULL)+
  theme_omicsEye()+theme(legend.position="none",axis.text.x = element_text(size = 7,vjust = 1, hjust = 1, angle = 45))

#################################
### Checking F1 score Overall ###
#################################

d<-ggplot(comb.DF,aes(x=reorder(Method,ARI,median),y=F1_score,fill = reorder(Method,ARI,median)))+
  geom_boxplot(
    ggplot2::aes(fill = reorder(Method,ARI,median)),
    outlier.alpha = 0.0,
    na.rm = TRUE,
    alpha = .5,
    show.legend = FALSE
  ) +
  geom_point(
    ggplot2::aes(fill = reorder(Method,ARI,median)),
    alpha = 0.75 ,
    size = 0.75,
    shape = 21,
    stroke = 0.15,
    color = 'black',
    position = ggplot2::position_jitterdodge()
  ) +
  ggplot2::scale_fill_brewer(palette = "Spectral", direction=-1)+
  theme_bw()+ggtitle("d, F1 Score")+labs(x=NULL,y=NULL)+
  theme_omicsEye()+theme(legend.position="none",axis.text.x = element_text(size = 7,vjust = 1, hjust = 1, angle = 45))

####################################                   
### Creating Manuscript Figure:1 ###
####################################

width=7.2
height=3
dpi=350
outlabel<-paste("Figure3",sep="_")

#######################
### Combining Plots ###
#######################

outputString<-paste(outputDirectory,paste(outlabel,'.pdf',sep=''),sep='/')
e<-arrangeGrob(a,b,c,d,nrow=2,ncol=2)
ggsave(outputString,e,width=width,height=height,dpi=dpi)        
        
  
  
                                                                       
                           
                           
                           
                           
                         
  
    
    
    
    
    
    
    
  
  
  
