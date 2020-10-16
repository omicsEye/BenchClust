cluster_eval<-function(truth,predicted,clusters,major.clusters)
{
library(ClusterR)
library(sabre)
truth<-as.numeric(truth)
predicted<-as.numeric(predicted)
measures<-capture.output(external_validation(truth,predicted,
                  method="var_info",summary_stats=T))
NMI<-as.numeric(str_split(measures[5],pattern = ":")[[1]][2])
specificity<-as.numeric(str_split(measures[9],pattern = ":")[[1]][2])
sensitivity<-as.numeric(str_split(measures[10],pattern = ":")[[1]][2])
precision<-as.numeric(str_split(measures[11],pattern = ":")[[1]][2])
Fmeasure<-as.numeric(str_split(measures[13],pattern = ":")[[1]][2])
Accuracy<-as.numeric(str_split(measures[15],pattern = ":")[[1]][2])
fowlkes.mallows<-as.numeric(str_split(measures[18],pattern = ":")[[1]][2])                         
Vmeasure<-vmeasure(truth,predicted,z=NULL,B=1)$v_measure
FDR<-1-precision
FPR<-1-specificity
measures.df<-data.frame(specificity=specificity,sensitivity=sensitivity,
                        precision=precision,Fmeasure=Fmeasure,NMI=NMI,
                        Accuracy=Accuracy,fowlkes.mallows=fowlkes.mallows,
                        Vmeasure=Vmeasure,FDR=FDR,FPR=FPR)
                        
return(measures.df)
}