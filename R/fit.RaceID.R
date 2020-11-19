
fit.RaceID<-function(inputDirectory,outputDirectory)
{
  library(RaceID)
  library(Hmisc)
  
  ##################
  ### Input Data ###
  ##################
  
  files<-list.files(paste(inputDirectory,"Input","Distance_Data",sep="\\"))
  files.truth<-list.files(paste(inputDirectory,"Input","Truth_Data",sep="\\"))
  
  ###############################
  ### Getting Cluster Members ###
  ###############################
  
  final.output<-NULL
  for(j in 1:length(files))
  {
    print(paste("simulation data",j,"running",sep=" "))  
    dist_mat<-data.frame(read.table(paste(inputDirectory,"Input","Distance_Data",files[j],sep="\\")
                                    ,sep="\t"),check.names=F)
    truth_mat<-data.frame(read.table(paste(inputDirectory,"Input","Truth_Data",files.truth[j],sep="\\")
                                     ,sep="\t",header=T),check.names=F)
    cor_mat<-data.frame(read.table(paste(inputDirectory,"Input","Corr_Data",files[j],sep="\\")
                                   ,sep="\t"),check.names=F)
    input.data<-as.dist(dist_mat,diag=T)
    dist_mat<-as.matrix(dist_mat)
    sc <- SCseq(abs(m))
    
    x_corr <- rcorr(as.matrix(m), type="pearson")
    attr(sc, "distances") <- as.matrix(dist(m, method = "euclidean"))
    #attr(sc, "distances") <-as.matrix(vegdist(m, method="bray"))
    attr(sc, "distances") <- 1.0 - x_corr$r
    sc <- clustexp(sc, clustnr = 10)
    member.df <- data.frame(attr(sc,'cluster')$kpart)
    member.df$members <- rownames(member.df)
    colnames(member.df) <- c("Clusters", "members")
    
    #number of clusters
    nc <- attr(sc,'cluster')$clb$nc
    
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
    pdf(paste(outputDirectory,paste(new.filename,".pdf",sep=""),
              sep="\\"),width=12,height=8)
    plotsaturation(sc,disp=TRUE)
    title<-paste("Samples=",sample,",Features=",features,",Clusters=",clusters,",
             Dist=",alpha,sep="")
    
    dev.off()
    
    #############################
    ### Measuring Performance ###
    #############################
    
    sil.sc<-silhouette(member.df$members,input.data)
    if(is.na(sil.sc)){
      sil.sc==0}else{
        sil.sc<-data.frame(score=sil.sc[, "sil_width"])
        neg_sil_index <- which(sil.sc < 0)
        sil.sc<-sil.sc[!row.names(sil.sc) %in% neg_sil_index,]
        sil.sc<-round(mean(sil.sc),2)
      }
    major.clusters<-length(unique(member.df$members))
    measure.df<-cluster_eval(truth=truth_mat$cluster,
                             predicted=member.df$members,
                             clusters,major.clusters)
    
    ##########################
    ### Organizing Outputs ###
    ##########################
    
    mis_rate<-abs(clusters-major.clusters)/sample
    output.df<-data.frame(clusters=clusters,features=features,
                          sample=sample,Dist=alpha,SD=sd,
                          method="RaceID",
                          major.clusters=major.clusters,
                          Silhoutte.score=sil.sc,residual=mis_rate)
    output.df<-data.frame(cbind(output.df,measure.df),check.names=F)
    final.output<-data.frame(rbind(final.output,output.df))
  }
  return(final.output)
}



