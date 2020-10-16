fit.m2clust.eval<-function(resolution,workingDirectory)
{
  
  #########################
  ### Loading Libraries ###
  #########################
  
  library(data.table)
  library(cluster)
  
  ##########################  
  ### Locating Directory ###
  ##########################
  
  if(resolution=="low")
  {
    folder="Low_resolution" 
  }
  if(resolution=="medium")
  {
    folder="Medium_resolution" 
  }
  if(resolution=="high")
  {
    folder="High_resolution" 
  }
  
  #########################
  ### Getting Data Path ###
  #########################
  
  data.path<-paste(workingDirectory,"Input","Distance_Data",sep="\\")
  truth.path<-paste(workingDirectory,"Input","Truth_meta",sep="\\")
  m2clust.path<-paste(workingDirectory,"m2clust",folder,sep="\\")
  
  ##############################
  ### Getting DataNames List ###
  ##############################
  
  data.list<-list.files(data.path)
  truth.list<-list.files(truth.path)
  m2clust.list<-list.files(m2clust.path)
  
  final.output<-NULL
  for(i in 1:length(data.list))
  {
    
    ####################
    ### Loading data ###  
    ####################
    
    input.data<-data.frame(read.table(file.path(data.path,data.list[i])))
    input.data<-as.dist(input.data,diag=T)
    input.truth<-data.frame(read.table(file.path(truth.path,truth.list[i])))
    input.m2clust<-data.frame(read.table(file.path(m2clust.path,m2clust.list[i],
                                                   "clusters.txt"),h=T))
    
    ################################
    ### Getting m2clust clusters ###
    ################################
    
    mem.mat<-NULL
    for (j in 1:nrow(input.m2clust))
    {
      temp<-input.m2clust[j,]
      cl<-str_sub(temp$cluster,2,str_length(temp$cluster))
      mem<-unlist(strsplit(as.character(temp$members), ";"))
      temp1<-data.frame(cbind(members=mem,cluster=rep(cl,length(mem))))
      mem.mat<-data.frame(rbind(mem.mat,temp1))
    }
    mem.mat<-mem.mat[order(as.numeric(mem.mat$cluster)),]
    
    #############################
    ### Creating OutputLabels ###
    #############################
    
    new.filename<-substr(m2clust.list[i],1,nchar(m2clust.list[i])-4)
    stringLabel<-c('clusters','features','sample','alpha','sd')
    named.files<-str_split(new.filename,pattern="_")[[1]]
    names(named.files)<-stringLabel
    clusters<-as.numeric(named.files["clusters"])
    features<-as.numeric(named.files["features"])
    sample<-as.numeric(named.files["sample"])
    alpha<-as.numeric(named.files["alpha"])
    sd<-as.numeric(named.files["sd"])
    
    #############################
    ### Measuring Performance ###
    #############################
    
    sil.sc<-silhouette(unname(sapply(mem.mat$cluster,as.numeric)),input.data)
    sil.sc<-round(mean(sil.sc[,"sil_width"]),2)
    major.clusters<-nrow(input.m2clust[input.m2clust$resolution_score>=0.10,])
    measure.df<-cluster_eval(truth=input.truth$Ground.Truth,
                             predicted=mem.mat$cluster,
                             nclusters,major.clusters)
    
    ##########################
    ### Organizing Outputs ###
    ##########################
    
    size<-length(unique(mem.mat$cluster))
    mis_rate<-abs(clusters-size)/sample
    output.df<-data.frame(clusters=clusters,features=features,
                          sample=sample,Dist=alpha,SD=sd,
                          method="m2clust",
                          major.clusters=major.clusters,
                          Silhoutte.score=sil.sc,residual=mis_rate)
    output.df<-data.frame(cbind(output.df,measure.df),check.names=F)
    final.output<-data.frame(rbind(final.output,output.df))
  }
  return(final.output)
}
