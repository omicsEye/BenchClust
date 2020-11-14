library(RaceID)
m <- data_in #as.data.frame(matrix(rexp(200, rate=.01), ncol=20))
sc <- SCseq(m)
#sc <- filterdata(sc,mintotal=2000)
#fdata <- getfdata(sc)
#sc <- compdist(sc,metric="pearson")
attr(sc, "distances") <- as.matrix(dist(m, method = "euclidean"))
sc <- clustexp(sc)
#plotsaturation(sc,disp=FALSE)
#plotsaturation(sc,disp=TRUE)
#plotjaccard(sc)


#cluster labels
attr(sc,'cluster')$kpart

#number of clusters
attr(sc,'cluster')$clb$nc
