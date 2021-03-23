# BenchClust

This repository includes benchmarking for clustering and community detection techniques.


* Summary of domain-agnostic clustering techniques. Method names along with their corresponding algorithmic details and computing platforms are provided.


Method Type                            |  Algorithm     |  Library (function) [platform]
----------------------------------------|----------------|-------------------------------
Partitional                             |  k-means       |  stats (k-means) [R]
Linkage                                 |  Hierarchical  |  cluster (agnes) [R]
Model-based: Gaussian Mixtures          |  Hcmodel       |  mclust (hc) [R]
Density-based                           |  DBSCAN        |  dbscan (dbscan) [R]
Subspace-based                          |  Hddc          |  HDclassif (hddc) [R]
Graph-based: Community Detection        |  Louvain       |  igraph(louvain) [R]
Graph-based: Community Detection        |  Infomap       |  igraph(louvain) [R]
Graph-based: Shared nearest neighbor    |  SNNclust      |  dbscan (sNNclust) [R]

* A summary of domain-specific clustering techniques for single-cell RNA sequencing data. Method names along with their corresponding algorithmic details and computing platforms are provided.


Method    |  Type               |  Algorithm                     |  Platform
-----------|---------------------|--------------------------------|----------
Seurat     |  graph-based        |  PCA + KNN + Louivan           |  R
Monocle3   |  graph-based        |  PCA + KNN + Leiden            |  R
CIDR       |  Linkage            |  PCA + hierarchical            |  R
Sincell    |  Partition          |  K-mediods                     |  R
pcaReduce  |  Linkage/Partition  |  PCA + k-means + hierarchical  |  R
SC3        |  Partition          |  PCA + k-means                 |  R
RaceID3    |  Partition          |  k-means                       |  R
sscClust   |  graph-based        |  Shared nearest neighbor       |  R

* Summary of benchmark datasets with ground truth information used for evaluating single-cell clustering techniques (obtained from https://hemberg-lab.github.io/scRNA.seq.datasets/). 

Data Set  |  Year  |  Technology   |  Data type    |  Number of Cells  |  Cell Populations  |  Features  |  PMID
-----------|--------|---------------|---------------|-------------------|--------------------|------------|----------
Baron      |  2016  |  Droplet      |  UMI counts   |  1886             |  13                |  14861     |  27667365
Darmanis   |  2015  |  SMARTer      |  Read counts  |  466              |  9                 |  21630     |  26060301
Deng       |  2014  |  Smart-Seq    |  Read counts  |  268              |  6                 |  21297     |  24408435
Goolam     |  2016  |  Smart-Seq2   |  Read counts  |  124              |  4                 |  28147     |  27015307
Klein      |  2015  |  Droplet      |  UMI counts   |  2717             |  4                 |  24047     |  26000487
Li         |  2017  |  SMARTer      |  Read counts  |  561              |  9                 |  43055     |  28319088
Romanov    |  2016  |  Fluidigm C1  |  Read counts  |  2881             |  7                 |  21143     |  27991900
Zeisel     |  2015  |  C1 UMI       |  UMI counts   |  3005             |  9                 |  19972     |  25700174
