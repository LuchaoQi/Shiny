# R-shiny-clustering Documentation

```
More details can be found in this presentation: 
![](https://luchaoqi.github.io/Shiny_clustering/)
```

[This file](https://raw.githubusercontent.com/LuchaoQi/Shiny_clustering/master/expression.txt) contains pre-normalized expression values for 100 genes over 10 time points. Most genes have a stable background expression level, but some special genes show increased expression over the timecourse and some show decreased expression.

This shiny app https://luchao-qi.shinyapps.io/shiny_clustering/

  - Cluster the genes using K-means clustering
  - Create a heatmap of the expression matrix. Order the genes by cluster, but keep the time points in numerical order.
  - Compare the performance among PCA, T-sne, Umap analysis

# Features

  - Import a txt file and format it to data frame
  - Choose the number of clusters you want

# System / library equirements to run locally

System:
```
Windows/Linux/Mac
```
Library: 
```
library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggplot2)
library(plotly)
library(ggfortify)
library(gridExtra)
library(umap)
library(gplots)
library(scales)
library(Rtsne)
library(reshape)
library(reshape2)
library(reticulate)
library(rlang)
```

# Manuals

> Download sample data

![](https://raw.githubusercontent.com/LuchaoQi/Shiny_clustering/master/figures/1.png)

> Upload gene expression data and format the  matrix

![](https://raw.githubusercontent.com/LuchaoQi/Shiny_clustering/master/figures/2.png)

> Enter the number of clusters

![](https://raw.githubusercontent.com/LuchaoQi/Shiny_clustering/master/figures/3.png)

> Results

![](https://raw.githubusercontent.com/LuchaoQi/Shiny_clustering/master/figures/4.png)
![](https://raw.githubusercontent.com/LuchaoQi/Shiny_clustering/master/figures/5.png)



**Free Software, Hell Yeah!**

*  [github](https://github.com/LuchaoQi/Shiny_clustering)
