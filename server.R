library(shiny)
library(shinydashboard)
library(caret)
library(tidyverse)
library(TCseq)
library(ggplot2)
library(plotly)
library(ggfortify)
library(gridExtra)
library(umap)
library(gplots)
library(scales)
library(Rtsne)
library(reshape)

server <- function(input, output) {
  
  
  df = reactive({
    req(input$file1)
    dat <- read.table(input$file1$datapath,
                     header = input$header,
                     sep = input$sep,
                     quote = input$quote,
                     row.names = 1)
    return(dat)
  })
  
  # df = df[,is.na(colSums(df=='Error'))] %>% na.omit(df) #remove columns with 'Error' and rows with NA
  
  output$contents <- renderTable({
    
    if(input$disp == "head") {
      return(head(df()))
    }
    else {
      return(df())
    }
  })
  
  
  
  dat = reactive({
    dat = timeclust(as.matrix(df()),
                    algo = input$algo,
                    k = input$k,
                    standardize = TRUE)
                    # iter.max=input$iter.max)
    return(dat)
  })
  
  output$clustering = renderPlot({
    if(is.null(input$file1)){return()}
    timeclustplot(dat(),categories = "time points",col =1,axis.text.size = 11)[0]
  })
  
  dat_cluster_result = reactive({
    dat = merge(as.data.frame(prcomp(df())$x),as.data.frame(dat()@cluster),by ='row.names',all.x=T)
    colnames(dat)[dim(dat)[2]] = 'Cluster'
    dat = dat[order(dat$Cluster),]
    return(dat)
  })
  
  
  # output$pca <- renderPlotly({
  #   ggplotly(ggplot(dat_pca(),aes(x=PC1,y=PC2,color= Cluster))+
  #              geom_point()) 
  #     
  # })
  
  output$threed_clustering <- renderPlotly({
    plot_ly(x = dat_cluster_result()$PC1, y = dat_cluster_result()$PC2, z = dat_cluster_result()$PC3, type="scatter3d", mode="markers", color= dat_cluster_result()$Cluster)
  })
  
  
  #heatmap
  ord <- reactive({hclust( dist(scale(df()), method = "euclidean"), method = "ward.D" )$order})
  dat_heatmap = reactive({ 
    dat = melt(read.table(input$file1$datapath,header = 1)[ord(),])
    return(dat)
  })
  # compare
  
  pca = reactive({prcomp(t(df()))})
  tsne = reactive({Rtsne(df())})
  Umap = reactive({umap(as.matrix(df()))})
  
  output$cluster_compare = renderPlot({grid.arrange(
    ggplot(as.data.frame(pca()$rotation),aes(x=PC1,y=PC2))+
      geom_point()+labs(title = 'pca'),
    ggplot(as.data.frame(tsne()$Y),aes(x=V1,y=V2))+
      geom_point()+labs(x= 'tsne 1', y = 'tsne 2', title = 'tsne'),
    ggplot(as.data.frame(Umap()$layout),aes(x = V1,y=V2))+
      geom_point()+labs(x= 'umap 1', y = 'umap 2', title = 'umap'),
  
    ggplot( dat_heatmap(), aes(variable, name) ) +
      geom_tile(aes(fill = value)),
      # scale_fill_gradient2(low=muted("blue"), high=muted("red")),
  
    nrow =2 
  )
  })
}






