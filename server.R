library(shiny)
library(shinydashboard)
# library(caret)
library(tidyverse)
# library(TCseq)
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
# library(data.table)

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
    dat = cbind(row.names(df()),df()) %>% `colnames<-`(c('genes',colnames(df())))
    
    if(input$disp == "head") {
      return(head(dat))
    }
    else {
      return(dat)
    }
  })
  
  # df2 = reactive({
  #   req(input$file1)
  #   dat <- read.table(input$file1$datapath,
  #                     header = input$header,
  #                     sep = input$sep,
  #                     quote = input$quote)
  #   return(dat)
  # })
  
  #heatmap
  # ord <- reactive({hclust( dist(scale(df()), method = "euclidean"), method = "ward.D" )$order})
  dat_heatmap = reactive({ 
    dat = cbind(df(),kmeans(df(),input$k)$cluster)
    # row.names(dat) = dat[,1]
    # dat = dat[-1]
    # dat = melt(read.table(input$file1$datapath,header = 1)[ord(),])
    colnames(dat)[dim(dat)[2]] = 'Cluster'
    dat = dat[order(dat$'Cluster'),][-dim(dat)[2]]
    # dat = gather(df2(),experiments,expression,2:length(df2()[1,]))
    dat = reshape2::melt(as.matrix(dat))
    
    # dat = melt(setDT(dat, keep.rownames = TRUE), "rn")
    # dat$rn = factor(dat$rn,levels = dat$rn)
    return(dat)
    # return(melt(as.matrix(dat)))
    # [,c('X2','X1','value')]
  })
  
  
  output$heatmap = renderPlot({
    if(is.null(input$file1)){return()}
    
    ggplot( dat_heatmap(), aes(x = Var2,y = factor(Var1,levels = unique(Var1))) )+
      geom_tile(aes(fill = value))+
      scale_fill_gradient(low="grey90", high="red") +
      labs(x= 'exp',y = 'gene')+
      theme(axis.text.y = element_text(size = 6))
  })
  
  # 
  # output$heatmap = renderPlot({
  #   if(is.null(input$file1)){return()}
  # 
  #   ggplot( dat_heatmap(), aes(x = variable,y = factor(rn,levels = unique(rn))) )+
  #     geom_tile(aes(fill = value))+
  #     scale_fill_gradient(low="grey90", high="red") +
  #     labs(x= 'exp',y = 'gene')+
  #     theme(axis.text.y = element_text(size = 6))
  # })
  # 
  # dat = reactive({
  #   dat = timeclust(as.matrix(df()),
  #                   algo = input$algo,
  #                   k = input$k,
  #                   standardize = TRUE)
  #                   # iter.max=input$iter.max)
  #   return(dat)
  # })
  
  # output$clustering = renderPlot({
  #   if(is.null(input$file1)){return()}
  #   timeclustplot(dat(),categories = "time points",col =1,axis.text.size = 11)[0]
  # })
  
  dat_pca_result = reactive({
    dat = merge(as.data.frame(prcomp(df())$x),as.data.frame(kmeans(df(),input$k)$cluster),by ='row.names',all.x=T)
    colnames(dat)[dim(dat)[2]] = 'Cluster'
    colnames(dat)[1] = 'genes'
    dat = dat[order(dat$Cluster),]
    return(dat)
  })
  
  
  # output$pca <- renderPlotly({
  #   ggplotly(ggplot(dat_pca(),aes(x=PC1,y=PC2,color= Cluster))+
  #              geom_point()) 
  #     
  # })
  
  output$threed_clustering <- renderPlotly({
    plot_ly(dat_pca_result(), x = ~PC1, y = ~PC2, z = ~PC3, color = ~Cluster,type ='scatter3d',mode = 'markers', text = ~genes, hoverinfo='text') 
    # text = ~genes
      # %>% add_markers() %>%
      # layout(scene = list(xaxis = list(title = 'PC1'),
      #                     yaxis = list(title = 'PC2'),
      #                     zaxis = list(title = 'PC3')))
      
  })
  
  

  # compare
  # beginning <- reactive({Sys.time()})
  pca = reactive({prcomp(t(df()))})
  # end <- reactive({Sys.time()})
  # t_prcomp <- reactive({as.numeric(beginning() - end())})
  
  # beginning <- reactive({Sys.time()})
  tsne = reactive({Rtsne(df())})
  # end <- reactive({Sys.time()})
  # t_tsne <- reactive({as.numeric(beginning() - end())})
  
  # beginning <- reactive({Sys.time()})
  Umap = reactive({umap(as.matrix(df()))})
  # end <- reactive({Sys.time()})
  # t_umap <- reactive({as.numeric(beginning() - end())})
  
  output$cluster_compare = renderPlot({grid.arrange(
    ggplot(as.data.frame(pca()$rotation),aes(x=PC1,y=PC2))+
      geom_point()+labs(title = 'pca'),
    ggplot(as.data.frame(tsne()$Y),aes(x=V1,y=V2))+
      geom_point()+labs(x= 'tsne 1', y = 'tsne 2', title = 'tsne'),
    ggplot(as.data.frame(Umap()$layout),aes(x = V1,y=V2))+
      geom_point()+labs(x= 'umap 1', y = 'umap 2', title = 'umap'),

    # ggplot( dat_heatmap(), aes(variable, name) ) +
    #   geom_tile(aes(fill = value)),
    # scale_fill_gradient2(low=muted("blue"), high=muted("red")),
  
    

    # ggplot(as.data.frame(rbind(t_prcomp(),t_tsne(),t_umap())),aes(x = c('PCA','tsne','Umap'), y = V1)) +
    #   geom_col() + labs(x = 'Algorithms', y = 'Running time'),
    
    
    ncol = 2,
    nrow = 2 
  )
  })
}






