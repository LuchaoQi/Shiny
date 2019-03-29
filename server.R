library(shiny)
library(shinydashboard)
library(caret)
library(tidyverse)
library(TCseq)
library(ggplot2)
library(plotly)
library(ggfortify)

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
  
  dat_pca = reactive({
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
  
  output$pca <- renderPlotly({
    plot_ly(x = dat_pca()$PC1, y = dat_pca()$PC2, z = dat_pca()$PC3, type="scatter3d", mode="markers", color= dat_pca()$Cluster)
  })
}






