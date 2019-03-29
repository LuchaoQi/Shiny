library(shiny)
library(shinydashboard)
library(ggfortify)
library(plotly)


header <- dashboardHeader(title = "Uploading Files")
sidebar <- dashboardSidebar(
  fileInput("file1", "Choose txt File: expression level matrix",
            multiple = TRUE,
            accept = c("text/comma-separated-values,text/plain",
                       "text/txt",
                       ".txt")),
  checkboxInput("header", "Header", TRUE),
  radioButtons("sep", "Separator",
               choices = c(Comma = ",",
                           Semicolon = ";",
                           Tab = "\t"),
               selected = "\t"),

  radioButtons("quote", "Quote",
               choices = c(None = "",
                           "Double Quote" = '"',
                           "Single Quote" = "'"),
               selected = '"'),


  radioButtons("disp", "Display",
               choices = c(Head = "head",
                           All = "all"),
               selected = "head"),
  
  tags$hr(), #horizontal line
  selectInput('algo','Algorithm',choices = list('kmeans' = 'km','partitioning around medoids' = 'pam',
                                                'hierachical clustering' = 'hc','cmeans' = 'cm'),selected = 'cm'),
  # textInput("algo","Algorithm:Options are 'km' (kmeans), 'pam' (partitioning around medoids), 
  #           'hc' (hierachical clustering), 'cm' (cmeans).",value = "cm"),
  numericInput('k',"Number of clusters",value = 3)
  # numericInput('iter.max','Iteration',value = 20)
  
  
)


body <- dashboardBody(tableOutput("contents"),
                      plotOutput("clustering"),
                      plotlyOutput("pca")
                      )




ui <- dashboardPage(header, sidebar, body)
