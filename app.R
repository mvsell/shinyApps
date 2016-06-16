# App Name: OGWI_toolV1
# Description: DH Interval grade thickness calculator
# Author(s): Maiko Sell, Curtis Brett
# Date:June 15, 2016


# Set wd
#setwd("C:/Data/SCRIPTS/R/Uncertainty/MCSims/simsShinyApp")

# Set libraries
library(shiny)
library(rriskDistributions)
library(ggplot2)
library(dplyr)
library(reshape2)
library(shinyjs)

# read data
# df1 <- read.csv("C:/Data/SCRIPTS/R/OGWI_toolV1/DH_interval/OGWI_ToolV1/Data/Au_Interval.csv", header = T)
# saveRDS(df1,"C:/Data/SCRIPTS/R/OGWI_toolV1/DH_interval/OGWI_ToolV1/Data/Au_Interval.Rds")

# Global variables can go here

df1 <- readRDS('data/Au_Interval.Rds')
df1[,4] <- log10(df1[, 4])

# JScode <-
#   "$(function() {
# setTimeout(function(){
# var vals = [0];
# var powStart = -2;
# var powStop = 2;
# for (i = powStart; i <= powStop; i++) {
# var val = Math.pow(10, i);
# val = parseFloat(val.toFixed(8));
# vals.push(val);
# }
# $('#slider1').data('ionRangeSlider').update({'values':vals})
# }, 5)})"

ui <-
  shinyUI(fluidPage(
    
    tabsetPanel(
    tabPanel("OGWI Tool v1.0", " ",
             fluidRow(
               titlePanel(h2("Grade Thickness Calculator"), br()),
               column(width = 2,
                      " ", br(),
                      fileInput('datafile', 'Choose CSV file',
                                accept=c('text/csv', 'text/comma-separated-values,text/plain')),
                      uiOutput("selectcol1"),
                      uiOutput("selectcol2"),
                      #tags$head(tags$script(HTML(JScode))),
                      sliderInput(
                        "slider1",
                        "Threshold Grades",
                        min = -2.1,
                        max = 2.1,
                        value = c(-0.1, 1.1)
                      )
               ),
               
               column(width = 4,
                      h4("Data Input Format"),br(),
                      dataTableOutput('filedata')),
               column(width = 6,
                      " ",
                      fluidRow(
                        column(width =12,
                               plotOutput("plot6", height = "300px", width = "450px"))),
                      fluidRow(
                        column(width =12,
                               plotOutput("plot7", height = "300px", width = "400px")))
             )
             ))
  )))
server <- function(input, output) {
  filedata <- reactive({
    infile <- input$datafile
    if (is.null(infile)) {
      return(df1) # this returns the default .Rds - see global variables
      #return(NULL)
    }
    temp<-read.csv(infile$datapath)
    #return
    temp[order(temp[, 1]),]
    
  })
  
  
  x <- reactive({
    1:dim(filedata())[1]
  })
  
  output$selectcol1 <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    
    items=names(df)
    names(items)=items
    selectInput("selectcol1", "From",items)
    
  })
  
  output$selectcol2 <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    
    items=names(df)
    names(items)=items
    selectInput("selectcol2", "Assay",items, selected = "Au_ppm")
  })
  
  output$filedata = renderDataTable({
    filedata()
  })
  
  
  output$mytable = renderTable({
    #beans
    filedata()$infile
    
  })
  
  
  output$plot6 <- renderPlot({
    ggplot(filedata()) +
      geom_line(aes(y= 10^filedata()[[input$selectcol2]], x= filedata()[[input$selectcol1]])) +
      geom_line(aes(y = -2, x= filedata()[[input$selectcol1]], colour = ( filedata()[[input$selectcol2]])), size=2) +
      coord_flip() +
      scale_x_reverse() +
      scale_colour_gradientn("Grade", colours = rev(rainbow(5))) +
      labs(title ="DH Grade", x = "Depth (m)", y = "Assay")
    })
  
  output$plot7 <- renderPlot({
    ggplot(filedata(), aes(sample = filedata()[[input$selectcol2]])) +
      stat_qq() +
      #scale_y_log10() +
      geom_hline(aes(yintercept=input$slider1[1])) + # cutoff
      geom_hline(aes(yintercept=input$slider1[2])) + # top cut
      labs(title ="Q-Q Plot", x = "Normal Theoretical Quantiles", y = "Normal Data Quantiles")
      })
  
  }

shinyApp(ui = ui, server = server)