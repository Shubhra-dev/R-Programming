options(shiny.maxRequestSize=10*1024^2)

library(shiny)
library(openxlsx)
library(tidyverse)
library(gridExtra)
library(lubridate)
library(RMySQL)

mydb <- dbConnect(MySQL(),
                  user = 'root',
                  password = '',
                  dbname = 'test',
                  port = 3306)
dbListTables(mydb)

shinyServer(function(input,output,session){
  startTime <- Sys.time()
  #declaring reactive values for generating pdf report of the output
  
  readingFiles<- function(fileName)({
    
    fLine <- readLines(fileName, n = 1)
    print(fLine)
    if(grepl(".xlsx",fileName) || grepl(".xls",fileName)){
      
      datf <- read.xlsx(fileName,colNames = TRUE)
      datf <- as.tibble(datf)
      print(datf)
      return(datf)
    }
    
    #read .tsv files
    else if(grepl(".tsv",fileName)){
      datf <- read_tsv(fileName,col_types =  cols(.default = col_character()))
      return(datf)
    }
    
    #read csv files with semicolon separator 
    else if(grepl(".csv",fileName)){
      
      if (grepl(";", fLine)){
        
        datf <- read_csv2(fileName,col_types =  cols(.default = col_character()))
        
      } 
      else{
        datf <- read_csv(fileName,col_types =  cols(.default = col_character()))
      }
      
      return(datf)
    }
    
    #read txt and dat files
    else if(grepl(".txt",fileName) || grepl(".dat",fileName)){
      
      if (grepl("\\|", fLine)){
        print("|")
        datf <- read_delim(fileName,delim = "|",col_types =  cols(.default = col_character()))
        
      } 
      else{
        print("Space")
        datf <- read_delim(fileName,delim = " ",col_types =  cols(.default = col_character()))
      }
      
      return(datf)
    }
    
  })
  
  getDataFrame <- reactive({
    
    validate(
      need(input$file != "" || input$link != "", "Please select a data set")
    )
    
    file_in <- input$file
    dPath <- file_in$datapath 
    
    upLink <- input$link
    print(upLink)
    
    if(upLink != ""){
      
      if(grepl("dl=0",upLink)){
        upLink <- gsub("dl=0","dl=1",upLink)
      }
      
      print(upLink)
      datf <- readingFiles(upLink)
      validate(
        need(datf != "", "Uploaded file/Link is not working or Permission is denied.")
      )
      return(datf)
    }
    else{
      datf <- readingFiles(dPath)
      return(datf)
    }
  })
  
  createPlot <- function(a,b){
    return(qplot(a,b))
  }
  
  output$plot <- renderUI({
    
    x <- dbSendQuery(mydb, "SELECT numOfLogin FROM loginfo where username = 'user1'");
    data <- fetch(x,n=1)
    if(data$numOfLogin < 5) {
    if(!(is.null(getDataFrame()))){
    tagList(
    renderPlot({
        datf <- getDataFrame()
        createPlot(datf[[1]],datf[[2]])
      }),
    downloadButton('export1',"Download Report"),
    renderPlot({
      #saving the plot in a reactive value
      datf <- getDataFrame()
      createPlot(datf[[1]],datf[[3]])
    }),
    downloadButton('export2',"Download Report")
    )
    }
    }
    else{
      renderText({ "Login Limit Exceeded." })
    }
  })
  
  pdfTitle<- as.character(format(Sys.Date(),"%A"))
  #Report generator
  output$export1 <- downloadHandler(
    #filename is a default argument of downloadHandler function. A string of the filename, including extension or a function that returns such a string..
    filename <- function() {
      paste("report-", Sys.Date(), ".pdf")
    },
    #content is a also default argument of downloadHandler function. A function that takes a single argument "file" that is a file path (string) of a nonexistent temp file, and writes the content to that file path. 
    content <- function(file) {
      datf <- getDataFrame()
      pdf(file,title = pdfTitle,paper = "a4") # open the pdf device
      grid.table(datf)
      p1 = qplot(datf[[1]],datf[[2]])
      arrangeGrob(print(p1), ncol = 1, main = "Main title")
      dev.off()  # turn the device off
    }
  )
  output$export2 <- downloadHandler(
    #filename is a default argument of downloadHandler function. A string of the filename, including extension or a function that returns such a string..
    filename <- function() {
      paste("report-", Sys.Date(), ".pdf")
    },
       content <- function(file) {
      datf <- getDataFrame()
      pdf(file,title = pdfTitle,paper = "a4") # open the pdf device
      grid.table(datf)
      p1 = qplot(datf[[1]],datf[[3]])
      arrangeGrob(print(p1), ncol = 1, main = "Main title")
      dev.off()  # turn the device off
    }
  )
  
})
