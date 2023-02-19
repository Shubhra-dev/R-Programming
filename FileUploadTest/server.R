options(shiny.maxRequestSize=10*1024^2)
source("ui.R")

library(shiny)
library(openxlsx)
library(tidyverse)
library(shinyauthr)
library(shinyscreenshot)



shinyServer(function(input,output,session){
  credentials <- shinyauthr::loginServer(
    id = "login",
    data = user_base,
    user_col = user,
    pwd_col = password,
    sodium_hashed = TRUE,
    log_out = reactive(logout_init())
  )
  
  logout_init <- shinyauthr::logoutServer(
    id = "logout",
    active = reactive(credentials()$user_auth)
  )
  
  startTime <- Sys.time()
  
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
    
    #read .csv files with semicolon separator 
    else if(grepl(".csv",fileName)){
      
      if (grepl(";", fLine)){
        
        datf <- read_csv2(fileName,col_types =  cols(.default = col_character()))
        
      } 
      else{
        datf <- read_csv(fileName,col_types =  cols(.default = col_character()))
      }
      
      return(datf)
    }
    
    #read .txt and .dat files
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
  observeEvent(input$screenshot1, {
    screenshot()
  })
  output$table1 <- renderTable({
    req(credentials()$user_auth)
    datf <- getDataFrame()
    print(NCOL(datf))
    datf
    })
})