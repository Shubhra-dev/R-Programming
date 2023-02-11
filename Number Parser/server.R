options(shiny.maxRequestSize=10*1024^2)
#if 3 different delim come out in a number, what happen?
#if a number field contain a wrong format, how to skip that row
library(shiny)
library(lubridate)
library(gdata)
library(openxlsx)
library(tidyverse)
library(ggfortify)

shinyServer(function(input,output,session){
  startTime <- Sys.time()
  
  getDataFrame <- reactive({
    startTime <<- Sys.time()
    validate(
      need(input$file != "", "Please select a data set")
    )
    
    
    file_in <- input$file
    x <- file_in$datapath
    
    datf <- read.delim(x,sep = ",")
    
    return(datf)
  })
  
  singleDelim <- function(num,datf,delim,indexArray) {
    marks <-c("X","X")
    #Example:
    #> num <- 123,42
    #> delim
    #[1] ,
    #> sum(strsplit(num, "")[[1]] == delim)
    #[1] 1
    #indexArray
    #[1] 4  -Inf  -Inf  -Inf
    #>str_length(num) - indexArray[1]
    #[1] 2
    
    #> num <- 123,423,123
    #> delim
    #[1] ,
    #> sum(strsplit(num, "")[[1]] == delim)
    #[1] 2
    #indexArray
    #[1] 8  -Inf  -Inf  -Inf
    
    
    #looking for the occurrence of single delimiter
    if (sum(strsplit(num, "")[[1]] == delim) == 1){
      #looking for the digits after delimiter
      if((str_length(num) - indexArray[1]) != 3){
        marks[2] <- delim 
      }
    }
    else{
      marks[1] <- delim
    }
    #Example:
    #> num <- 123,42
    #> mark[1]
    #[1] "X"
    #>mark[2]
    #[1] ","
    
    #> num <- 123,423,123
    #> mark[1]
    #[1] ","
    #>mark[2]
    #[1] "X"
    
    
    return(marks)
    
  }
  

  findSeperator <- function(datf) {
    
    groupingMark = "X"
    decimalMark = "X"
    #row traversing
    for (rowIndex in 1:nrow(datf)) {
      if(groupingMark != decimalMark){
        if(groupingMark != "X" && decimalMark != "X"){
          break;
        }
      }
  
      #taking row values as number
      num <- as.character(datf[rowIndex,1])
      
      # To look the delimeter and their index, strsplit function will split the string as character. 
      # which() will look for the matching and max() will return the maximum index value.
      # Example 1:  
      #  > num <- ("123,456,789.123")
      #  > strsplit(num, "")
      #  [1] "1" "2" "3" "," "4" "5" "6" "," "7" "8" "9" "." "1" "2" "3"  
      #  > which(strsplit(num, "")[[1]] == ",")
      #  [1] 4 8  
      #  > max(which(strsplit(num, "")[[1]] == ",") )
      #  [1] 8
      #  > max(which(strsplit(num, "")[[1]] == ".") )
      #  [1] 12
      #  > max(which(strsplit(num, "")[[1]] == " ") )
      #  [1] -Inf  

      # Example 2:  
      #  > num <- ("123,456")
      #  > strsplit(num, "")
      #  [1] "1" "2" "3" "," "4" "5" "6" 
      #  > which(strsplit(num, "")[[1]] == ",")
      #  [1] 4  
      #  > max(which(strsplit(num, "")[[1]] == ",") )
      #  [1] 4
      #  > max(which(strsplit(num, "")[[1]] == ".") )
      #  [1] -Inf
      #  > max(which(strsplit(num, "")[[1]] == " ") )
      #  [1] -Inf  
      
      commaInd <- max(which(strsplit(num, "")[[1]] == ","))
      dotInd <- max(which(strsplit(num, "")[[1]] == "."))
      spaceInd <- max(which(strsplit(num, "")[[1]] == " "))
      quoteInd <- max(which(strsplit(num, "")[[1]] == "'"))
      
      #Keeping the index of the delimiters in a array
      indexArray <- c(commaInd,dotInd,spaceInd,quoteInd)
      #> num <- ("123,456,789.123")
      #> indexArray
      #[1]    8   12 -Inf -Inf 
      
      #> num <- ("123,456")
      #> indexArray
      #[1]    4 -Inf -Inf -Inf

      delimArray <- c(",","."," ","'")
      #> delimArray
      #[1] "," "." " " "'"      
      delimCount <- sum( !(indexArray %in% "-Inf"))
      #> num <- ("123,456,789.123")
      #> delimCount 
      #[1] 2
	  
      #> num <- ("123,456")
      #> delimCount 
      #[1] 1
      
      if(delimCount == 0){
        next
      }
      
      #> num <- ("123,456")
      else if(delimCount == 1){
        #Looking for specific delimiter
        delimIndex <- which(!( indexArray %in% "-Inf"))
        
        #Example: 
        #num <- "123,456"
        #> delimIndex <- which(!( indexArray %in% "-Inf"))
        #> delimIndex
        #[1] 1
        
        #num <- "123.456"
        #> delimIndex <- which(!( indexArray %in% "-Inf"))
        #> delimIndex
        #[1] 2
        
        #num <- "123 456"
        #> delimIndex <- which(!( indexArray %in% "-Inf"))
        #> delimIndex
        #[1] 3
        
        #num <- "123'456"
        #> delimIndex <- which(!( indexArray %in% "-Inf"))
        #> delimIndex
        #[1] 4
        
        if(delimIndex == 1){
          #Example 2: 
          #num <- "123,456"
          # > str_length(num) - indexArray[1]
          #[1] 3
          
          #Example 3: 
          #num <- "123,46"
          # > str_length(num) - indexArray[1]
          #[1] 2
          
          if((str_length(num) - indexArray[1]) == 3){
            if(decimalMark != ","){
              groupingMark <- ","
            }
            next
          }
          else{
            marks = singleDelim(num,datf,delimArray[1],indexArray)
            groupingMark <- marks[1]
            decimalMark <- marks[2]
          }
        }
        else if(delimIndex == 2){
          #Example: 
          #num <- "123.456"
          # > str_length(num) - indexArray[1]
          #[1] 3
          
          #num <- "123,46"
          # > str_length(num) - indexArray[1]
          #[1] 2
          
          if((str_length(num) - indexArray[2]) == 3){
            if(decimalMark != "."){
              groupingMark <- "."
            }
          }
          else{
            marks = singleDelim(num,datf,delimArray[1],indexArray)
            groupingMark <- marks[1]
            decimalMark <- marks[2]
          }
        }
        else if(delimIndex == 3){
          groupingMark <- " "
        }
        else if(delimIndex == 4){
          groupingMark <- "'"
        }
        
      }
      
      #  > num <- ("123,456,789.123")
      else if( delimCount == 2 ){
        delimIndex = which(!(indexArray %in% "-Inf"))
        #> indexArray
        #[1]    8   12 -Inf -Inf        
        #> delimIndex
        #[1] 1 2
        
        #> indexArray[delimIndex[1]]
        #[1] 8
        #> indexArray[delimIndex[2]]
        #[1] 12
        
        if(indexArray[delimIndex[1]] > indexArray[delimIndex[2]]){
          groupingMark <- delimArray[delimIndex[2]]
          decimalMark <- delimArray[delimIndex[1]]
        }
        else{
          groupingMark <- delimArray[delimIndex[1]]
          decimalMark <- delimArray[delimIndex[2]]
        }
        
      }
    }
    
    #Example:
    #> num <- 1234
    #> groupingMark
    #[1] "X"
    #> decimalMark
    #> "X"
    
    #> num = 123.233
    #> groupingMark
    #[1] "."
    #> decimalMark
    #> "X"
    
    #> num <- 123.4
    #> groupingMark
    #[1] "X"
    #> decimalMark
    #> "."
    
    #> num <- 123,324.12
    #> groupingMark
    #[1] ","
    #> decimalMark
    #> "."
    
    if(groupingMark == decimalMark){
      if(groupingMark == "X"){
        datf[,1] = parse_number(datf[,1])
      }
      else{
        datf[,1] = parse_number(datf[,1], locale = locale(decimal_mark = decimalMark))
      }
    }
    else{
      if(groupingMark == "X"){
        datf[,1] = parse_number(datf[,1], locale = locale(decimal_mark = decimalMark))
      }
      else if (decimalMark == "X"){
        datf[,1] = parse_number(datf[,1], locale = locale(grouping_mark = groupingMark))
      }
      else{
        datf[,1] = parse_number(datf[,1], locale = locale(grouping_mark = groupingMark, decimal_mark = decimalMark))
      }
    }
    
    return(datf)
    
  } 
  
  output$table1 <- renderTable({
    datf <- getDataFrame()
    datf
    })
  
  output$table <- renderUI({
    datf <- getDataFrame()
    datf <- findSeperator(datf)
    renderTable(datf)
  })
})