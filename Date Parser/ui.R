library(shiny)
shinyUI(fluidPage(
  titlePanel("File Input"),
  sidebarLayout(
    sidebarPanel(
      tags$a(href="//shortstaysolution.com/intro/", "Instruction"),
      fileInput("file","Upload the file (CSV or Excel)"),
      textInput("text","Link upload")
    ),
    mainPanel(
      tableOutput('table1')
    )
  )
))
