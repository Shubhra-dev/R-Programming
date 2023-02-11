library(shiny)
shinyUI(fluidPage(
  titlePanel("File Input"),
  sidebarLayout(
    sidebarPanel(
      tags$a(href="//shortstaysolution.com/intro/", "Instruction"),
      fileInput("file","Upload the file (CSV or Excel)"),
    ),
    mainPanel(
      h1("Original Data"),
      tableOutput("table1"),
      h1("After Conversion"),
      uiOutput("table")
    )
  )
))
