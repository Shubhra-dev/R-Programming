#Database Connect
library(shiny)

shinyUI(fluidPage(
  titlePanel("File Input"),
#sidebar
  sidebarLayout(
    sidebarPanel(
      fileInput("file","Upload the file (CSV or Excel)"),
      textInput("link", "Upload link")
    ),
#main panel
    mainPanel(
      uiOutput("plot")
    )
  )
))
