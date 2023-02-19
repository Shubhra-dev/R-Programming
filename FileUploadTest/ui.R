library(shiny)
library(shinyauthr)
source("server.R")


user_base <- tibble::tibble(
  user = c("ShortStayUser","Sale4castUser"),
  password = sapply(c("shortstay_1","sale4cast_1"), sodium::password_store),
  permissions = c("admin", "standard"),
  name = c("User One", "User Two")
)

shinyUI(fluidPage(
  titlePanel("File Input"),
  sidebarLayout(
    sidebarPanel(
      req(credentials()$user_auth),
      tags$a(href="//shortstaysolution.com/intro/", "Instruction"),
      fileInput("file","Upload the file (CSV or Excel)"),
      textInput("link", "Upload link")
    ),
    mainPanel(
      div(class = "pull-right", shinyauthr::logoutUI(id = "logout")),
      
      shinyauthr::loginUI(id = "login"),
      tableOutput("table1"),
      actionButton("screenshot1", "Capture entire page")
    )
  )
))
