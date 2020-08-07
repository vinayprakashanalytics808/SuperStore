library(shiny)
library(shinydashboard)
library(DT)
library(readxl)

ui <- dashboardPage(
  
  dashboardHeader(title = "SuperStore"),
  
  dashboardSidebar(sidebarMenu(id="tabs",
                               menuItem("Raw Data", tabName = "raw_data"),
                               menuItem("Data Set Review", tabName = "data_set_review"),
                               sidebarMenuOutput("review_tab")
  )),
  
  
  dashboardBody(tags$head(tags$style(".shiny-output-error { visibility: hidden; }")),
                tabItems(
                  tabItem(tabName = "raw_data",
                          fluidRow(
                            div(fileInput("file1", "Upload data", accept = ".xls"),style = "padding:20px;")
                          ),
                          fluidRow(offset = 0, 
                                   div(dataTableOutput("input_table"),style = "padding:20px;"),
                                   div(uiOutput("tab2"),style = "padding:20px;")
                                   # div(actionButton("go_to_tab2", "Go to Data Review"),style = "padding:20px;")
                          )),
                  tabItem(tabName = "data_set_review",
                          h1("fg")
                  ))
  ))

