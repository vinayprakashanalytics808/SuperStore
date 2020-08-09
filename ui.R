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
                          )),
                  tabItem(tabName = "data_set_review",
                          selectInput("des_ana", "Sales Trend", choices = c("Category sales trend", 
                                                                      "Sub-Category sales trend")),
                          amChartsOutput("sales_trend",width = "100%",height = 400),
                          selectInput("des_ana1", "No of Products Trend", choices = c("Category products trend", 
                                                                      "Sub-Category products trend")),
                          amChartsOutput("products_trend",width = "100%",height = 400)
                  ))
  ))

