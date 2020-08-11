library(shiny)
library(shinydashboard)
library(DT)
library(readxl)
library(dplyr)
require(reshape2)
library(rAmCharts)
library(rlang)

ui <- dashboardPage(
  
  dashboardHeader(title = "SuperStore"),
  
  dashboardSidebar(sidebarMenu(id="tabs",
                               menuItem("Raw Data", tabName = "raw_data"),
                               menuItem("Data Set Review", tabName = "data_set_review"),
                               menuItem("Clustering", tabName = "clustering"),
                               sidebarMenuOutput("review_tab")
  )),
  
  
  dashboardBody(tags$head(tags$style(".shiny-output-error { visibility: hidden; }")),
                tabItems(
                  tabItem(tabName = "raw_data",
                          fluidRow(
                            column(width = 4, div(fileInput("file1", "Upload data", accept = ".xls"),style = "padding:20px;")),
                                   column(width = 4, br(), div(uiOutput("tab2"),style = "padding:20px;"))
                          ),
                          fluidRow(offset = 0, 
                                   div(dataTableOutput("input_table"),style = "padding:20px;")
                          )),
                  tabItem(tabName = "data_set_review",
                          fluidRow(
                            column(width = 4, uiOutput("moreControls3")),
                            column(width = 4, uiOutput("moreControls5")),
                            column(width = 4, div(uiOutput("tab3"),style = "padding:20px;"))),
                          amChartsOutput("sales_bar",width = "100%",height = 400),br(),br(),
                          fluidRow(
                          column(width = 4, uiOutput("moreControls1")),
                          column(width = 4, uiOutput("moreControls")),
                          column(width = 4, uiOutput("moreControls2"))),
                          amChartsOutput("sales_trend",width = "100%",height = 400)
                  ),
                  tabItem(tabName = "clustering",
                          fluidRow(
                            column(width = 4, uiOutput("moreControls31")),
                            column(width = 4, uiOutput("moreControls51"))
                            # column(width = 4, br(),uiOutput("moreControls52"))
                            ),
                          div(dataTableOutput("cluster_table"),style = "padding:20px;"),
                          div(dataTableOutput("nocluster_table"),style = "padding:20px;")
                  ))
  ))

