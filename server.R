library(shiny)
# library(shinydashboard)
# library(DT)
# library(readxl)
# library(dplyr)
# require(reshape2)
# library(rAmCharts)

# source("Data_wrangling.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  Sample_Superstore <- reactiveValues()

  
  Sample_Superstore <- data.frame()

  observe({
    
    tryCatch({
      # req(input$file1)
      Sample_Superstore <- read_excel(input$file1$datapath, 1)
      df_100 <- head(Sample_Superstore, n = 100)
      
      extension_file <- grepl("\\.xls$", input$file1$datapath)
      print(extension_file)
      print(input$file1$type)
      
      
      output$input_table <- renderDataTable({
        datatable(df_100, rownames = FALSE, class = "compact nowrap hover row-border",
                  options = list(searching = FALSE, scrollX=TRUE)
        )
      })
      
      
    }, error=function(e) 
    {
      cat(paste("in err handler2\n"),unlist(e))
    }
    )
    
          output$tab2 <- renderUI({
            if(nrow(Sample_Superstore) > 0){
              actionButton("go_to_tab2", "Go to Data Review")
            }
          })
    
          output$moreControls1 <- renderUI({
            if(nrow(Sample_Superstore) > 0){
            selectInput("des_ana", "Select Variable", choices = c("Ship Mode",
                                                              "Segment", "Country/Region", "City","State",
                                                              "Region", "Category", "Sub-Category"), selected = "Category")
            }
          })

          output$moreControls <- renderUI({
            
            if(!is.null(input$des_ana)){
              selectizeInput("sub_cat", "sub-Div", choices = levels(factor(Sample_Superstore[[input$des_ana]])),selected = 
                             levels(factor(Sample_Superstore[[input$des_ana]]))[1],
                             multiple = TRUE) 
            } 
            
          })
          
          output$moreControls2 <- renderUI({
            
            if(nrow(Sample_Superstore) > 0){
              radioButtons("parameters", label = "Parameters",choices = c("Sales", "Profit"),inline = TRUE)
            } 
            
          })
          
          
     
          
         output$sales_trend = renderAmCharts({
         req(input$sub_cat)
         req(input$parameters)
         if(input$des_ana == "Category"){
          
         
         Sample_Superstore <- Sample_Superstore %>% filter(Category%in% input$sub_cat)
         category_wise_sales <- Sample_Superstore %>%
         group_by(`Order Date`, Category) %>%
         summarise(`Total Sales` = sum(!!sym(input$parameters)))
         print(category_wise_sales)
         category_wise_sales <- as.data.frame(category_wise_sales)

         category_wise_sales <- dcast(category_wise_sales, `Order Date`~ Category)
         category_wise_sales[is.na(category_wise_sales)] <- 0


         amTimeSeries(category_wise_sales, "Order Date", input$sub_cat,export = TRUE,
                      main = paste0(toString(input$des_ana)," ",toString(input$parameters)),bullet = 'round')
         
       } 
         else if(input$des_ana == "Sub-Category"){

         Sample_Superstore <- Sample_Superstore %>% filter(`Sub-Category`%in% input$sub_cat)
         category_wise_sales <- Sample_Superstore %>%
           group_by(`Order Date`, `Sub-Category`) %>%
           summarise(`Total Sales` = sum(!!sym(input$parameters)))

         category_wise_sales <- as.data.frame(category_wise_sales)

         category_wise_sales <- dcast(category_wise_sales, `Order Date`~ `Sub-Category`)
         category_wise_sales[is.na(category_wise_sales)] <- 0


         amTimeSeries(category_wise_sales, "Order Date", input$sub_cat,export = TRUE,
                      main = paste0(toString(input$des_ana)," ",toString(input$parameters)),bullet = 'round')
         
         }  else if(input$des_ana == "Ship Mode"){
           Sample_Superstore <- Sample_Superstore %>% filter(`Ship Mode`%in% input$sub_cat)
           category_wise_sales <- Sample_Superstore %>%
             group_by(`Order Date`, `Ship Mode`) %>%
             summarise(`Total Sales` = sum(!!sym(input$parameters)))
           
           category_wise_sales <- as.data.frame(category_wise_sales)
           
           category_wise_sales <- dcast(category_wise_sales, `Order Date`~ `Ship Mode`)
           category_wise_sales[is.na(category_wise_sales)] <- 0
           
           
           amTimeSeries(category_wise_sales, "Order Date", input$sub_cat,export = TRUE,
                        main = paste0(toString(input$des_ana)," ",toString(input$parameters)),bullet = 'round')
         }
         })
         
                   
               
  })
         
  observeEvent(input$go_to_tab2,{
    tab_switch <- switch (input$tabs,
                          "raw_data" = "data_set_review"
    ) 
    updateTabItems(session, "tabs", tab_switch)
  })
  
})
