library(shiny)
library(dplyr)
require(reshape)
library(rAmCharts)
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
    
    # output$review_tab <- renderMenu({
    #   if(nrow(df) > 0){
    #   sidebarMenu(
    #     menuItem("Data Set Review", tabName = "data_set_review")
    #   )
    #   
    #     }
    # })
       print(colnames(Sample_Superstore))

    
       output$category_wise_sales_trend = renderAmCharts({
         
       if(input$des_ana == "Category sales trend"){
         category_wise_sales <- Sample_Superstore %>%
         group_by(`Order Date`, Category) %>%
         summarise(`Total Sales` = sum(Sales))
         category_wise_sales <- as.data.frame(category_wise_sales)
         category_wise_sales <- dcast(category_wise_sales, `Order Date`~Category)
         category_wise_sales[is.na(category_wise_sales)] <- 0
         
         amTimeSeries(category_wise_sales, "Order Date", c("Furniture", "Office Supplies", "Technology"))
         
       } else if(input$des_ana == "Sub-Category sales trend"){
         sub_category_wise_sales <- Sample_Superstore %>%
         group_by(`Order Date`, `Sub-Category`) %>%
         summarise(`Total Sales` = sum(Sales))
         sub_category_wise_sales <- as.data.frame(sub_category_wise_sales)
         sub_category_wise_sales <- dcast(sub_category_wise_sales, `Order Date`~`Sub-Category`)
         sub_category_wise_sales[is.na(sub_category_wise_sales)] <- 0
         
         amTimeSeries(sub_category_wise_sales, "Order Date", 
                      colnames(sub_category_wise_sales)[colnames(sub_category_wise_sales) != "Order Date"],export = TRUE)
         
       }
         
      
       })
     
    # category_wise_sales <- Sample_Superstore %>%
    #   group_by(`Order Date`, Category) %>%
    #   summarise(`Total Sales` = sum(Sales))
    # category_wise_sales <- as.data.frame(category_wise_sales)
    # category_wise_sales <- dcast(category_wise_sales, `Order Date`~Category)
    # category_wise_sales[is.na(category_wise_sales)] <- 0
    # 
    # category_wise_products <- Sample_Superstore %>%
    #   group_by(`Order Date`, Category) %>%
    #   summarise(`Total Products` = n_distinct(`Product ID`))
    # category_wise_products <- as.data.frame(category_wise_products)
    # category_wise_products <- dcast(category_wise_products, `Order Date`~Category)
    # category_wise_products[is.na(category_wise_products)] <- 0
    
    
  })
  
  observeEvent(input$go_to_tab2,{
    tab_switch <- switch (input$tabs,
                          "raw_data" = "data_set_review"
    ) 
    updateTabItems(session, "tabs", tab_switch)
  })
  
})
