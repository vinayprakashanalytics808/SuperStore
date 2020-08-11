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
  cluster1 <- reactiveValues()
  
  Sample_Superstore <- data.frame()
  cluster1 <- data.frame()

  observe({
    
    tryCatch({
      # req(input$file1)
      Sample_Superstore <- read_excel(input$file1$datapath, 1)
      df_100 <- head(Sample_Superstore, n = 100)
      
      extension_file <- grepl("\\.xls$", input$file1$datapath)
      print(extension_file)
      print(input$file1$type)
      Sample_Superstore$year <- as.numeric(format(Sample_Superstore$`Order Date`,'%Y'))
      
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
    
          output$tab3 <- renderUI({
            if(nrow(Sample_Superstore) > 0){
              actionButton("go_to_tab3", "Go to Cluster Analysis")
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
          
          selectable_group_columns <- c("Ship Mode", "Segment","Region","State",
                                        "Category",       "Sub-Category")
          
          output$moreControls3 <- renderUI({
            
            if(nrow(Sample_Superstore) > 0){
              selectizeInput("categories", "Categories", 
                             choices = selectable_group_columns,options = list(maxItems = 2)) 
            } 

          })
          
        
          output$moreControls5 <- renderUI({
            
            if(nrow(Sample_Superstore) > 0){
              radioButtons("parameters1", label = "Parameters",choices = c("Sales", "Profit"),inline = TRUE)
              
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
         # print(category_wise_sales)
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
         
         output$sales_bar = renderAmCharts({
           req(input$categories)
           req(input$parameters1)
          
           
           
             category_wise_sales_bar <- Sample_Superstore %>%
             group_by(!!!syms(input$categories)) %>%
             summarise(`Total Sales` = sum(!!sym(input$parameters1)))
             
             category_wise_sales_bar <- as.data.frame(category_wise_sales_bar)

             if(length(colnames(category_wise_sales_bar)) == 2){
             category_wise_sales_bar <- category_wise_sales_bar
             rownames(category_wise_sales_bar) <-  category_wise_sales_bar[,1]
             category_wise_sales_bar  <- category_wise_sales_bar [,-1,drop=F]
           } else {
             category_wise_sales_bar <- dcast(category_wise_sales_bar,  eval(as.symbol(input$categories[1])) ~ eval(as.symbol(input$categories[2])))
             category_wise_sales_bar[is.na(category_wise_sales_bar)] <- 0
             rownames(category_wise_sales_bar) <-  category_wise_sales_bar[,1]
             category_wise_sales_bar <- category_wise_sales_bar[,-1]
           }
           
           amBarplot(y = colnames(category_wise_sales_bar), data = category_wise_sales_bar, stack_type = "regular",xlab = input$categories[1],ylab = input$categories[2] )

         })
  
         output$moreControls31 <- renderUI({
           
           if(nrow(Sample_Superstore) > 0){
             radioButtons("parameters11", label = "Cluster based on",choices = c("Sales", "Profit"),inline = TRUE)
             
           } 
           
         })
         
         
         output$moreControls51 <- renderUI({
           
           if(nrow(Sample_Superstore) > 0){
             radioButtons("kvalue", label = "K Value",choices = c(5,6,7,8,9,10),inline = TRUE)
             # selectInput("kvalue", label = "K Value",choices = c(5,6,7,8,9,10))
             
           } 
           
         })
         
         output$moreControls52 <- renderUI({
           
           if(nrow(Sample_Superstore) > 0){
             actionButton("startcluster", "Perform Clustering")
             
           } 
           
         })
           
             output$cluster_table <- renderDataTable({
             
             req(input$kvalue)
             req(input$parameters11)
             
             cluster1 <- Sample_Superstore %>%  filter(year == "2018" | year == "2019") %>%
             group_by(`Customer ID`) %>%
             summarise(`Total Sales/Profit` = sum(!!sym(input$parameters11)), Order = n_distinct(`Order ID`), Qty = sum(Quantity))
             storecluster <- kmeans(cluster1[, 2:4], input$kvalue)
             cluster1$cluster <- storecluster$cluster
             cluster1 <- as.data.frame(cluster1)
             
             datatable(cluster1, rownames = FALSE, class = "compact nowrap hover row-border",
                       options = list(searching = FALSE, scrollX=TRUE)
             )
           })
             
             output$nocluster_table <- renderDataTable({
               
               req(input$kvalue)
               req(input$parameters11)
               
               cluster1 <- Sample_Superstore %>%  filter(year == "2018" | year == "2019") %>%
                 group_by(`Customer ID`) %>%
                 summarise(`Total Sales/Profit` = sum(!!sym(input$parameters11)), Order = n_distinct(`Order ID`), Qty = sum(Quantity))
               storecluster <- kmeans(cluster1[, 2:4], input$kvalue)
               cluster1$cluster <- storecluster$cluster
               cluster1 <- as.data.frame(cluster1)
               
               custmo <- cluster1 %>% group_by(cluster) %>% summarise(`No of Customers` = n_distinct(`Customer ID`))
               
               datatable(custmo, rownames = FALSE, class = "compact nowrap hover row-border",
                         options = list(searching = FALSE, scrollX=TRUE)
               )
             })
           
         
         
         
               
  })
         
  observeEvent(input$go_to_tab2,{
    tab_switch <- switch (input$tabs,
                          "raw_data" = "data_set_review"
    ) 
    updateTabItems(session, "tabs", tab_switch)
  })
  
  observeEvent(input$go_to_tab3,{
    tab_switch <- switch (input$tabs,
                          "data_set_review" = "clustering"
    ) 
    updateTabItems(session, "tabs", tab_switch)
  })
  
})
