library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  observe({
    
    tryCatch({
      # req(input$file1)
      df <- read_excel(input$file1$datapath, 1)
      df_100 <- head(df, n = 100)
      
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
      if(nrow(df) > 0){
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
    
    
  })
  
  observeEvent(input$go_to_tab2,{
    tab_switch <- switch (input$tabs,
                          "raw_data" = "data_set_review"
    ) 
    updateTabItems(session, "tabs", tab_switch)
  })
  
})
