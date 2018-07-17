function(input, output) {
  
  # Test ----------------------------------------------------------------
  
  # coalesce select regional lu tables
  regdf <- reactive({
    attributes <- c("employment", "households", "population", "residential_units")
    
    df <- NULL
    for (a in attributes) {
      d <- read.csv(file.path(lu.run, paste0("alldata__table__", a, ".csv")))
      # transform data into long form
      d2 <- d %>% 
        select(-alldata_id) %>%
        gather(contains(a), key = "column", value = "estimate") %>%
        mutate(attribute = a, 
               year = str_extract(column, "\\d+")) %>%
        select(-column)
      
      ifelse(is.null(df), df <- d2, df <- rbind(df, d2))
    }
    return(df)
  })
  
  # render bar graph
  output$regplot <- renderPlotly({
    # call reactive function that contains regional lu table & subset it for user's input
    r <- regdf() 
    r2 <- r %>% filter(attribute == input$lu_select_attr)
    
    # plot
    p <- plot_ly(r2,
                 x = ~year,
                 y = ~estimate) %>%
      add_lines() %>%
      layout(font = list(family="Segoe UI", size = 13.5),
             title = " ",
             xaxis = list(title = "Year"),
             yaxis = list(title = "Regional Estimate"),
             margin = list(l=100, b=100, t=90, r=100))
      
  })

  
  
    
} # end function



