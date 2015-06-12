library(shiny)
library(ggplot2)
library(dplyr)
library(lubridate)

top_n_crime <- function(df, num, cat){
  table(df[, cat]) %>% sort(decreasing = TRUE) %>%
    head(n = num) %>% names
  
}


shinyServer(function(input, output){
  dist_df <- reactive({
    train_crime %>%
    filter(PdDistrict == input$district)
  })
  
  
  output$address <- renderUI({
    selectInput("address", "Select the Address from the District Above.",
                top_n_crime(dist_df(), 20, "Address"))
    
  })
  
  output$plot <- renderPlot({
    crime_dates <- train_crime %>%
      filter(Dates >= as.POSIXct(input$dates[1]),
             Dates <= as.POSIXct(input$dates[2]),
             Address == input$address,
             Category == input$cat)
    
    minute(crime_dates$Dates) <- 0
    
    crime_dates %>%
      mutate(date_only = as.Date(Dates),
             hour_only = as.factor(hour(Dates))) %>%
      group_by(hour_only) %>%
      tally() %>%
      ggplot(aes(hour_only, n)) +
      geom_bar(stat = "identity") +
      labs(x = "Hour of Day",
           y = "Counts of Crimes Reported",
           title = paste("Count of", input$cat, "Crimes\nat", input$address, "\nBetween",
                         input$dates[1], "and", input$dates[2]))
    
    
  })
  
})
