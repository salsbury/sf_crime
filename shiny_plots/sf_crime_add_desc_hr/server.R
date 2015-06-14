library(shiny)
library(ggplot2)
library(dplyr)
library(lubridate)

#
# functions to use in the shiny server
#

top_n_crime <- function(df, num, cat){
  table(df[, cat]) %>% sort(decreasing = TRUE) %>%
    head(n = num) %>% names
  
}

street_wrangle <- function(street_add){ 
  street <- gsub("Street", "st", 
                 street_add, ignore.case = T)
  ave <- gsub("avenue|ave", "av",
              street, ignore.case = T)
  blvd <- gsub("blvd", "bl",
               ave, ignore.case = T)
  split_street<- unlist(strsplit(tolower(blvd), " ?(&|/|and) ?"))
  unique_add <- unique(train_crime$Address)
  if(length(split_street) ==1){
   add_comp <- unlist(strsplit(split_street, " "))
   print(add_comp)
   add_logical <- grepl(add_comp[1], unique_add, ignore.case = T) & 
     grepl(add_comp[2], unique_add, ignore.case = T)
   unique_add[add_logical]
  }
  else{
    add_logical <- grepl(split_street[1], unique_add, ignore.case = T) & 
      grepl(split_street[2], unique_add, ignore.case = T)
    unique_add[add_logical]
  }
}

#
# start of shiny server
#

shinyServer(function(input, output){
  dist_df <- reactive({
    train_crime %>%
    filter(PdDistrict == input$district)
  })
  
  
  output$address <- renderUI({
    if(input$search_add == "Select"){
    selectInput("address", "Select the Address from the District Above.",
                top_n_crime(dist_df(), 20, "Address"))
    }
    else{
      textInput("address", "Search for the Address.", "16th st & shotwell st")
    }
  })
  
  output$district <- renderUI({
    if(input$search_add == "Select"){
    selectInput("district", "Select the Police District.",
                choices = c("NORTHERN", "PARK", "INGLESIDE", "BAYVIEW",
                            "RICHMOND", "CENTRAL", "TARAVAL", "TENDERLOIN",
                            "MISSION", "SOUTHERN"))
                    }
    else{
      NULL
    }
    
  })
  

    crime_df <- reactive({
      if(input$search_add == "Search"){
             add_choose <- street_wrangle(input$address)
                  }
      else{
        add_choose <- input$address
      }
                         
      add_df <- train_crime %>%
        filter(Address %in% add_choose,
             Category == input$cat) %>%
            select(Dates, Address, Descript)
      add_df
      
                })
  
  hour_df <- reactive({
    crime_dates <- crime_df()
    minute(crime_dates$Dates) <- 0
    
    hour_tally <-crime_dates %>%
      mutate(
        hour_only = hour(Dates)) %>%
      group_by(hour_only) %>%
      tally()

    hour_of_day <- 0:23
    
    if(nrow(hour_tally) == 24){
           hour_tally %>%
             mutate(hour_only = as.factor(hour_only))
                }
    else{    
      new_tally <- cbind(hour_only = 
            hour_of_day[!(hour_of_day %in% hour_tally$hour_only)],
                n = 0) %>%
              rbind.data.frame(hour_tally) %>%
                mutate(hour_only = as.numeric(hour_only)) %>%
                  arrange(hour_only) %>%
                    mutate(hour_only = as.factor(hour_only))

              }
  })
    
    
    output$plot <- renderPlot({
      ggplot(hour_df(), aes(hour_only, n)) +
      geom_bar(stat = "identity") +
        geom_text(aes(label = n), vjust = -0.75, size = 3) +
      labs(x = "Hour of Day",
           y = "Counts of Crimes Reported",
           title = paste("Count of", input$cat, "Crimes\nat", input$address, "\nBetween",
                         input$dates[1], "and", input$dates[2]))
          })
    
  output$info <- renderPrint({
    plot_click_x <- input$plotclick$x
    if(is.null(plot_click_x)){
      "Please click on a bar to glance at the data for that hour."
    }
    else{
      print(unique(crime_df()$Address))
    crime_df() %>%
      mutate(hour_only = hour(Dates)) %>%
      filter(hour_only == (round(as.numeric(plot_click_x)) - 1)) %>%
      select(Dates, Descript)
    }

  })
  
  })
  

