library(shiny)
library(ggplot2)
library(dplyr)

shinyServer(function(input, output){
  output$plot <- renderPlot({
    crime_filt <- train_crime %>%
      filter(Category == input$cat)
    
    des_names_10 <- top_n_crime(crime_filt, 10, "Descript")
    res_names_10 <- top_n_crime(crime_filt, 10, "Resolution")
    
    
    ggp <- crime_filt %>%
      filter(Descript %in% des_names_10) %>%
        group_by(Descript, Resolution) %>%
          tally() %>%
          group_by(Descript) %>%
            mutate(Percentage = (n/sum(n))*100) %>%
            filter(Resolution %in% res_names_10) %>%
          ggplot(aes(Descript, Percentage, fill = Resolution)) +
            geom_bar(stat = "identity") +
              coord_flip() +
                guides(fill = guide_legend("Resolution"))+
            labs(y = "Percentage of Resolutions for Crime Descriptions",
              title = paste("Percentage of Resolutions for\nTop Crime Descriptions for", input$cat, "Category"))
    
    print(ggp)
  })
  
})