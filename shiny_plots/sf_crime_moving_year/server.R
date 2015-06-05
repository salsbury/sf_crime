library(shiny)
library(ggplot2)

#make sure data frame sfn is loaded

shinyServer(function(input, output){
  output$plot <- renderPlot({
    crime_df <- train_crime %>%
      mutate(Year_crime = year(Dates)) %>%
        filter(Category == input$cat,
               Year_crime == input$year,
               Y < 40)
      ggp <- ggplot() +
      geom_polygon(data = sfn, 
                   aes(x = long, y = lat, group = group),
                   fill="#3D3D4C", colour = "black") +
            geom_point(data = crime_df, 
                       aes(X,Y), colour = "#3366FF")
      print(ggp)
    
  })
  
})