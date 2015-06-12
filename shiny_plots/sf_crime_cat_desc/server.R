library(dplyr)
library(shiny)
library(ggplot2)
library(lubridate)

load("../../data/train_crime.RData")

shinyServer(function(input, output){
  plotInput <- reactive({
    crime_filt <- train_crime %>%
                    filter(Category == input$cat)
    top_desc <- table(crime_filt$Descript) %>% 
                sort(decreasing = T) %>% 
                  head(n = input$num) %>% names
    
    if(input$perc_num == "Percentage"){
    ggp <- crime_filt %>%
            mutate(Year = as.factor(year(Dates))) %>%
            group_by(Descript, Year) %>% tally() %>%
              group_by(Year) %>%
              mutate(perc = (round(n/sum(n), 3)*100)) %>%
                filter(Descript %in% top_desc
                       ) %>%
                ggplot(aes(Year, perc, group = Descript, colour = Descript)) +
                  geom_line() + geom_point() +
                    theme(legend.position = "bottom") +
                      guides(colour = guide_legend("Description", ncol = 2)) +
                      labs(y = input$perc_num,
                           title = paste("Percentage of Crimes for",
                                         input$cat, "Category\nIn San Francisco From (01/06/03 - 05/13/15)"))
                            }
    else{
      ggp <- crime_filt %>%
              mutate(Year = as.factor(year(Dates))) %>%
                group_by(Descript, Year) %>% tally() %>%
                      filter(Descript %in% top_desc,
                             !(Year == 2015)) %>%
              ggplot(aes(Year, n, group = Descript, colour = Descript)) +
                    geom_line() + geom_point() +
                      theme(legend.position = "bottom") +
                        guides(colour = guide_legend( "Description", ncol = 2)) +
                labs(y = input$perc_num,
                    title = paste("Counts of Crimes for",
                           input$cat, "Category\nIn San Francisco From (01/06/03 - 05/13/15)"))
              }
  })
  output$plot <- renderPlot(function() {
    if(input$savePlot) {
      isolate({name <- paste0("../../plots/", input$filename, ".png")})
      ggsave(name, plotInput(), type="cairo-png", height = 6, units = "in")
    }
    else print(plotInput())
  })
  
})