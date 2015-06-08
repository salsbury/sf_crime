library(shiny)
library(ggplot2)
library(dplyr)

top_n_crime <- function(df, num, cat){
  table(df[, cat]) %>% sort(decreasing = TRUE) %>%
    head(n = num) %>% names
}

shinyServer(function(input, output){
  output$plot <- renderPlot({
    zip_df <- sfn[sfn$ZIP_CODE == input$zipcode,]
    zip_bb <- slot(zip_df, name = "bbox")
    
    zipcode_name <- switch(input$zipcode,
                           "94121" = "Outer Richmond",
                           "94118" = "Inner Richmond",
                           "94129" = "Presidio Area",
                           "94123" = "Marina",
                           "94115" = "Bottom of Pacific Heights/Top of Western Addition",
                           "94109" = "Nob Hill/Left of Russian Hill/Top Left of Japantown",
                           "94133" = "Right of Russian Hill/Left of North Beach",
                           "94108" = "Chinatown/Right of North Beach",
                           "94105" = "South Beach/Bottom of Financial District",
                           "94122" = "Golden Gate Park/Top of Inner and Outer Sunset",
                           "94117" = "Haight Ashbury",
                           "94102" = "Tenderloin/Downtown/Civic Center",
                           "94103" = "South of Market",
                           "94107" = "Potrero Hill/Mission Bay/Right of SOMA",
                           "94110" = "Mission/Bernal Heights",
                           "94114" = "Castro/Top of Noe Valley",
                           "94131" = "Twin Peaks/Diamond Heights/Glen Park",
                           "94127" = "West of Twin Peaks",
                           "94116" = "Bottom of Inner and Outer Sunset/Top of Parkside",
                           "94132" = "Bottom of Parkside/Lakeshore",
                           "94112" = "Outer Mission/Crocker Amazon/Left of Excelsior",
                           "94134" = "Right of Excelsior/Vistacion Valley",
                           "94124" = "Bayview/Hunter's Point",
                           "94111" = "Embarcadero",
                           "94104" = "Financial District"
                           
                      )
    
    clean_tc_df<- train_crime %>%
          filter(Y < 40,
             X >= zip_bb[1],
             X <= zip_bb[3],
             Y >= zip_bb[2],
             Y <= zip_bb[4],
             Category %in% input$cat,
             Dates >= as.POSIXct(input$dates[1]),
             Dates <= as.POSIXct(input$dates[2]))
    
    ggplot() + 
      geom_polygon(data = zip_df, 
                   aes(x = long, y = lat, group = group),
                   fill="#3D3D4C") +
      geom_point(data = clean_tc_df,
                 aes(X, Y, colour = Category), alpha = 0.075) +
      geom_path(data = zip_df,
                aes(x = long, y = lat, group = group), colour = "black") +
      guides(colour = guide_legend(override.aes = list(alpha=1.0, size=3.0),
                                   title = "Category")) +
      labs(title = paste("Crime in San Francisco's", input$zipcode, "Zip Code\n", zipcode_name, "\nFrom", input$dates[1], "-", input$dates[2])) +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.ticks = element_blank(),
            axis.line = element_blank())
    
  })
  
})