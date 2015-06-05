library(dplyr)
library(shiny)
library(ggplot2)
library(lubridate)
library(rgdal)
library(readr)

train_crime <- read_csv("data/train.csv")

train_crime$Dates <- as.POSIXct(train_crime$Dates, format = "%m/%d/%Y %H:%M", tz = "UTC")

dayofweek <- 1:7
names(dayofweek) <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
train_crime$DayOfWeekNum <- dayofweek[train_crime$DayOfWeek]

sfn = readOGR("./data/sfzipcodes", "sfzipcodes") %>% spTransform(CRS("+proj=longlat +datum=WGS84"))

server <- function(input, output){
  
  output$choose_time <- renderUI({
    radioButtons("time", "Choose the measure of time to look at.",
                 choices = c("Year", "Month", "Day", "Hour"),
                 selected = "Year", inline = TRUE)
  })
  
  output$choose_slide <- renderUI({
    if(input$time == "Year"){
      sliderInput("num_time", "Select the Year.",
                  min = 2003, max = 2015, value = 2003,
                  step = 1, sep = "", 
                  animate = animationOptions(interval = input$ani_time * 1000))
    }
    else if(input$time == "Month"){
      sliderInput("num_time", "Select the Month.",
                  min = 1, max = 12, value = 1,
                  step = 1, sep = "", 
                  animate = animationOptions(interval = input$ani_time * 1000))
    }
    else if(input$time == "Day"){
      sliderInput("num_time", "Select the Day of the Week.",
                  min = 1, max = 7, value = 1,
                  step = 1, sep = "", 
                  animate = animationOptions(interval = input$ani_time * 1000))
    }
    else{
      sliderInput("num_time", "Select the Hour.",
                  min = 0, max = 23, value = 0,
                  step = 1, sep = "", 
                  animate = animationOptions(interval = input$ani_time * 1000))
    }
  })
  
  crime_dataf <- reactive({
    
    if(input$time == "Year"){
      train_crime %>%
        mutate(Year_crime = year(Dates)) %>%
        filter(Category %in% input$cat,
               Year_crime == input$num_time,
               Y < 40)
    }
    
    else if(input$time == "Month"){
      train_crime %>%
        mutate(Month_crime = month(Dates)) %>%
        filter(Category %in% input$cat,
               Month_crime == input$num_time,
               Y < 40)
    }
    
    else if(input$time == "Day"){
      train_crime %>%
        filter(Category %in% input$cat,
               DayOfWeekNum == input$num_time,
               Y < 40)
    }
    else{
      train_crime %>%
        mutate(Hour_crime = hour(Dates)) %>%
        filter(Category %in% input$cat,
               Hour_crime == input$num_time,
               Y < 40)
    }
  })
  
  output$plot <- renderPlot({
    ggp <- 
      ggplot() +
      geom_polygon(data = sfn, 
                   aes(x = long, y = lat, group = group),
                   fill="#3D3D4C") +
      geom_point(data = crime_dataf(), 
                 aes(X,Y, colour = Category), 
                 alpha = 0.1) +
      geom_path(data = sfn, 
                aes(x = long, y = lat, group = group),
                colour = "black") +
      ggtitle(paste("Crimes in San Francisco for", 
                    input$time, input$num_time)) +
      guides(colour = guide_legend(override.aes = 
                                     list(alpha=1.0, size=3.0),
                                   title = "Crime Category")) +
      theme(axis.line = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank())
    
    print(ggp)
    
  })
  
}


ui <- fluidPage(
  includeCSS("./shiny_plots/sf_crime_moving_time/styles.css"),
  titlePanel("Crime in San Francisco"),
  sidebarLayout(
    sidebarPanel(
      uiOutput("choose_time"),
      uiOutput("choose_slide"),
      numericInput("ani_time", "Select the number of seconds for animation time.",
                   value = 5),
      checkboxGroupInput("cat", "Select the crime category.",
                         choices = c("LARCENY/THEFT", "VEHICLE THEFT", "VANDALISM",
                                     "ROBBERY", "ASSAULT", "BURGLARY",
                                     "DRUNKENNESS", "DRUG/NARCOTIC", "MISSING PERSON",
                                     "KIDNAPPING", "PROSTITUTION",
                                     "ARSON", "LOITERING",
                                     "SEX OFFENSES, FORCIBLE"),
                         selected = "ROBBERY")),
    mainPanel(plotOutput("plot"))
    
  )
)


shinyApp(ui = ui, server = server)
