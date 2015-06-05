library(shiny)
library(ggplot2)

animationOptions(interval = 4000)

shinyUI(fluidPage(
  titlePanel("Crime in San Francisco"),
  sidebarLayout(
    sidebarPanel(
      selectInput("cat", "Select the crime category.",
                  choices = c("OTHER OFFENSES","LARCENY/THEFT", "VEHICLE THEFT", "VANDALISM",
                              "NON-CRIMINAL", "ROBBERY", "ASSAULT", "BURGLARY",
                              "DRUNKENNESS", "DRUG/NARCOTIC", "STOLEN PROPERTY", "MISSING PERSON",
                              "KIDNAPPING", "DRIVING UNDER THE INFLUENCE", "PROSTITUTION",
                              "DISORDERLY CONDUCT", "ARSON", "SUICIDE", "LOITERING",
                              "SEX OFFENSES, FORCIBLE", "SEX OFFENSES, NON FORCIBLE", "RECOVERED VEHICLE"),
                  selected = "ROBBERY"),
      sliderInput("year", "Select the year.",
                  min = 2003, max = 2015, value = 2003,
                  step = 1, animate = TRUE)
      ),
    mainPanel(plotOutput("plot"))
    )
  ))