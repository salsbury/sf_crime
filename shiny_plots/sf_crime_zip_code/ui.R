library(shiny)
library(ggplot2)
library(dplyr)

shinyUI(fluidPage(
  titlePanel("Crime in Zip Codes in San Francisco"),
  sidebarLayout(
    sidebarPanel(
      selectInput("zipcode", "Select the Zipcode.",
                  choices = c(94107, 94105, 94129, 94121, 94118, 94123,
                              94133, 94109, 94111, 94104, 94108, 94103,
                              94115, 94102, 94124, 94110, 94134, 94112,
                              94116, 94114, 94131, 94122, 94127, 94117, 94132
                              ),
                  selected = 94107),
      dateRangeInput("dates", "Select the dates (between 2003-01-06 and 2015-05-13).",
                     start = "2003-01-06", end = "2015-05-13",
                     min = "2003-01-06", max = "2015-05-13"),
      checkboxGroupInput("cat", "Select the crime category.",
                         choices = c("LARCENY/THEFT", "VEHICLE THEFT", "VANDALISM",
                                     "ROBBERY", "ASSAULT", "BURGLARY",
                                     "DRUNKENNESS", "DRUG/NARCOTIC", "MISSING PERSON",
                                     "KIDNAPPING", "PROSTITUTION",
                                     "ARSON", "LOITERING",
                                     "SEX OFFENSES, FORCIBLE"),
                         selected = "ROBBERY")
      
      ),
    mainPanel(plotOutput("plot", click = "plotclick"),
              verbatimTextOutput("info")),
    )
  ))