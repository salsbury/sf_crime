library(shiny)
library(ggplot2)


shinyUI(fluidPage(
  includeCSS("styles.css"),
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
              )