library(shiny)
library(ggplot2)
library(dplyr)


shinyUI(fluidPage(
  titlePanel("Count of Crimes for Addresses and Categories Per Hour"),
  sidebarLayout(
    sidebarPanel(
      selectInput("district", "Select the Police District.",
                  choices = c("NORTHERN", "PARK", "INGLESIDE", "BAYVIEW",
                              "RICHMOND", "CENTRAL", "TARAVAL", "TENDERLOIN",
                              "MISSION", "SOUTHERN")),
      uiOutput("address"),
      selectInput("cat", "Select the Category.",
                  top_n_crime(train_crime, 30, "Category")),
      dateRangeInput("dates", "Select the dates (between 2003-01-06 to 2015-05-13).",
                     start = "2003-01-06", end = "2015-05-13",
                     min = "2003-01-06", max = "2015-05-13")
      ),
    mainPanel(plotOutput("plot"))
    )
  ))