library(shiny)
library(ggplot2)
library(dplyr)

shinyUI(fluidPage(
  titlePanel("Percentage of Resolutions for Top Crime Descriptions"),
      selectInput("cat", "Select the category.",
                  choices = unique(train_crime$Category), selected = "ASSAULT"),
  textInput('filename', "Filename"),
  checkboxInput('savePlot', "Check to save"),
    mainPanel(plotOutput("plot"), width = 12)
    )
  )