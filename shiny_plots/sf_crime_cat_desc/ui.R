library(shiny)
library(ggplot2)

shinyUI(fluidPage(
  titlePanel("Crimes that Occur the Most for the Top 20 Categories"),
  sidebarLayout(
    sidebarPanel(
      selectInput("cat", "Select the Crime Category.",
                  choices = unique(train_crime$Category), selected = "ASSAULT"),
      numericInput("num", "Select number of Crime Descriptions\nranked by number of occurences.",
                   value = 10),
      radioButtons("perc_num", label = NULL,
                   choices = c("Percentage", "Count"),
                   selected = "Percentage"),  
      textInput('filename', "Filename"),
      checkboxInput('savePlot', "Check to save")
      ),
    mainPanel(plotOutput("plot"))
    )
  ))