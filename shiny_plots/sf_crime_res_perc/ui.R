library(shiny)
library(ggplot2)
library(dplyr)

shinyUI(fluidPage(
  titlePanel("Percentage of Resolutions for Top Crime Descriptions"),
      selectInput("cat", "Select the category.",
                  choices = c("LARCENY/THEFT", "VEHICLE THEFT", "VANDALISM",
                              "ROBBERY", "ASSAULT", "BURGLARY",
                              "DRUG/NARCOTIC", "MISSING PERSON",
                              "KIDNAPPING", "PROSTITUTION",
                              "ARSON", "FORGERY/COUNTERFEITING",
                              "SEX OFFENSES, FORCIBLE", "FRAUD",
                              "NON-CRIMINAL", "OTHER OFFENSES",
                              "SECONDARY CODES", "STOLEN PROPERTY",
                              "SUSPICIOUS OCC", "TRESPASS", "WARRANTS",
                              "WEAPON LAWS"), selected = "ASSAULT"),
    mainPanel(plotOutput("plot"), width = 12)
    )
  )