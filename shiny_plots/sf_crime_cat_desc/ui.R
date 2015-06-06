library(shiny)
library(ggplot2)

shinyUI(fluidPage(
  titlePanel("Crimes that Occur the Most for the Top 20 Categories"),
  sidebarLayout(
    sidebarPanel(
      selectInput("cat", "Select the Crime Category.",
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
      numericInput("num", "Select number of Crime Descriptions\nranked by number of occurences.",
                   value = 10),
      radioButtons("perc_num", label = NULL,
                   choices = c("Percentage", "Count"),
                   selected = "Percentage")
      ),
    mainPanel(plotOutput("plot"))
    )
  ))