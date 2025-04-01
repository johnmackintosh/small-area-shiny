#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# library(ggplot2)
# library(scales)
# library(plotly)



#source("./phi_pop_pyramid.R")
source("./base_pyramid.R")


maximals <- read.csv("./maximal_values.txt")

# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  titlePanel("Small area population estimates"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      selectInput("cps",
                  "Select Community Partnership:",
                  choices = c("Badenoch and Strathspey",
                              "Bute and Cowal",
                              "Caithness",
                              "East Ross",
                              "Helensburgh and Lomond",
                              "Inverness",
                              "Lochaber",
                              "Mid-Argyll, Kintyre and Islay",
                              "Mid Ross",
                              "Nairn and Nairnshire",
                              "Oban, Lorn and the Isles",
                              "Skye, Lochalsh and West Ross",
                              "Sutherland"))
    ),

    # Show a plot of the generated distribution
    mainPanel(
      fluidRow(
        column(6, plotOutput("pyramid",width = "600px", height = "400px")),
      )

    )
  )
)




# Define server logic required to draw a histogram
server <- function(input, output) {

  output$pyramid <- renderPlot({

base_pyramid(areaname = input$cps)

  })


}

# Run the application
shinyApp(ui = ui, server = server)
