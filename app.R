#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
#library(curl)
#library(data.table)
library(dplyr)
#library(arrow)
#library(purrr)
library(ggplot2)
library(scales)
library(plotly)

source("./phi_pop_pyramid.R")


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
        column(6, plotlyOutput("pyramid",width = "600px", height = "400px")),
        column(6, plotlyOutput("pyramid2",width = "600px",height = "400px"))
      )

    )
  )
)




# Define server logic required to draw a histogram
server <- function(input, output) {


  output$pyramid <- renderPlotly({

    # get the correct max value for the chosen CP area

    max_value <- maximals |>
      filter(CP_Name == input$cps) |>
      select(maxval) |>
      distinct()|>
      pull()

    t1 <- read.csv("cp-age-sex-pops.csv")|>
      filter(CP_Name == input$cps & sex != "Persons")

    p <- phi_pop_pyramid(t1,
                         xcol = age_band,
                         ycol = pop,
                         fill_by = sex,
                         male_val = "Males",
                         female_val = "Females",
                         ylimit = max_value)

    p <- p + ggplot2::labs(p,
                           title = paste0("Aggregated small area population estimates\n",input$cps),
                           subtitle = "Mid-2022",
                           caption = "Source: NRS small area population estimates\nPublished: Nov 2024",
                           x =  "Age band",
                           y = "Population")

    ggplotly(p)
  })


  output$pyramid2 <- renderPlotly({

    t2 <- read.csv("male-female-broad-age-sex.csv") |>
      filter(CP_Name == input$cps & sex != "Persons")

    p2 <- phi_pop_pyramid(t2,
                          xcol = broad_age_band,
                          ycol = pop,
                          fill_by = sex,
                          male_val = "Males",
                          female_val = "Females",
                          ylimit = NA)  #+
    #scale_y_continuous(labels = label_comma())

    p2 <- p2 + ggplot2::labs(p2,
                             title = paste0("Aggregated small area population estimates\n",input$cps),
                             subtitle = "Mid-2022",
                             caption = "Source: NRS small area population estimates\nPublished: Nov 2024",
                             x =  "Broad age band",
                             y = "Population")




    ggplotly(p2)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
