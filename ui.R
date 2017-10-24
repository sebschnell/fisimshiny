#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  titlePanel("Forest Inventory Simulator"),

  sidebarLayout(
    sidebarPanel("Settings",
                 selectInput(inputId = "select_pop",
                             label = "Choose a stand:",
                             choices = list("Kalimantan peat" = "kalimantan_peat")),
                 sliderInput(inputId = "select_sample_size",
                             label = "Choose number of plots",
                             min = 2, max = 10, value = 5, step = 1),
                 sliderInput(inputId = "select_radius",
                             label = "Select a plot radius",
                             min = 5, max = 20, value = 10, step = 1),
                 sliderInput(inputId = "select_sim_steps",
                             label = "How many simulations?",
                             min = 1, max = 10000, value = 1000, step = 200),
                 checkboxGroupInput(inputId = "select_var",
                                    label = "Choose variable of interest",
                                    choices = list("Basal area" = "ba",
                                                   "Tree counts" = "n"),
                                    selected = "n"),
                 radioButtons(inputId = "select_edge",
                              label = "Choose method for edge correction",
                              choices = list("None" = "NA",
                                             "Walkthrough" = "wt"),
                              selected = "wt"),
                 actionButton(inputId = "start_sim",
                              label = "Start simulation")),
    mainPanel(
      tabsetPanel(
        tabPanel("Stand", plotOutput("stand", width = "auto", height = "800px")),
        tabPanel("Estimates", tableOutput("estimates")),
        tabPanel("Distribution of estimates", plotOutput("distribution"))
      )
    )
  )
))
