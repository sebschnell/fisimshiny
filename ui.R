library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  titlePanel("Forest Inventory Simulator"),

  sidebarLayout(
    sidebarPanel("Settings",
                 selectInput(inputId = "select_pop",
                             label = "Choose a stand:",
                             choices = list("Kalimantan peat" = "kalimantan_peat",
                                            "Heinrichsberg beech" = "hberg_beech",
                                            "Random trees" = "random_tree")),
                 selectInput(inputId = "select_sampling_design",
                             label = "Choose a sampling_design:",
                             choices = list("Random sampling" = "random",
                                            "Systematic sampling" = "regular")),
                 sliderInput(inputId = "select_sample_size",
                             label = "Choose number of plots",
                             min = 2, max = 10, value = 5, step = 1),
                 radioButtons(inputId = "select_r_design",
                              label = "Choose response design",
                              choices = list("Fixed area" = "fixed_area",
                                             "Angle count" = "angle_count",
                                             "k-tree" = "k_tree"),
                              selected = "fixed_area"),
                 conditionalPanel("input.select_r_design == 'fixed_area'",
                                  sliderInput(inputId = "select_radius",
                                              label = "Select a plot radius",
                                              min = 5, max = 20, value = 10, step = 1)),
                 conditionalPanel("input.select_r_design == 'angle_count'",
                                  sliderInput(inputId = "select_baf",
                                              label = "Select a basal area factor",
                                              min = 1, max = 8, value = 2, step = 1)),
                 conditionalPanel("input.select_r_design == 'k_tree'",
                                  sliderInput(inputId = "select_k",
                                              label = "Select number of trees",
                                              min = 2, max = 15, value = 6, step = 1)),
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
        tabPanel("Stand", plotlyOutput("stand", width = "auto", height = "800px")),
        tabPanel("Estimates", dataTableOutput("estimates")),
        tabPanel("Distribution of estimates", plotOutput("distribution"))
      )
    )
  )
))
