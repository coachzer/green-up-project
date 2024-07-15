library(shiny)
library(plotly)
library(DT)

shinyUI(fluidPage(
  titlePanel("Wood Waste Management Simulation"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("scenario", "Select Scenario:",
                  choices = list("Default" = "default",
                                 "Increased Waste Generation" = "increased_waste_generation",
                                 "Improved Collection Efficiency" = "improved_collection_efficiency",
                                 "Enhanced Recycling Programs" = "enhanced_recycling_programs",
                                 "Storage Capacity Expansion" = "storage_capacity_expansion",
                                 "Penalties for Overflow" = "penalties_for_overflow",
                                 "Different Transportation Costs" = "different_transportation_costs",
                                 "Seasonal Variations" = "seasonal_variations")),
      uiOutput("dynamic_ui"),
      actionButton("run_simulation", "Run Simulation"),
      hr(),
      checkboxGroupInput("selected_plots", "Select Plots to Display:", 
                         choices = list("Generation" = "generation", 
                                        "Collection" = "collection", 
                                        "Handling" = "handling", 
                                        "Recycling" = "recycling"),
                         selected = c("generation", "collection", "handling", "recycling")),
      radioButtons("plot_type", "Plot Type:",
                   choices = list("Cumulative" = "cumulative", "Non-Cumulative" = "non_cumulative"),
                   selected = "cumulative")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Simulation Output", verbatimTextOutput("simulation_output")),
        tabPanel("Cumulative Totals", verbatimTextOutput("cumulative_totals")),
        tabPanel("Plots", 
                 conditionalPanel(
                   condition = "input.selected_plots.includes('generation')",
                   plotlyOutput("generation_plot")
                 ),
                 conditionalPanel(
                   condition = "input.selected_plots.includes('collection')",
                   plotlyOutput("collection_plot")
                 ),
                 conditionalPanel(
                   condition = "input.selected_plots.includes('handling')",
                   plotlyOutput("handling_plot")
                 ),
                 conditionalPanel(
                   condition = "input.selected_plots.includes('recycling')",
                   plotlyOutput("recycling_plot")
                 )),
        tabPanel("Results Table", DTOutput("results_table"))
      )
    )
  )
))
