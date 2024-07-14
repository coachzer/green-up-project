library(shiny)
library(plotly)
library(DT)

shinyUI(fluidPage(
  titlePanel("Wood Waste Management Simulation"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("regions", "Regions (comma-separated):", "North,South,East,West"),
      textInput("wood_waste_types", "Wood Waste Types (comma-separated):", "Construction,Demolition,Packaging"),
      numericInput("storage_cost_per_unit", "Storage Cost per Unit:", 0.5),
      numericInput("collection_cost_per_unit", "Collection Cost per Unit:", 1),
      numericInput("processing_cost_per_unit", "Processing Cost per Unit:", 2),
      numericInput("transportation_cost_per_unit", "Transportation Cost per Unit:", 0.3),
      numericInput("overflow_penalty_per_unit", "Overflow Penalty per Unit:", 2),
      numericInput("recycling_revenue_per_unit", "Recycling Revenue per Unit:", 3),
      numericInput("avoided_disposal_cost_per_unit", "Avoided Disposal Cost per Unit:", 1),
      numericInput("storage_capacity", "Storage Capacity per Region:", 500),
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
