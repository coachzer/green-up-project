library(shiny)
library(plotly)
library(DT)
library(shinyFeedback)

# Helper function to create a row with two numeric inputs
create_numeric_input_row <- function(input_id1, label1, value1, min1, input_id2, label2, value2, min2) {
  fluidRow(
    column(6, numericInput(input_id1, label1, value1, min = min1)),
    column(6, numericInput(input_id2, label2, value2, min = min2))
  )
}

shinyUI(fluidPage(
  useShinyFeedback(),
  titlePanel("Wood Waste Management Simulation"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("scenario", "Scenario:",
                  choices = list("Default" = "default",
                                 "Increased Waste Generation" = "increased_waste_generation",
                                 "Improved Collection Efficiency" = "improved_collection_efficiency",
                                 "Improved Handling Efficiency" = "improved_handling_efficiency",
                                 "Enhanced Recycling Programs" = "enhanced_recycling_programs",
                                 "Storage Capacity Expansion" = "storage_capacity_expansion",
                                 "Penalties for Overflow" = "penalties_for_overflow",
                                 "Different Transportation Costs" = "different_transportation_costs",
                                 "Seasonal Variations" = "seasonal_variations")),
      textInput("regions", "Regions (comma-separated):", "North,South,East,West"),
      numericInput("num_regions", "Number of Regions:", min = 2, value = 4),
      actionButton("generate_matrix", "Generate Distance Matrix"),
      hr(),
      textInput("wood_waste_types", "Wood Waste Types (comma-separated):", "Construction,Demolition,Packaging"),
      hr(),
      create_numeric_input_row("generation_rate_increase", "Adjust Generation Multiplier:", 1, 0,
                               "collection_rate_increase", "Adjust Collection Multiplier:", 1, 0),
      create_numeric_input_row("handling_rate_increase", "Adjust Handling Multiplier:", 1, 0,
                               "recycling_rate_increase", "Adjust Recycling Multiplier:", 1, 0),
      create_numeric_input_row("storage_cost_per_unit", "Storage Cost per Unit:", 0.5, 0,
                               "collection_cost_per_unit", "Collection Cost per Unit:", 1, 0),
      create_numeric_input_row("processing_cost_per_unit", "Processing Cost per Unit:", 2, 0,
                               "transportation_cost_per_unit", "Transportation Cost per Unit:", 0.3, 0),
      create_numeric_input_row("overflow_penalty_per_unit", "Overflow Penalty per Unit:", 2, 0,
                               "recycling_revenue_per_unit", "Recycling Revenue per Unit:", 3, 0),
      create_numeric_input_row("avoided_disposal_cost_per_unit", "Avoided Disposal Cost per Unit:", 1, 0,
                               "storage_capacity", "Storage Capacity per Region:", 500, 0),
      actionButton("reset_parameters", "Reset Parameters"),
      hr(),
      actionButton("run_simulation", "Run Simulation", class = "btn btn-primary"),
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
        tabPanel("Results Table", DTOutput("results_table")),
        tabPanel("Distance Matrix", tableOutput("distance_matrix"))
      )
    )
  )
))
