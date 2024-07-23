library(shiny)
library(plotly)
library(DT)
library(shinyFeedback)
library(shinydashboard)
library(bslib)
library(igraph)

# Helper function to create a row with two numeric inputs
create_numeric_input_row <- function(input_id1, label1, value1, min1, input_id2, label2, value2, min2, max = 2.0, step1 = 0.1, step2 = 0.1) {
  fluidRow(
    column(6, numericInput(input_id1, label1, value1, min = min1, max = max, step = step1)),
    column(6, numericInput(input_id2, label2, value2, min = min2, max = max, step = step2))
  )
}

shinyUI(dashboardPage(
  dashboardHeader(title = "Wood Waste Management Simulation"),
  dashboardSidebar(
    sidebarMenu( 
      id = "tabs",
      menuItem("Simulation", tabName = "simulation", icon = icon("play")),
      menuItem("Outputs", tabName = "outputs", icon = icon("file-alt")),
      menuItem("Plots", tabName = "plots", icon = icon("chart-line")),
      menuItem("Costs", tabName = "costs", icon = icon("dollar-sign")),
      menuItem("Tables", tabName = "tables", icon = icon("table")),
      menuItem("Distance Matrix", tabName = "distance_matrix", icon = icon("th"))
    )
  ),
  dashboardBody(
    useShinyFeedback(),
    tabItems(
      # Simulation -----
      tabItem(tabName = "simulation",
              fluidRow(
                box(width = 6,
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
                    conditionalPanel(
                      condition = "input.scenario == 'seasonal_variations'",
                      selectInput("season", "Select Season:",
                                  choices = c("Winter", "Spring", "Summer", "Fall", "Year-long"),
                                  selected = "Spring")
                    ),
                    textInput("regions", "Regions (comma-separated):", "North,South,East,West"),
                    # numericInput("num_regions", "Number of Regions:", min = 2, value = 4),
                    textInput("wood_waste_types", "Wood Waste Types (comma-separated):", "Construction,Demolition,Packaging"),
                    # actionButton("generate_matrix", "Generate Distance Matrix", class = "btn btn-secondary")),
                    checkboxGroupInput("selected_plots", "Select Plots to Display:", 
                                       choices = list("Generation" = "generation", 
                                                      "Collection" = "collection", 
                                                      "Handling" = "handling", 
                                                      "Recycling" = "recycling"),
                                       selected = c("generation", "collection", "handling", "recycling"))),
                box(width = 6,
                      create_numeric_input_row("generation_rate_increase", "Adjust Generation Multiplier:", 1.0, 0,
                                               "collection_rate_increase", "Adjust Collection Multiplier:", 1.0, 0),
                      create_numeric_input_row("handling_rate_increase", "Adjust Handling Multiplier:", 1.0, 0,
                                               "recycling_rate_increase", "Adjust Recycling Multiplier:", 1.0, 0),
                      create_numeric_input_row("storage_cost_per_unit", "Storage Cost per Unit:", 0.5, 0,
                                               "collection_cost_per_unit", "Collection Cost per Unit:", 1.0, 0),
                      create_numeric_input_row("processing_cost_per_unit", "Processing Cost per Unit:", 2, 0,
                                               "transportation_cost_per_unit", "Transportation Cost per Unit:", 0.3, 0),
                      create_numeric_input_row("overflow_penalty_per_unit", "Overflow Penalty per Unit:", 2.0, 0,
                                               "recycling_revenue_per_unit", "Recycling Revenue per Unit:", 2.0, 0),
                      create_numeric_input_row("avoided_disposal_cost_per_unit", "Avoided Disposal Cost per Unit:", 1.0, 0,
                                               "storage_capacity", "Storage Capacity per Region:", value2 = 500, min2 = 0, max = 2000, step2 = 10),
                    fluidRow(
                      column(6, actionButton("reset_parameters", "Reset Parameters", class = "btn btn-danger", width = "100%")),
                      column(6, actionButton("run_simulation", "Run Simulation", class = "btn btn-primary", width = "100%"))
                    )
                )
              )
            ),
      # Outputs -----
      tabItem(tabName = "outputs",
              fluidRow(
                box(width = 12, title = "Simulation Outputs", solidHeader = TRUE, status = "primary",
                    DTOutput("simulation_output")
                ),
                box(width = 12, title = "Cumulative Totals", solidHeader = TRUE, status = "primary",
                    DTOutput("cumulative_totals")
                )
              )
      ),
      # Plots -----
      tabItem(tabName = "plots",
              fluidRow(
                box(width = 12, title = "Plots", solidHeader = TRUE, status = "primary",
                    radioButtons("plot_type", "Plot Type:",
                                 choices = list("Cumulative" = "cumulative", "Non-Cumulative" = "non_cumulative"),
                                 selected = "cumulative"),
                    plotlyOutput("generation_plot"),
                    plotlyOutput("collection_plot"),
                    plotlyOutput("handling_plot"),
                    plotlyOutput("recycling_plot")
                )
              )
      ),
      # Costs -----
      tabItem(tabName = "costs",
              fluidRow(
                box(width = 12, title = "Costs Over Time", solidHeader = TRUE, status = "primary",
                    plotlyOutput("collection_cost_plot"),
                    plotlyOutput("processing_cost_plot"),
                    plotlyOutput("transportation_cost_plot"),
                    plotlyOutput("recycling_revenue_plot")
                )
              )
      ),
      # Tables -----
      tabItem(tabName = "tables",
              fluidRow(
                box(width = 12, title = "Results Table", solidHeader = TRUE, status = "primary",
                    DTOutput("results_table")
                ),
                box(width = 12, title = "Costs Table", solidHeader = TRUE, status = "primary",
                    DTOutput("costs_table")
                )
              )
      ),
      # Distance Matrix -----
      tabItem(tabName = "distance_matrix",
              fluidRow(
                box(width = 6, title = "Distance Matrix", solidHeader = TRUE, status = "primary",
                    div(style = "display: flex; flex-direction: column; justify-content: center; align-items: center; height: 100%; text-align: center;",
                        tableOutput("distance_matrix"),
                        br(),  # Add some space between the table and the plot
                        plotlyOutput("force_directed_plot", height = "400px")
                    )
                ),
                box(width = 6, title = "Distance Matrix Plot", solidHeader = TRUE, status = "primary",
                    plotlyOutput("distance_matrix_plot")
                )
              )
      )
    )
  )
)
)
