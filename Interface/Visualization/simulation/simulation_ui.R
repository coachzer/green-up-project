# Simulation tab bodies — DORMANT. Not included by the running app's ui.R.
# To revive: see Interface/Visualization/simulation/README.md
simulation_tab_items <- function() {
  list(
      # Simulation -----
      tabItem(
        tabName = "hiddenSimulation",
        h1("Description of the Wood Waste Management Simulation")
      ),
      # Define Simulation -----
      tabItem(
        tabName = "define_simulation",
        h1("Define your own Wood Waste Management Simulation"),
        fluidRow(
          # Left column -----
          box(
            width = 6,
            title = "Select a Scenario and Set Parameters",
            solidHeader = TRUE,
            status = "primary",
            selectInput(
              "scenario",
              "Scenario/Presets:",
              choices = list(
                "Default" = "default",
                "Increased Waste Generation" = "increased_waste_generation",
                "Improved Collection Efficiency" = "improved_collection_efficiency",
                "Improved Handling Efficiency" = "improved_handling_efficiency",
                "Enhanced Recycling Programs" = "enhanced_recycling_programs",
                "Storage Capacity Expansion" = "storage_capacity_expansion",
                "Penalties for Overflow" = "penalties_for_overflow",
                "Different Transportation Costs" = "different_transportation_costs",
                "Seasonal Variations" = "seasonal_variations"
              )
            ),
            bsTooltip(
              "scenario",
              "Select a scenario or preset to automatically set the input parameters.",
              placement = "top"
            ),
            conditionalPanel(
              condition = "input.scenario == 'seasonal_variations'",
              selectInput(
                "season",
                "Select Season:",
                choices = c("Winter", "Spring", "Summer", "Fall", "Year-long"),
                selected = "Spring"
              ),
              textOutput("seasonal_multiplier_display"),
              hr()
            ),
            textInput(
              "regions",
              "Regions (comma-separated):",
              "North,South,East,West"
            ),
            bsTooltip(
              "regions",
              "Enter the names of the regions in the simulation, separated by commas."
            ),
            # numericInput("num_regions", "Number of Regions:", min = 2, value = 4),
            textInput(
              "wood_waste_types",
              "Wood Waste Types (comma-separated):",
              "Construction,Demolition,Packaging"
            ),
            bsTooltip(
              "wood_waste_types",
              "Enter the names of the wood waste types in the simulation, separated by commas."
            ),
            numericInput(
              "runtime",
              "Simulation Runtime (time units):",
              value = 50,
              min = 50,
              max = 365,
              step = 5,
              width = "50%"
            ),
            bsTooltip(
              "runtime",
              "Enter the number of time units to run the simulation for."
            ),
            # actionButton("generate_matrix", "Generate Distance Matrix", class = "btn btn-secondary")),
            checkboxGroupInput(
              "selected_plots",
              "Select Plots to Display:",
              choices = list(
                "Generation" = "generation",
                "Collection" = "collection",
                "Handling" = "handling",
                "Recycling" = "recycling"
              ),
              selected = c("generation", "collection", "handling", "recycling")
            )
          ),
          # Right column -----
          box(
            width = 6,
            title = "Simulation Parameters",
            solidHeader = TRUE,
            status = "primary",
            create_numeric_input_row(
              input_id1 = "generation_rate_increase",
              label1 = "Generation Rate Multiplier:",
              value1 = 1.0,
              min1 = 0,
              tooltip1 = "Multiplier for the generation rate of wood waste in each region.",
              input_id2 = "collection_rate_increase",
              label2 = "Collection Rate Multiplier:",
              value2 = 1.0,
              min2 = 0,
              tooltip2 = "Multiplier for the collection rate of wood waste in each region."
            ),
            create_numeric_input_row(
              input_id1 = "handling_rate_increase",
              label1 = "Handling Rate Multiplier:",
              value1 = 1.0,
              min1 = 0,
              tooltip1 = "Multiplier for the handling rate of wood waste in each region.",
              input_id2 = "recycling_rate_increase",
              label2 = "Recycling Rate Multiplier:",
              value2 = 1.0,
              min2 = 0,
              tooltip2 = "Multiplier for the recycling rate of wood waste in each region."
            ),
            create_numeric_input_row(
              input_id1 = "storage_cost_per_unit",
              label1 = "Storage Cost per Unit:",
              value1 = 0.5,
              min1 = 0,
              tooltip1 = "Cost of storing wood waste in each region.",
              input_id2 = "collection_cost_per_unit",
              label2 = "Collection Cost per Unit:",
              value2 = 1.0,
              min2 = 0,
              tooltip2 = "Cost of collecting wood waste in each region."
            ),
            create_numeric_input_row(
              input_id1 = "processing_cost_per_unit",
              label1 = "Processing Cost per Unit:",
              value1 = 2.0,
              min1 = 0,
              tooltip1 = "Cost of processing wood waste in each region.",
              input_id2 = "transportation_cost_per_unit",
              label2 = "Transportation Cost per Unit:",
              value2 = 0.3,
              min2 = 0,
              tooltip2 = "Cost of transporting wood waste between regions."
            ),
            create_numeric_input_row(
              input_id1 = "overflow_penalty_per_unit",
              label1 = "Overflow Penalty per Unit:",
              value1 = 2.0,
              min1 = 0,
              max1 = 5,
              tooltip1 = "Penalty for overflowing wood waste in each region.",
              input_id2 = "recycling_revenue_per_unit",
              label2 = "Recycling Revenue per Unit:",
              value2 = 2.0,
              min2 = 0,
              max2 = 5,
              tooltip2 = "Revenue from recycling wood waste in each region."
            ),
            create_numeric_input_row(
              input_id1 = "avoided_disposal_cost_per_unit",
              label1 = "Avoided Disposal Cost per Unit:",
              value1 = 1.0,
              min1 = 0,
              tooltip1 = "Cost savings from avoiding disposal of wood waste in each region.",
              input_id2 = "storage_capacity",
              label2 = "Storage Capacity per Region:",
              value2 = 500,
              min2 = 0,
              max2 = 2000,
              step2 = 10,
              tooltip2 = "Maximum amount of wood waste that can be stored in each region."
            ),
            fluidRow(column(
              6,
              actionButton(
                "reset_parameters",
                "Reset Parameters",
                class = "btn btn-danger",
                width = "100%"
              )
            ), column(
              6,
              actionButton(
                "run_simulation",
                "Run Simulation",
                class = "btn btn-primary",
                width = "100%"
              )
            ))
          )
        )
      ),
      # Outputs -----
      tabItem(tabName = "outputs", fluidRow(
        uiOutput("simulation_notice"),
        box(
          collapsible = TRUE,
          width = 12,
          title = "Simulation Outputs",
          solidHeader = TRUE,
          status = "primary",
          fluidRow(
            column(
              6,
              selectizeInput(
                "selected_region",
                "Select Region:",
                choices = NULL,
                multiple = TRUE,
                options = list(plugins = list("remove_button", "drag_drop"))
              )
            ),
            column(
              6,
              selectizeInput(
                "selected_waste_type",
                "Select Waste Type:",
                choices = NULL,
                multiple = TRUE,
                options = list(plugins = list("remove_button"))
              )
            )
          ),
          fluidRow(
            column(
              12,
              selectizeInput(
                "selected_metric",
                "Select Metric:",
                choices = NULL,
                multiple = TRUE,
                options = list(plugins = list("remove_button"))
              )
            )
          ),
          DTOutput("simulation_output")
        ),
        box(
          collapsible = TRUE,
          width = 6,
          title = "Cumulative Totals",
          solidHeader = TRUE,
          status = "primary",
          DTOutput("cumulative_totals")
        )
      )),
      # Plots of Data -----
      tabItem(tabName = "plots", fluidRow(
        box(
          collapsible = TRUE,
          width = 12,
          title = "Filter By",
          solidHeader = TRUE,
          status = "primary",
          # column(
          #   3, 
          #   numericInput(
          #     "time_start", 
          #     "Start Time:", 
          #     value = 0, 
          #     min = 0
          # )),
          # column(
          #   3,
          #   numericInput(
          #     "time_end",
          #     "Time End:",
          #     value = 100,
          #     min = 0,
          #     max = 365
          #   )
          # ),
          radioButtons(
            "plot_type",
            "Plot Type:",
            choices = list("Cumulative" = "cumulative", "Non-Cumulative" = "non_cumulative"),
            selected = "cumulative"
          ),
          selectInput(
            "regions_selected",
            "Select Regions:",
            choices = NULL,
            selected = NULL,
            multiple = TRUE
          ),
          selectInput(
            "waste_types_selected",
            "Select Waste Types:",
            choices = NULL,
            selected = NULL,
            multiple = TRUE
          )
        ),
        box(
          collapsible = TRUE,
          width = 12,
          title = "Plots",
          solidHeader = TRUE,
          status = "primary",
          plotlyOutput("generation_plot"),
          plotlyOutput("collection_plot"),
          plotlyOutput("handling_plot"),
          plotlyOutput("recycling_plot")
        )
      )),
      # Tables of Data -----
      tabItem(tabName = "tables", fluidRow(
        box(
          collapsible = TRUE,
          width = 12,
          title = "Results Table",
          solidHeader = TRUE,
          status = "primary",
          DTOutput("results_table"),
          column(
            6,
            downloadButton("download_data", "Download Data", width = "100%")
          ),
          #column(6, downloadButton("download_report", "Download Report", width = "100%"))
        ),
        box(
          collapsible = TRUE,
          width = 12,
          title = "Costs Table",
          solidHeader = TRUE,
          status = "primary",
          DTOutput("costs_table")
        )
      )),
      # Costs -----
      tabItem(tabName = "costs", fluidRow(
        box(
          collapsible = TRUE,
          width = 12,
          title = "Costs Over Time",
          solidHeader = TRUE,
          status = "primary",
          plotlyOutput("collection_cost_plot"),
          plotlyOutput("processing_cost_plot"),
          plotlyOutput("transportation_cost_plot"),
          plotlyOutput("recycling_revenue_plot")
        )
      )),
      # Distance Matrix -----
      tabItem(tabName = "distance_matrix", fluidRow(
        box(
          collapsible = TRUE,
          width = 6,
          title = "Distance Matrix",
          solidHeader = TRUE,
          status = "primary",
          div(
            style = "display: flex; flex-direction: column; justify-content: center; align-items: center; height: 100%; text-align: center;",
            tableOutput("distance_matrix"),
            br(),
            # Add some space between the table and the plot
            plotlyOutput("force_directed_plot", height = "400px")
          )
        ),
        box(
          collapsible = TRUE,
          width = 6,
          title = "Distance Matrix Plot",
          solidHeader = TRUE,
          status = "primary",
          plotlyOutput("distance_matrix_plot")
        )
      ))

  )
}
