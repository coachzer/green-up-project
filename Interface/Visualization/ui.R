library(shiny)
library(plotly)
library(DT)
library(shinyFeedback)
library(shinydashboard)
library(shinyjs)
library(shinyBS)
library(bslib)
library(igraph)

# Helper function to create a row with two numeric inputs
create_numeric_input_row <- function(input_id1,
                                     label1,
                                     value1,
                                     min1,
                                     max1 = 2.0,
                                     tooltip1 = NULL,
                                     input_id2,
                                     label2,
                                     value2,
                                     min2,
                                     max2 = 2.0,
                                     tooltip2 = NULL,
                                     step1 = 0.1,
                                     step2 = 0.1) {
  fluidRow(
    column(
      6,
      numericInput(
        input_id1,
        label1,
        value1,
        min = min1,
        max = max1,
        step = step1
      )
    ),
    bsTooltip(input_id1, tooltip1),
    column(
      6,
      numericInput(
        input_id2,
        label2,
        value2,
        min = min2,
        max = max2,
        step = step2
      )
    ),
    bsTooltip(input_id2, tooltip2)
  )
}

shinyUI(dashboardPage(
  dashboardHeader(title = "Wood Waste Management"),
  # Dashboard Sidebar -----
  dashboardSidebar(sidebarMenu(
    id = "tabs",
    # Hidden tabs -----
    ## Analysis -----
    hidden(
      menuItem(
        "hiddenAnalysis",
        tabName = "hiddenAnalysis",
        icon = icon("chart-simple")
      )
    ),
    ## Simulation -----
    hidden(
      menuItem(
        "hiddenSimulation",
        tabName = "hiddenSimulation",
        icon = icon("play")
      )
    ),
    # Main tabs -----
    ## Analysis -----
    menuItem(
      "Analysis",
      id = "analysisID",
      tabName = "analysis",
      expandedName = "ANALYSIS",
      icon = icon("chart-simple"),
      ### Sub-items -----
      #### Generation -----
      menuSubItem(
        "Generation",
        tabName = "generation",
        icon = icon("trash-arrow-up")
      ),
      #### Collection -----
      menuSubItem(
        "Collection",
        tabName = "collection",
        icon = icon("truck-arrow-right")
      ),
      #### Treatment -----
      menuSubItem(
        "Treatment",
        tabName = "treatment",
        icon = icon("hand-holding-heart")
      )
    ),
    ## Simulation -----
    menuItem(
      "Simulation",
      id = "simulationID",
      expandedName = "SIMULATION",
      tabName = "simulation",
      icon = icon("play"),
      ### Sub-items -----
      #### Define Simulation -----
      menuSubItem(
        "Define Simulation",
        tabName = "define_simulation",
        icon = icon("pen-to-square")
      ),
      #### Outputs -----
      menuSubItem("Outputs", tabName = "outputs", icon = icon("file-alt")),
      #### Plots of Data -----
      menuSubItem(
        "Plots of Data",
        tabName = "plots",
        icon = icon("chart-line")
      ),
      #### Tables of Data -----
      menuSubItem("Tables of Data", tabName = "tables", icon = icon("table")),
      #### Costs -----
      menuSubItem(
        "Costs of Management",
        tabName = "costs",
        icon = icon("dollar-sign")
      ),
      #### Distance Matrix -----
      menuSubItem("Distance Matrix", tabName = "distance_matrix", icon = icon("th"))
    )
  )), 
  # Dashboard Body -----
  dashboardBody(
    useShinyFeedback(),
    useShinyjs(),
    tabItems(
      # Analysis -----
      tabItem(
        tabName = "hiddenAnalysis",
        h1("Analysis of Wood Waste Management")
      ),
      # Generation ----
      tabItem(
        tabName = "generation",
        h2("Generation of Wood Waste"),
        fluidRow(
          infoBoxOutput("totalWasteGenerated"),
          infoBoxOutput("totalTreatedByProducer"),
          infoBoxOutput("totalTransferredRS")
        ),
        fluidRow(
          style = "display: flex; justify-content: center; align-items: center;",
          infoBoxOutput("totalSentEU"),
          infoBoxOutput("totalSentOutsideEU")
        ),
        fluidRow(
          box(
            collapsible = TRUE,
            collapsed = FALSE,
            width = 12,
            title = "Total Wood Waste Generation by Year",
            plotlyOutput("totalWasteByYear")
          )
        ),
        fluidRow(
          box(
            collapsible = TRUE,
            collapsed = FALSE,
            title = "Total Wood Waste Generation by Region and Year",
            plotlyOutput("wasteByRegionYear")
          ),
          box(
            collapsible = TRUE,
            collapsed = FALSE,
            title = "Total Wood Waste Generation by Type and Year",
            plotlyOutput("wasteByTypeYear")
          )
          
        ),
        fluidRow(
          box(
            collapsible = TRUE,
            collapsed = FALSE,
            width = 12,
            title = "Total Wood Waste Transferred for Treatment by Year",
            plotlyOutput("wasteTransferred")
          )
        ),
        fluidRow(
          box(
            collapsible = TRUE,
            collapsed = FALSE,
            title = "Total Wood Waste Transferred for Treatment by Region and Year",
            plotlyOutput("wasteTransferredByRegionYear")
          ),
          box(
            collapsible = TRUE,
            collapsed = FALSE,
            title = "Total Wood Waste Transferred for Treatment by Type and Year",
            plotlyOutput("wasteTransferredByTypeYear")
          )
          
        ),
        fluidRow(
          box(
            collapsible = TRUE,
            collapsed = FALSE,
            width = 12,
            title = "Total Wood Waste Stored at the End of the Year",
            plotlyOutput("wasteStoredEndYear")
          )
        ),
        fluidRow(
          box(
            collapsible = TRUE,
            collapsed = FALSE,
            title = "Total Wood Waste Stored at the End of the Year by Region and Year",
            plotlyOutput("wasteStoredEndYearByRegionYear")
          ),
          box(
            collapsible = TRUE,
            collapsed = FALSE,
            title = "Total Wood Waste Stored at the End of the Year by Type and Year",
            plotlyOutput("wasteStoredEndYearByTypeYear")
          )
          
        ),
      ),
      # Collection ----
      tabItem(
        tabName = "collection",
        h2("Collection of Wood Waste"),
        
        # Tabset for different collection views
        tabsetPanel(
          id = "collection_tabs",
          
          # Collection Storage ----
          tabPanel("Collection Storage",
                   fluidRow(
                     # infoBoxOutput("totalStorageWaste"),
                     # infoBoxOutput("totalStoredByRegion"),
                     # infoBoxOutput("totalStoredByType")
                   ),
                   fluidRow(
                     box(
                       # Select input for choosing the plot
                       selectInput("plot_selection", "Choose a Plot:",
                                   choices = c("Total Wood Waste Over Time", "Total Wood Waste Over Time - Variant"),
                                   selected = "Total Wood Waste Over Time"),  # Default selection
                     )
                   ),
                   fluidRow(
                     box(
                       collapsible = TRUE,
                       width = 12,
                       title = "Selected Plot",
                       plotlyOutput("selectedPlot1")
                     )
                   )
          ),
          
          # Collection Received ----
          tabPanel("Collection Received",
                   # add space between
                   fluidRow(
                     style = "display: flex; justify-content: center; align-items: center; margin-top: 20px;",
                     infoBoxOutput("totalReceivedWaste"),
                     infoBoxOutput("highestWasteProducingRegion")
                   ),
                   fluidRow(
                     style = "display: flex; justify-content: center; align-items: center;",
                     infoBoxOutput("totalWasteFromProducersNoRecord"),
                     infoBoxOutput("totalWasteFromProducers"),
                     infoBoxOutput("totalWasteFromCollectors"),
                     infoBoxOutput("totalWasteFromProcessors")
                   ),
                   # Custom CSS styles
                   tags$style(
                     '
                       #totalWasteFromProducersNoRecord .info-box-icon {
                         background-color: #E69F00 !important; /* Color for waste from producers no record */
                       }
                       #totalWasteFromProducers .info-box-icon {
                         background-color: #56B4E9 !important; /* Color for waste from producers with record */
                       }
                       #totalWasteFromCollectors .info-box-icon {
                         background-color: #009E73 !important; /* Color for waste from collectors */
                       }
                       #totalWasteFromProcessors .info-box-icon {
                         background-color: #F0E442 !important; /* Color for waste from processors */
                       }
                       .info-box-icon {
                         color: white; /* Default text color */
                       }
                     '
                   ),
                   fluidRow(
                     box(
                       collapsible = TRUE,
                       width = 12,
                       title = "Total Wood Waste Received by Year",
                       plotlyOutput("totalWasteReceivedByYear")
                     )
                   ),
                   fluidRow(
                     # Add a dropdown menu for plot selection
                     box(
                       collapsible = TRUE,
                       width = 3,
                       title = "Waste Data Plot Selection",
                       # Plot selection input
                       selectInput(inputId = "plot_type", 
                                   label = "Choose a Plot Type", 
                                   choices = c("Stacked Bar Plot" = "stacked", 
                                               "Faceted Bar Plot" = "faceted", 
                                               "Grouped Bar Plot" = "grouped"),
                                   selected = "stacked"),  # Default is stacked plot
                       
                       # Selectize input for statistical regions
                       selectizeInput(inputId = "region_filter", 
                                      label = "Select Statistical Regions", 
                                      choices = NULL,  # Will be populated from server
                                      multiple = TRUE),
                       
                       # Selectize input for sources
                       selectizeInput(inputId = "source_filter", 
                                      label = "Select Sources", 
                                      choices = c("From Producers (No Record)", 
                                                  "From Producers (With Record)", 
                                                  "From Collectors (RS)", 
                                                  "From Processors (RS)"),
                                      selected = c("From Producers (No Record)", 
                                                   "From Producers (With Record)", 
                                                   "From Collectors (RS)", 
                                                   "From Processors (RS)"), 
                                      multiple = TRUE)
                     ),
                     box(
                       collapsible = TRUE,
                       width = 9,
                       title = "Waste Data Plot",
                       plotlyOutput("selectedPlot2")
                     )
                   )
          ),
          # Collection Municipal ----
          tabPanel("Collection Municipal",
                   fluidRow(
                     # infoBoxOutput("totalMunicipalWaste"),
                     # infoBoxOutput("totalMunicipalByRegion"),
                     # infoBoxOutput("totalMunicipalByType")
                   ),
                   fluidRow(
                     box(
                       collapsible = TRUE,
                       width = 12,
                       title = "Total Municipal Waste Collected by Year",
                       # plotlyOutput("municipalByYear")
                     )
                   )
          ),
          
          # Collection Municipal Collected ----
          tabPanel("Collection Municipal Collected",
                   fluidRow(
                     # infoBoxOutput("totalMunicipalCollected"),
                     # infoBoxOutput("totalCollectedByRegion"),
                     # infoBoxOutput("totalCollectedByType")
                   ),
                   fluidRow(
                     box(
                       collapsible = TRUE,
                       width = 12,
                       title = "Total Municipal Collected by Year",
                       # plotlyOutput("municipalCollectedByYear")
                     )
                   )
          ),
          
          # Collection Management ----
          tabPanel("Collection Management",
                   fluidRow(
                     # infoBoxOutput("totalManagementWaste"),
                     # infoBoxOutput("totalManagementByRegion"),
                     # infoBoxOutput("totalManagementByType")
                   ),
                   fluidRow(
                     box(
                       collapsible = TRUE,
                       width = 12,
                       title = "Management of Collected Waste by Year",
                       # plotlyOutput("managementByYear")
                     )
                   )
          )
        )
      ),
      # Treatment ----
      tabItem(
        tabName = "treatment",
        h2("Treatment of Wood Waste"),
        
        # Tabset for different treatment views
        tabsetPanel(
          id = "treatment_tabs",
          
          # Treatment Storage ----
          tabPanel("Treatment Storage",
                   fluidRow(
                     # infoBoxOutput("totalStorageTreatment"),
                     # infoBoxOutput("totalStoredByRegion"),
                     # infoBoxOutput("totalStoredByType")
                   ),
                   fluidRow(
                     box(
                       collapsible = TRUE,
                       width = 12,
                       title = "Total Wood Waste Stored for Treatment by Year",
                       # plotlyOutput("storedTreatmentByYear")
                     )
                   )
          ),
          
          # Treatment Collected ----
          tabPanel("Treatment Collected",
                   fluidRow(
                     # infoBoxOutput("totalCollectedTreatment"),
                     # infoBoxOutput("totalCollectedByRegion"),
                     # infoBoxOutput("totalCollectedByType")
                   ),
                   fluidRow(
                     box(
                       collapsible = TRUE,
                       width = 12,
                       title = "Total Wood Waste Collected for Treatment by Year",
                       # plotlyOutput("collectedTreatmentByYear")
                     )
                   )
          ),
          
          # Treatment Treatment ----
          tabPanel("Treatment Treatment",
                   fluidRow(
                     # infoBoxOutput("totalTreatedWaste"),
                     # infoBoxOutput("totalTreatedByRegion"),
                     # infoBoxOutput("totalTreatedByType")
                   ),
                   fluidRow(
                     box(
                       collapsible = TRUE,
                       width = 12,
                       title = "Total Wood Waste Treated by Year",
                       # plotlyOutput("treatedByYear")
                     )
                   )
          ),
          
          # Treatment Municipal Waste Received ----
          tabPanel("Treatment Municipal Waste Received",
                   fluidRow(
                     # infoBoxOutput("totalMunicipalReceived"),
                     # infoBoxOutput("totalReceivedByRegion"),
                     # infoBoxOutput("totalReceivedByType")
                   ),
                   fluidRow(
                     box(
                       collapsible = TRUE,
                       width = 12,
                       title = "Total Municipal Waste Received for Treatment by Year",
                       # plotlyOutput("municipalReceivedByYear")
                     )
                   )
          ),
          
          # Treatment Input Treatment ----
          tabPanel("Treatment Input Treatment",
                   fluidRow(
                     # infoBoxOutput("totalInputTreatment"),
                     # infoBoxOutput("totalInputByRegion"),
                     # infoBoxOutput("totalInputByType")
                   ),
                   fluidRow(
                     box(
                       collapsible = TRUE,
                       width = 12,
                       title = "Input Treatment Data by Year",
                       # plotlyOutput("inputTreatmentByYear")
                     )
                   )
          )
        )
      ),
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
            h3(
              "Scenarios lock one or more of the parameters based on the selected scenario."
            ),
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
        box(
          width = 12,
          title = "Simulation Outputs",
          solidHeader = TRUE,
          status = "primary",
          selectInput(
            "selected_region",
            "Select Region:",
            choices = NULL,
            multiple = TRUE
          ),
          selectInput(
            "selected_waste_type",
            "Select Waste Type:",
            choices = NULL,
            multiple = TRUE
          ),
          selectInput(
            "selected_metric",
            "Select Metric:",
            choices = NULL,
            multiple = TRUE
          ),
          DTOutput("simulation_output")
        ),
        box(
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
          width = 6,
          title = "Distance Matrix Plot",
          solidHeader = TRUE,
          status = "primary",
          plotlyOutput("distance_matrix_plot")
        )
      ))
    )
  )
))
