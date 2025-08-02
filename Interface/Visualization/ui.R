# install.packages("shiny", "plotly", "DT", "shinyFeedback", "shinydashboard", "shinyjs", "shinyBS", "bslib", "igraph")

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

shinyUI(
  dashboardPage(
  dashboardHeader(title = "Wood Waste Management"),
  # Dashboard Sidebar -----
  dashboardSidebar(sidebarMenu(
    id = "tabs",
    # Hidden tabs -----
    ## Analysis -----
    # add information if needed for the hidden tabs, then remove the #'s from the lines below
    # hidden(
    #   menuItem(
    #     "hiddenAnalysis",
    #     tabName = "hiddenAnalysis",
    #     icon = icon("chart-simple")
    #   )
    # ),
    # ## Simulation -----
    # hidden(
    #   menuItem(
    #     "hiddenSimulation",
    #     tabName = "hiddenSimulation",
    #     icon = icon("play")
    #   )
    # ),
    # Main tabs -----
    ## Analysis -----
    menuItem(
      "Analysis",
      id = "analysisID",
      tabName = "analysis",
      expandedName = "ANALYSIS",
      icon = icon("chart-simple"),
      startExpanded = TRUE,
      ### Sub-items -----
      #### Overview -----
      menuSubItem(
        HTML("&nbsp;&nbsp;Overview"),
        tabName = "overview",
        icon = icon("eye")
      ),
      #### Generation -----
      menuSubItem(
        HTML("&nbsp;&nbsp;Generation"),
        tabName = "generation",
        icon = icon("trash-arrow-up")
      ),
      #### Collection -----
      menuSubItem(
        HTML("&nbsp;&nbsp;Collection"),
        tabName = "collection",
        icon = icon("truck-arrow-right")
      ),
      #### Treatment -----
      menuSubItem(
        HTML("&nbsp;&nbsp;Treatment"),
        tabName = "treatment",
        icon = icon("hand-holding-heart")
      )
    ) #,
    ## Simulation -----
  #   menuItem(
  #     "Simulation",
  #     id = "simulationID",
  #     expandedName = "SIMULATION",
  #     tabName = "simulation",
  #     icon = icon("play"),
  #     ### Sub-items -----
  #     #### Define Simulation -----
  #     menuSubItem(
  #       "Define Simulation",
  #       tabName = "define_simulation",
  #       icon = icon("pen-to-square")
  #     ),
  #     #### Outputs -----
  #     menuSubItem("Outputs", tabName = "outputs", icon = icon("file-alt")),
  #     #### Plots of Data -----
  #     menuSubItem(
  #       "Plots of Data",
  #       tabName = "plots",
  #       icon = icon("chart-line")
  #     ),
  #     #### Tables of Data -----
  #     menuSubItem("Tables of Data", tabName = "tables", icon = icon("table")),
  #     #### Costs -----
  #     menuSubItem(
  #       "Costs of Management",
  #       tabName = "costs",
  #       icon = icon("dollar-sign")
  #     ),
  #     #### Distance Matrix -----
  #     menuSubItem("Distance Matrix", tabName = "distance_matrix", icon = icon("th"))
  #   )
  )), 
  # # Dashboard Body -----
  dashboardBody(
    includeCSS("data/styles.css"),
    useShinyFeedback(),
    useShinyjs(),
    tabItems(
      # Analysis -----
      tabItem(
        tabName = "hiddenAnalysis",
        h1("Analysis of Wood Waste Management")
      ),
      # Overview ----
      tabItem(
        tabName = "overview",
        h1("Overview of Wood Waste Management in Slovenia"),
        # Info Boxes ----
        fluidRow(
          class = "info-boxes-row",
          infoBoxOutput("totalWasteGeneration"),
          infoBoxOutput("totalWasteCollected"),
          infoBoxOutput("totalWasteForProcessing")
        ),
        # Combined plot of generation, collection and treatment ----
        fluidRow(
          box(
            collapsible = TRUE,
            collapsed = FALSE,
            width = 12,
            title = "Combined Waste Management Metrics",
            solidHeader = TRUE,
            status = "primary",
            plotlyOutput("overviewPlot", height = "600px")
          )
        ),
        fluidRow(
          box(
            collapsible = TRUE,
            collapsed = FALSE,
            width = 12,
            title = "Waste Flow Overview by Category",
            solidHeader = TRUE,
            status = "primary",
            plotlyOutput("overviewPlotSankeyCategory", height = "600px")
          )
        ),
        # fluidRow(
        #   box(
        #     collapsible = TRUE,
        #     collapsed = FALSE,
        #     width = 12,
        #     title = "Waste Flow Overview by Type",
        #     solidHeader = TRUE,
        #     status = "primary",
        #     plotlyOutput("overviewPlotSankeyType", height = "600px")
        #   )
        # ),
        fluidRow(
          box(
            collapsible = TRUE,
            collapsed = FALSE,
            width = 12,
            title = "Comparison based on the Statistical Region",
            solidHeader = TRUE,
            status = "primary",
            plotlyOutput("overviewPlotRegion", height = "600px")
          )
        ),
        # fluidRow(
        #   box(
        #     collapsible = TRUE,
        #     collapsed = FALSE,
        #     width = 12,
        #     title = "Waste Category vs. Year",
        #     solidHeader = TRUE,
        #     status = "primary",
        #     plotlyOutput("overviewPlotCategory", height = "600px")
        #   )
        # ),
        fluidRow(
          box(
            collapsible = TRUE,
            collapsed = FALSE,
            width = 12,
            title = "Comparison based on the Type of Waste",
            solidHeader = TRUE,
            status = "primary",
            plotlyOutput("overviewPlotType", height = "600px")
          )
        )
      ),
      # Generation ----
      tabItem(
        tabName = "generation",
        h1("Generation of Wood Waste"),
        # Info Boxes ----
        fluidRow(
           class = "info-boxes-row",
           infoBoxOutput("totalWasteGenerated"),
           infoBoxOutput("totalTreatedByProducer"),
           infoBoxOutput("totalTransferredRS")
        ),
        fluidRow(
          class = "info-boxes-row-center",
          infoBoxOutput("totalSentEU"),
          infoBoxOutput("totalSentOutsideEU")
        ),
        # Plots ----
        fluidRow(
          box(
            collapsible = TRUE,
            collapsed = FALSE,
            width = 12,
            title = "Combined Waste Management Metrics",
            solidHeader = TRUE,
            status = "primary",
            plotlyOutput("combinedWastePlot")
          )
        ),
        # fluidRow(
        #   box(
        #     collapsible = TRUE,
        #     collapsed = FALSE,
        #     width = 12,
        #     title = "Total Wood Waste Generation by Year",
        #     solidHeader = TRUE,
        #     status = "primary",
        #     plotlyOutput("totalWasteByYear")
        #   )
        # ),
        fluidRow(
          box(
            width = 3,
            solidHeader = TRUE, status = "primary",
            title = "Waste Category Plot Selection",
            # inner container styling
            tags$div(
              style = "background-color: #f8f9fa; padding: 20px; border-radius: 10px;
           margin: 10px 0;",
              
              # Plot selection
              tags$div(
                style = "margin-bottom: 25px;",
                tags$label(
                  tags$span(icon("chart-area", style = "margin-right: 8px;"),
                            style = "color: #3498db;"),
                  "Choose a Plot:",
                  style = "font-weight: bold; color: #2c3e50;
               display: block; margin-bottom: 10px; font-size: 14px;"
                ),
                selectInput(
                  "plot_selection_waste_category", label = NULL,
                  choices = c("Waste by Category Over Time (Line Chart)" = "line",
                              "Total Waste by Category (Pie Chart)" = "pie"),
                  selected = "line",
                  width = "100%"
                )
              )
            )
          ),
          box(
            collapsible = TRUE,
            collapsed = FALSE,
            width = 9,
            title = "Total Wood Waste Generation by Waste Category",
            solidHeader = TRUE,
            status = "primary",
            plotlyOutput("wasteCategoryPlot", height = "600px")
          )
        ),
        fluidRow(
          box(
            collapsible = TRUE,
            collapsed = FALSE,
            width = 12,
            title = "Total Wood Waste Generation by Region and Year",
            solidHeader = TRUE,
            status = "primary",
            plotlyOutput("wasteByRegionYear")
          )
        ),
        fluidRow(
          box(
            collapsible = TRUE,
            collapsed = FALSE,
            width = 12,
            title = "Total Wood Waste Generation by Type and Year",
            solidHeader = TRUE,
            status = "primary",
            plotlyOutput("wasteByTypeYear")
          )
        ),
        # Section Treatment -----
        fluidRow(
          column(12,
                 h3("Waste Treatment Analysis", style = "color: #3c8dbc; margin-bottom: 5px;"),
                 p("This section is focused on wood waste that has been transferred for treatment, 
      distinct from the previous charts showing overall waste generation patterns.", 
                   style = "margin-bottom: 20px; color: #666;")
          )
        ),
        fluidRow(
          box(
            collapsible = TRUE,
            collapsed = FALSE,
            width = 12,
            title = "Total Wood Waste Transferred for Treatment by Year",
            solidHeader = TRUE,
            status = "primary",
            plotlyOutput("wasteTransferred")
          )
        ),
        fluidRow(
          box(
            collapsible = TRUE,
            collapsed = FALSE,
            width = 12,
            title = "Total Wood Waste Transferred for Treatment by Region and Year",
            solidHeader = TRUE,
            status = "primary",
            plotlyOutput("wasteTransferredByRegionYear")
          )
        ),
        fluidRow(
          box(
            collapsible = TRUE,
            collapsed = FALSE,
            width = 12,
            title = "Total Wood Waste Transferred for Treatment by Type and Year",
            solidHeader = TRUE,
            status = "primary",
            plotlyOutput("wasteTransferredByTypeYear")
          )
        ),
        # Section Storage -----
        fluidRow(
          column(12,
                 h3("Waste Storage Analysis", style = "color: #3c8dbc; margin-bottom: 5px;"),
                 p("This section comapares wood waste inventory levelsat the start and the end of the year.", 
                   style = "margin-bottom: 20px; color: #666;")
          )
        ),
        fluidRow(
          box(
            collapsible = TRUE,
            collapsed = FALSE,
            width = 12,
            title = "Wood Waste Storage Comparison",
            solidHeader = TRUE,
            status = "primary",
            plotlyOutput("wasteStorage")
          )
        ),
        # Section End of Year -----
        fluidRow(
          column(12,
                 h3("Waste End of Year Storage Analysis", style = "color: #3c8dbc; margin-bottom: 5px;"),
                 p("This section examines wood waste inventory levels - the amount of waste stored at facilities 
      at the end of each year.", 
                   style = "margin-bottom: 20px; color: #666;")
          )
        ),
        fluidRow(
          box(
            collapsible = TRUE,
            collapsed = FALSE,
            width = 12,
            title = "Total Wood Waste Stored at the End of the Year",
            solidHeader = TRUE,
            status = "primary",
            plotlyOutput("wasteStoredEndYear")
          )
        ),
        fluidRow(
          box(
            collapsible = TRUE,
            collapsed = FALSE,
            width = 12,
            title = "Total Wood Waste Stored at the End of the Year by Region and Year",
            solidHeader = TRUE,
            status = "primary",
            plotlyOutput("wasteStoredEndYearByRegionYear")
          )
        ),
        fluidRow(
          box(
            collapsible = TRUE,
            collapsed = FALSE,
            width = 12,
            title = "Total Wood Waste Stored at the End of the Year by Type and Year",
            solidHeader = TRUE,
            status = "primary",
            plotlyOutput("wasteStoredEndYearByTypeYear")
          )
        ),
        # Section Start of Year -----
        fluidRow(
          column(12,
                 h3("Waste Start of Year Storage Analysis", style = "color: #3c8dbc; margin-bottom: 5px;"),
                 p("This section examines wood waste inventory levels - the amount of waste stored at facilities 
      at the start of each year.", 
                   style = "margin-bottom: 20px; color: #666;")
          )
        ),
        fluidRow(
          box(
            collapsible = TRUE,
            collapsed = FALSE,
            width = 12,
            title = "Total Wood Waste Stored at the Start of the Year",
            solidHeader = TRUE,
            status = "primary",
            plotlyOutput("wasteStoredStartYear")
          )
        ),
        fluidRow(
          box(
            collapsible = TRUE,
            collapsed = FALSE,
            width = 12,
            title = "Total Wood Waste Stored at the Start of the Year by Region and Year",
            solidHeader = TRUE,
            status = "primary",
            plotlyOutput("wasteStoredStartYearByRegionYear")
          )
        ),
        fluidRow(
          box(
            collapsible = TRUE,
            collapsed = FALSE,
            width = 12,
            title = "Total Wood Waste Stored at the Start of the Year by Type and Year",
            solidHeader = TRUE,
            status = "primary",
            plotlyOutput("wasteStoredStartYearByTypeYear")
          )
        ),
      ),
      # Collection ----
      tabItem(
        tabName = "collection",
        h1("Collection of Wood Waste"),
        
        # Tabset for different collection views
        tabsetPanel(
          id = "collection_tabs",
          
          # Collection Storage ----
          tabPanel(
            "Storage",
            style = "margin-top: 20px;",
            # Data Table of Waste Collection Storage ----
            # fluidRow(
            #   box(
            #     collapsible = TRUE,
            #     width = 12,
            #     title = "Storage Data",
            #     solidHeader = TRUE,
            #     status = "primary",
            #     DT::dataTableOutput("storageData")
            #   )
            # ),
            # Waste Storage Plot Selection ----
            fluidRow(
              box(
                collapsible = TRUE,
                width = 3,
                solidHeader = TRUE, 
                status = "primary",
                title = "Select Filters:",
                # inner container styling
                tags$div(
                  style = "background-color: #f8f9fa; padding: 20px; border-radius: 10px;
                 margin: 10px 0;",
                  
                  # Year selection
                  tags$div(
                    style = "margin-bottom: 25px;",
                    tags$label(
                      tags$span(icon("calendar-alt", style = "margin-right: 8px;"),
                                style = "color: #2c3e50;"),
                      "Select a Period:",
                      style = "font-weight: bold; color: #2c3e50;
           display: block; margin-bottom: 10px; font-size: 14px;"
                    ),
                    sliderInput(
                      "year_selection_coll_storage",
                      label = NULL,
                      min = 2016,
                      max = 2022,
                      value = c(2016, 2022)
                    )
                  ),
                  
                  # Waste Type selection
                  tags$div(
                    style = "margin-bottom: 25px;",
                    tags$label(
                      tags$span(icon("recycle", style = "margin-right: 8px;"),
                                style = "color: #3498db;"),
                      "Select Waste Type:",
                      style = "font-weight: bold; color: #2c3e50;
                     display: block; margin-bottom: 10px; font-size: 14px;"
                    ),
                    selectizeInput(
                      "type_of_waste_selection_coll_storage", label = NULL,
                      choices = NULL, selected = NULL, multiple = TRUE,
                      options = list(
                        plugins = list("remove_button"),
                        placeholder = "Select types of waste to compare"
                      ),
                      width = "100%"
                    )
                  ),
                  
                  # Region selection
                  tags$div(
                    style = "margin-bottom: 10px;",
                    tags$label(
                      tags$span(icon("map-marker-alt", style = "margin-right: 8px;"),
                                style = "color: #27ae60;"),
                      "Select Region:",
                      style = "font-weight: bold; color: #2c3e50;
                     display: block; margin-bottom: 10px; font-size: 14px;"
                    ),
                    selectizeInput(
                      "region_selection_coll_storage", label = NULL,
                      choices = NULL, selected = NULL, multiple = TRUE,
                      options = list(
                        plugins = list("remove_button"),
                        placeholder = "Select regions to compare"
                      ),
                      width = "100%"
                    )
                  )
                )
              ),
              box(
                collapsible = TRUE, 
                width = 9,
                solidHeader = TRUE, 
                status = "primary",
                title = "Comparison of Waste Stored at Year's End and Next Year's Start",
                plotlyOutput("selectedPlot1", height = "600px")
              )
            )
          ),
          # Collection Received ----
          tabPanel("Received",
                   # Info Boxes ----
                   # add space between
                   fluidRow(
                     class = "info-boxes-row-collection",
                     infoBoxOutput("totalReceivedWaste"),
                     infoBoxOutput("highestWasteProducingRegion")
                   ),
                   fluidRow(
                     class = "info-boxes-row-center",
                     infoBoxOutput("totalWasteFromProducersNoRecord"),
                     infoBoxOutput("totalWasteFromProducers"),
                     infoBoxOutput("totalWasteFromCollectors"),
                     infoBoxOutput("totalWasteFromProcessors")
                   ),
                   # Custom CSS styles ----
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
                   # Waste Received by Year ----
                   fluidRow(
                     box(
                       collapsible = TRUE,
                       width = 12,
                       title = "Total Wood Waste Received by Year by Source",
                       solidHeader = TRUE,
                       status = "primary",
                       plotlyOutput("totalWasteReceivedByYear")
                     )
                   ),
                   # Waste Received by Region ----
                   fluidRow(
                     # Add a dropdown menu for plot selection
                     box(
                       collapsible = TRUE,
                       title = "Waste Data Plot Selection",
                       solidHeader = TRUE,
                       status = "primary",
                       width = 3,
                       
                       # Background
                       tags$div(
                         style = "background-color: #f8f9fa; padding: 20px; border-radius: 10px; margin: 10px 0;",
                         
                         # Plot type selection section
                         tags$div(
                           style = "margin-bottom: 25px;",
                           tags$label(
                             tags$span(icon("chart-line"), style = "color: #3498db; margin-right: 8px;"),
                             "Choose a Plot Type:",
                             style = "font-weight: bold; color: #2c3e50; display: block; margin-bottom: 10px; font-size: 14px;"
                           ),
                           selectInput(
                             inputId = "plot_type_coll_received", 
                             label = NULL,
                             choices = c("Stacked Bar Plot" = "stacked", 
                                         "Faceted Bar Plot" = "faceted", 
                                         "Grouped Bar Plot" = "grouped"),
                             selected = "stacked",
                             width = "100%"
                           ),
                           tags$small(
                             icon("info-circle"), 
                             "Select visualization style",
                             style = "color: #6c757d; font-style: italic;"
                           )
                         ),
                       
                         # Statistical regions selection section
                         tags$div(
                           style = "margin-bottom: 25px;",
                           tags$label(
                             tags$span(icon("map-marked-alt"), style = "color: #27ae60; margin-right: 8px;"),
                             "Select Statistical Regions:",
                             style = "font-weight: bold; color: #2c3e50; display: block; margin-bottom: 10px; font-size: 14px;"
                           ),
                           selectizeInput(
                             inputId = "region_filter", 
                             label = NULL,
                             choices = NULL,  
                             multiple = TRUE,
                             options = list(plugins = list("remove_button"), placeholder = "Select Statistical Regions"),
                             width = "100%"
                           ),
                           tags$small(
                             icon("info-circle"), 
                             "Choose one or more regions to analyze",
                             style = "color: #6c757d; font-style: italic;"
                           )
                         ),
                       
                         # Sources selection section
                         tags$div(
                           style = "margin-bottom: 10px;",
                           tags$label(
                             tags$span(icon("list"), style = "color: #e74c3c; margin-right: 8px;"),
                             "Select Sources:",
                             style = "font-weight: bold; color: #2c3e50; display: block; margin-bottom: 10px; font-size: 14px;"
                           ),
                           selectizeInput(
                             inputId = "source_filter", 
                             label = NULL,
                             choices = c("From Producers (No Record)", 
                                         "From Producers (With Record)", 
                                         "From Collectors (RS)", 
                                         "From Processors (RS)"),
                             selected = c("From Producers (No Record)", 
                                          "From Producers (With Record)", 
                                          "From Collectors (RS)", 
                                          "From Processors (RS)"), 
                             multiple = TRUE,
                             options = list(plugins = list("remove_button"), placeholder = "Select Sources"),
                             width = "100%"
                           ),
                           tags$small(
                             icon("info-circle"), 
                             "Filter by waste source categories",
                             style = "color: #6c757d; font-style: italic;"
                           )
                         )
                       )
                     ),
                     box(
                       collapsible = TRUE,
                       title = "Waste Data Plot",
                       solidHeader = TRUE,
                       status = "primary",
                       width = 9,
                       plotlyOutput("selectedPlot2", height = "700px")
                     )
                   )
          ),
          # Collection Municipal ----
          tabPanel("Municipal",
                   style = "margin-top: 20px;",
                   fluidRow(
                     column(
                       width = 3,  # Left column: Filter options first, then the InfoBox
                       fluidRow(
                         box(
                           collapsible = TRUE,
                           width = 12,
                           title = "Filter Options",
                           solidHeader = TRUE,
                           status = "primary",
                           
                           # Background
                           tags$div(
                             style = "background-color: #f8f9fa; padding: 20px; border-radius: 10px; margin: 10px 0;",
                           
                           # Region selection section
                           tags$div(
                             style = "margin-bottom: 25px;",
                             tags$label(
                               tags$span(icon("map-marker-alt"), style = "color: #3498db; margin-right: 8px;"),
                               "Statistical Region:",
                               style = "font-weight: bold; color: #2c3e50; display: block; margin-bottom: 10px; font-size: 14px;"
                             ),
                             selectInput("region1", label = NULL, choices = NULL, width = "100%"),
                             tags$small(
                               icon("info-circle"), 
                               "Filter data by geographic area",
                               style = "color: #6c757d; font-style: italic;"
                             )
                           ),
                           
                           # Waste type selection section
                           tags$div(
                             style = "margin-bottom: 25px;",
                             tags$label(
                               tags$span(icon("recycle"), style = "color: #27ae60; margin-right: 8px;"),
                               "Select Type of Waste:",
                               style = "font-weight: bold; color: #2c3e50; display: block; margin-bottom: 10px; font-size: 14px;"
                             ),
                             selectInput("waste_type", label = NULL, choices = NULL, width = "100%"),
                             tags$small(
                               icon("info-circle"), 
                               "Choose specific waste type",
                               style = "color: #6c757d; font-style: italic;"
                             )
                           ),
                           
                           # Municipality count section
                           tags$div(
                             style = "margin-bottom: 10px;",
                             tags$label(
                               tags$span(icon("list-ol"), style = "color: #e74c3c; margin-right: 8px;"),
                               "Number of Municipalities to Display:",
                               style = "font-weight: bold; color: #2c3e50; display: block; margin-bottom: 10px; font-size: 14px;"
                             ),
                             numericInput("top_n_municipal", label = NULL, value = 10, min = 1, max = 50, width = "100%"),
                             tags$small(
                               icon("info-circle"), 
                               "Limit results to top performers (Max: 10)",
                               style = "color: #6c757d; font-style: italic;"
                             )
                           )
                           )
                         )
                       ),
                       fluidRow(
                         column(
                           width = 12,
                           div(
                             style = "margin-top: 20px; padding: 10px;",
                             infoBoxOutput("maxWasteMunicipality", width = 12)  # InfoBox comes after filtering
                           )
                         )
                       )
                     ),
                     column(
                       width = 9,  # Right column: Plot output
                       fluidRow(
                         box(
                           collapsible = TRUE,
                           width = 12,
                           title = "Waste Collected by Municipality (2018-2023)",
                           solidHeader = TRUE,
                           status = "primary",
                           plotlyOutput("municipalComparison", height = "600px")
                         )
                       )
                     )
                   )
          ),
          # Collection Municipal Collected ----
          tabPanel("Municipal Collected",
                   style = "margin-top: 20px;",
                   fluidRow(
                     box(
                       collapsible = TRUE,
                       width = 12,
                       title = "Total Waste Collected by Year",
                       solidHeader = TRUE,
                       status = "primary",
                       plotlyOutput("plot1")
                     )
                   ),
                   fluidRow(
                     box(
                       width = 3,
                       title = "Filter Options",
                       solidHeader = TRUE,
                       status = "primary",
                       tags$div(
                         style = "background-color: #f8f9fa; padding: 20px; border-radius: 10px; margin: 10px 0;",
                         
                         # Year selection section
                         tags$div(
                           style = "margin-bottom: 25px;",
                           tags$label(
                             tags$span(icon("calendar-alt"), style = "margin-right: 8px;"),
                             "Select Year:",
                             style = "font-weight: bold; color: #2c3e50; display: block; margin-bottom: 10px; font-size: 14px;"
                           ),
                           selectInput(
                             "selected_year", 
                             label = NULL,
                             choices = NULL,
                             width = "100%"
                           ),
                           tags$small(
                             icon("info-circle"), 
                             "Choose the year for analysis",
                             style = "color: #6c757d; font-style: italic;"
                           )
                         ),
                         
                         # Top N municipalities section
                         tags$div(
                           style = "margin-bottom: 20px;",
                           tags$label(
                             tags$span(icon("list-ol"), style = "color: #27ae60; margin-right: 8px;"),
                             "Display Limit:",
                             style = "font-weight: bold; color: #2c3e50; display: block; margin-bottom: 10px; font-size: 14px;"
                           ),
                           numericInput(
                             "top_n", 
                             label = NULL,
                             value = 10, 
                             min = 1, 
                             max = 50,
                             width = "100%"
                           ),
                           tags$small(
                             icon("info-circle"), 
                             "Number of top municipalities to show",
                             style = "color: #6c757d; font-style: italic;"
                           )
                         )
                       )
                     ),
                     box(
                       width = 9,
                       title = paste("Waste Collected by Top Municipality per Year"),
                       solidHeader = TRUE,
                       status = "primary",
                       collapsible = TRUE,
                       plotlyOutput("plot2")
                     )
                   ),
                   fluidRow(
                     box(
                       collapsible = TRUE,
                       width = 12,
                       height      = "600px",
                       title = "Waste Collected by Municipalities (2018-2023)",
                       solidHeader = TRUE,
                       status = "primary",
                       plotlyOutput("plot3", height = "550px")
                     )
                   ),
                   fluidRow(
                     box(
                       collapsible = TRUE,
                       width = 12,
                       title = "Waste Types Collected by Year",
                       solidHeader = TRUE,
                       status = "primary",
                       plotlyOutput("plot4")
                     )
                   ),
                   fluidRow(
                     box(
                       collapsible = TRUE,
                       width = 12,
                       title = "Heatmap of Waste Collection by Region and Year",
                       solidHeader = TRUE,
                       status = "primary",
                       plotlyOutput("plot5")
                     )
                   )
          ),
          
          # Collection Management ----
          tabPanel(
            "Management",
            style = "margin-top: 20px;",
            fluidRow(
              box(
                collapsible = TRUE,
                solidHeader = TRUE,
                status = "primary",
                title = "Total Waste Management Trend (2016-2022)",
                plotlyOutput("totalWasteTrend"),
                width = 12
              )
            ),
            fluidRow(
              box(
                collapsible = TRUE,
                solidHeader = TRUE,
                status = "primary",
                title = "Total Waste Management by Region",
                plotlyOutput("wasteByRegion"),
                width = 12
              )
            ),
            # fluidRow(
            #   box(
            #     collapsible = TRUE,
            #     solidHeader = TRUE,
            #     status = "primary",
            #     title = "Total Waste Management by Type",
            #     plotlyOutput("wasteByType"),
            #     width = 12
            #   )
            # ),
            fluidRow(
              box(
                collapsible = TRUE,
                solidHeader = TRUE,
                status = "primary",
                title = "Total Waste Management by Type and Year",
                fluidRow(
                  column(12,
                         tags$div(
                           style = "background-color: #f8f9fa; padding: 15px; border-radius: 8px; margin-bottom: 15px;",
                           tags$h5(
                             icon("filter", style = "margin-right: 8px;"), 
                             "Filter Options",
                             style = "margin-top: 0; color: #2c3e50; font-weight: bold;"
                           ),
                           fluidRow(
                             column(6,
                                    selectInput("yearFilter", 
                                                label = tagList(
                                                  icon("calendar-alt", style = "margin-right: 8px;"),
                                                  "Select Year:"
                                                ),
                                                choices = NULL,  # Will be populated from server
                                                selected = NULL)
                             ),
                             column(6,
                                    tags$div(
                                      style = "margin-top: 25px;",
                                      tags$small(
                                        icon("info-circle"), 
                                        "Choose 'All Years' to see combined totals",
                                        style = "color: #6c757d;"
                                      )
                                    )
                             )
                           )
                         )
                  )
                ),
                tags$div(
                  style = "background-color: white; padding: 10px; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
                  plotlyOutput("wasteByTypeAndYear")
                ),
                width = 12
              )
            ),
            fluidRow(
              column(
                3,
                box(
                  collapsible = TRUE,
                  width = 12,
                  solidHeader = TRUE,
                  status = "primary",
                  title = "Filter Options",
                  tags$div(
                    style = "background-color: #f8f9fa; padding: 15px; border-radius: 8px; margin: 10px 0;",
                    
                    # Region filter
                    tags$div(
                      style = "margin-bottom: 20px;",
                      tags$label(
                        tags$span(icon("map-marker-alt"), style = "color: #3498db; margin-right: 8px;"),
                        "Statistical Region:",
                        style = "font-weight: bold; color: #2c3e50; display: block; margin-bottom: 8px;"
                      ),
                      selectInput(
                        "region", 
                        label = NULL,
                        choices = NULL,
                        width = "100%"
                      ),
                      tags$small(
                        icon("info-circle"), 
                        "Filter data by geographic area",
                        style = "color: #6c757d; font-style: italic;"
                      )
                    ),
                    
                    # Waste type filter
                    tags$div(
                      style = "margin-bottom: 20px;",
                      tags$label(
                        tags$span(icon("recycle"), style = "color: #27ae60; margin-right: 8px;"),
                        "Waste Category:",
                        style = "font-weight: bold; color: #2c3e50; display: block; margin-bottom: 8px;"
                      ),
                      selectInput(
                        "wasteType", 
                        label = NULL,
                        choices = NULL,
                        width = "100%"
                      ),
                      tags$small(
                        icon("info-circle"), 
                        "Choose specific waste type",
                        style = "color: #6c757d; font-style: italic;"
                      )
                    ),
                    
                    # Year range slider
                    tags$div(
                      style = "margin-bottom: 10px;",
                      tags$label(
                        tags$span(icon("calendar-alt"), style = "color: #e74c3c; margin-right: 8px;"),
                        "Time Period:",
                        style = "font-weight: bold; color: #2c3e50; display: block; margin-bottom: 12px;"
                      ),
                      sliderInput(
                        "yearRange",
                        label = NULL,
                        min = 2016,
                        max = 2023,
                        value = c(2016, 2023),
                        step = 1,
                        sep = ""
                      ),
                      tags$small(
                        icon("info-circle"), 
                        "Adjust the analysis timeframe",
                        style = "color: #6c757d; font-style: italic;"
                      )
                    )
                  )
                )
              ),
              
              column(
                9,
                box(
                  collapsible = TRUE,
                  solidHeader = TRUE,
                  status = "primary",
                  title = "Management of Waste Over Time by Region and Type",
                  width = 12,
                  plotlyOutput("detailedPlot", height = "600px")
                )
              )
            )
          )
        )
      ),
      # Treatment ----
      tabItem(
        tabName = "treatment",
        h1("Treatment of Wood Waste"),
        
        # Tabset for different treatment views
        tabsetPanel(
          id = "treatment_tabs",
          
          # Treatment Storage ----
          tabPanel("Storage", 
                   style = "margin-top: 20px;", 
                   fluidRow(
                     box(
                       collapsible = TRUE,
                       width = 3,
                       title = "Select Filters:",
                       solidHeader = TRUE,
                       status = "primary",
                       tags$div(
                         style = "background-color: #f8f9fa; padding: 20px; border-radius: 10px;
       margin: 10px 0;",
                         
                         # Year selection
                         tags$div(
                           style = "margin-bottom: 25px;",
                           tags$label(
                             tags$span(icon("calendar-alt", style = "margin-right: 8px;"),
                                       style = "color: #2c3e50;"),
                             "Select a Period:",
                             style = "font-weight: bold; color: #2c3e50;
           display: block; margin-bottom: 10px; font-size: 14px;"
                           ),
                           sliderInput(
                             "year_range_trt_storage",
                             label = NULL,
                             min = 2016,
                             max = 2022,
                             value = c(2016, 2022)
                           )
                         ),
                         
                         # Waste Type selection
                         tags$div(
                           style = "margin-bottom: 25px;",
                           tags$label(
                             tags$span(icon("recycle", style = "margin-right: 8px;"),
                                       style = "color: #3498db;"),
                             "Select Waste Type:",
                             style = "font-weight: bold; color: #2c3e50;
           display: block; margin-bottom: 10px; font-size: 14px;"
                           ),
                           selectizeInput(
                             "waste_type_trt_storage", label = NULL,
                             choices = NULL, selected = NULL, multiple = TRUE,
                             options = list(
                               plugins = list("remove_button"),
                               placeholder = "Select types of waste to compare"
                             ),
                             width = "100%"
                           )
                         ),
                         
                         # Region selection
                         tags$div(
                           style = "margin-bottom: 10px;",
                           tags$label(
                             tags$span(icon("map-marker-alt", style = "margin-right: 8px;"),
                                       style = "color: #27ae60;"),
                             "Select Region:",
                             style = "font-weight: bold; color: #2c3e50;
           display: block; margin-bottom: 10px; font-size: 14px;"
                           ),
                           selectizeInput(
                             "region_trt_storage", label = NULL,
                             choices = NULL, selected = NULL, multiple = TRUE,
                             options = list(
                               plugins = list("remove_button"),
                               placeholder = "Select regions to compare"
                             ),
                             width = "100%"
                           )
                         )
                       )
                     ), 
                     
                     box(
                       collapsible = TRUE,
                       width = 9,
                       title = "Wood Waste Storage Trends: Start vs End of Year",
                       solidHeader = TRUE,
                       status = "primary",
                       plotlyOutput("storageTrendsStartEnd", height = "600px")
                     )
                   )
          ), 
          
          # Treatment Received ----
          tabPanel("Received",
                   style = "margin-top: 20px;",
                   fluidRow(
                     box(
                       collapsible = TRUE,
                       title = "Total Waste Received Over Years (in tons)",
                       solidHeader = TRUE,
                       status = "primary",
                       width = 12, 
                       plotlyOutput("plot_yearly")
                     )
                   ),
                   fluidRow(
                     box(
                       width = 3,
                       solidHeader = TRUE, status = "primary",
                       title = "Regional Waste Plot Selection",
                       # inner container styling
                       tags$div(
                         style = "background-color: #f8f9fa; 
                         padding: 20px; 
                         border-radius: 10px;
                         margin: 10px 0;",
                         # Plot selection
                         tags$div(
                           style = "margin-bottom: 25px;",
                           tags$label(
                             tags$span(icon("chart-area", style = "margin-right: 8px;"),
                                       style = "color: #3498db;"),
                             "Choose a Plot:",
                             style = "font-weight: bold; color: #2c3e50;
           display: block; margin-bottom: 10px; font-size: 14px;"
                           ),
                           selectInput(
                             "plot_selection_region", label = NULL,
                             choices = c("Bar Chart" = "bar",
                                         "Pie Chart" = "pie"),
                             selected = "bar",
                             width = "100%"
                           )
                         )
                       )
                     ),
                     box(
                       collapsible = TRUE,
                       collapsed = FALSE,
                       width = 9,
                       title = "Total Waste Received by Statistical Region (in tons)",
                       solidHeader = TRUE,
                       status = "primary",
                       plotlyOutput("regionalPlot", height = "600px")
                     )
                   ),
                   fluidRow(
                     box(
                       collapsible = TRUE,
                       title = "Total Waste Received by Type of Waste (in tons)", 
                       solidHeader = TRUE,
                       status = "primary",
                       width = 12, 
                       plotlyOutput("plot_waste_type")
                     )
                   )
          ),
          
          # Treatment Treatment ----
          tabPanel("Treatment", 
                   style = "margin-top: 20px;", 
                   fluidRow(
                     box(
                       collapsible = TRUE,
                       title = "Filter Data:",
                       solidHeader = TRUE,
                       status = "primary",
                       width = 12,
                       fluidRow(
                         column(6,
                                sliderInput(
                                  "year_range_trt_treatment",
                                  "Select Year Range:",
                                  min = 2016,
                                  max = 2023,
                                  value = c(2016, 2023)
                                )
                         ),
                         column(6,
                                selectizeInput(
                                  "category_trt_treatment",
                                  "Select Waste Category",
                                  choices = NULL,
                                  multiple = TRUE
                                )
                         )
                       ),
                       fluidRow(
                         column(6,
                                selectizeInput(
                                  "region_trt_treatment",
                                  "Select Region",
                                  choices = NULL,
                                  multiple = TRUE
                                )
                         ),
                         column(6,
                                selectizeInput(
                                  "trt_process_trt_treatment",
                                  "Select Treatment Process",
                                  choices = NULL,
                                  multiple = TRUE
                                )
                         )
                       ),
                       fluidRow(
                         column(12,
                                selectizeInput(
                                  "waste_type_trt_treatment",
                                  "Select Waste Type",
                                  choices = NULL,
                                  multiple = TRUE
                                )
                         )
                       )
                     )
                   ),
                   fluidRow(
                     box(
                       collapsible = TRUE,
                       title = "Waste Treatment Over Time by Waste Type (in tons)",
                       solidHeader = TRUE,
                       status = "primary",
                       width = 12,
                       plotlyOutput("plot_waste_treatment", height = "500px")
                     )
                   )
          ), 
          
          
          
          # Treatment Municipal Waste Collected ----
          tabPanel("Municipal Waste Collected",
                   style = "margin-top: 20px;",
                   fluidRow(
                     box(
                       collapsible = TRUE,
                       title = "Total Waste Collected by Region (in tons)",
                       solidHeader = TRUE,
                       status = "primary",
                       width = 12,
                       plotlyOutput("regionPlot", height = "600px")
                     )
                   ),
                   fluidRow(
                     box(
                       collapsible = TRUE,
                       title = "Total Waste Collected by Type (in tons)",
                       solidHeader = TRUE,
                       status = "primary",
                       width = 12,
                       plotlyOutput("wasteTypePlot", height = "600px")
                     )
                   ),
                   fluidRow(
                     box(
                       collapsible = TRUE,
                       title = "Average Municipal Waste Collected for Treatment in Slovenia (2018-2021)",
                       solidHeader = TRUE,
                       status = "primary",
                       width = 12,
                       plotlyOutput("wasteMap", height = "600px")
                     )
                   ),
                   fluidRow(
                     box(
                       collapsible = TRUE,
                       title = "Filter Data:",
                       solidHeader = TRUE,
                       status = "primary",
                       width = 3,
                       sliderInput("top_n_trt_municipal", 
                                   "Select Top N Municipalities:",
                                   min = 1, 
                                   max = 10, 
                                   value = 3,
                                   animate = TRUE,
                                   ticks = FALSE
                                   ),
                       sliderInput("year_range_trt_municipal", "Select Year Range:",
                                   min = 2018, max = 2021, value = c(2018, 2021), step = 1, sep = "")
                     ),
                     box(
                       collapsible = TRUE,
                       title = "Waste Collected Over Time by Municipality (in tons)",
                       solidHeader = TRUE,
                       status = "primary",
                       width = 9,
                       plotlyOutput("trendPlot", height = "600px")
                     )
                   )
          ),
          
          # Treatment Input Treatment ----
          tabPanel("Input Treatment", 
                   style = "margin-top: 20px;", fluidRow(
            box(
              collapsible = TRUE,
              title = "Filter Data:",
              solidHeader = TRUE,
              status = "primary",
              width = 3,
              sliderInput(
                "year_filter",
                "Select Year Range:",
                min = 2018,
                max = 2022,
                value = c(2018, 2022)
              ),
              selectizeInput(
                "waste_type_filter",
                "Select Waste Type:",
                choices = NULL,
                multiple = TRUE
              )
            ),
            box(
              collapsible = TRUE,
              title = "Mass Change During Treatment Over the Years by Waste Type (in tons)",
              solidHeader = TRUE,
              status = "primary",
              width = 9,
              plotlyOutput("inputTreatmentByYear"),
            )
          ))
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
  )
))
