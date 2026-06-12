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
    )
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
                      max = 2030,
                      value = c(2016, 2030),
                      sep = ""
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
                           title = "Waste Collected by Municipality (2018-2024)",
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
                       title = "Waste Collected by Municipalities (2018-2024)",
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
                title = "Total Waste Management Trend (2016-2024)",
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
                        max = 2030,
                        value = c(2016, 2030),
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
                       width = 2,
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
                             max = 2030,
                             value = c(2016, 2030),
                             sep = ""
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
                       width = 10,
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
                       width = 2,
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
                       width = 10,
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
                                  max = 2030,
                                  value = c(2016, 2030),
                                  sep = ""
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
                                   min = 2016, max = 2030, value = c(2016, 2030), step = 1, sep = "")
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
              width = 2,
              sliderInput(
                "year_filter",
                "Select Year Range:",
                min = 2016,
                max = 2030,
                value = c(2016, 2030),
                sep = ""
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
              width = 10,
              plotlyOutput("inputTreatmentByYear", height = "600px"),
            )
          )),
          # Treatment Management ----
          tabPanel("Management",
                   style = "margin-top: 20px;",
                   fluidRow(
                     column(2,
                            # --- Sidebar filter box ---
                            box(
                              title = "Filters", collapsible = TRUE, solidHeader = TRUE,
                              status = "primary", width = NULL,
                              
                              radioButtons("dataset_selector", "Dataset:",
                                           choices = c("2020–2024 Data"              = "data_2020_2024",
                                                       "2022–2024 with Operations"   = "data_2022_2024"),
                                           selected = "data_2020_2024"
                              ),
                              hr(),
                              sliderInput("year_filter_mgmt", "Year range:",
                                          min = 2016, max = 2030, value = c(2016, 2030), sep = ""
                              ),
                              hr(),
                              selectizeInput("waste_type_filter_mgmt", "Input waste type:",
                                             choices = NULL, multiple = TRUE
                              ),
                              selectizeInput("region_filter_mgmt", "Statistical region:",
                                             choices = NULL, multiple = TRUE
                              ),
                              hr(),
                              conditionalPanel(
                                condition = "input.dataset_selector == 'data_2022_2024'",
                                selectizeInput("treatment_operation_filter", "Treatment operation:",
                                               choices = NULL, multiple = TRUE
                                ),
                                selectizeInput("further_treatment_filter", "Further treatment:",
                                               choices = NULL, multiple = TRUE
                                )
                              )
                            )
                     ),
                     
                     column(10,
                            # --- Sankey ---
                            box(
                              title = "Waste management flow (Sankey diagram)",
                              collapsible = TRUE, solidHeader = TRUE,
                              status = "primary", width = NULL,
                              plotlyOutput("sankeyManagement", height = "600px")
                            ),
                            # --- Table ---
                            box(
                              title = "Total waste by destination (tons)",
                              collapsible = TRUE, solidHeader = TRUE,
                              status = "info", width = NULL,
                              tableOutput("wasteSummaryTable")
                            )
                     )
                   )
          )
        )
      )
    )
  )
))
