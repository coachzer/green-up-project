# Simulation server logic — DORMANT. Not sourced by the running app.
# To revive: see Interface/Visualization/simulation/README.md
# Requires: library(simmer); library(igraph)  (removed from global.R in plan 010)
library(simmer)
library(igraph)
source("simulation/engine.R")  # sim_params(), run_wood_waste_sim()

# Conflict preferences for igraph and simmer (moved from global.R in plan 010)
conflict_prefer("as_data_frame",   "igraph")
conflict_prefer("union",           "igraph")
conflict_prefer("compose",         "igraph")
conflict_prefer("crossing",        "igraph")
conflict_prefer("decompose",       "igraph")
conflict_prefer("simplify",        "igraph")
conflict_prefer("spectrum",        "igraph")
conflict_prefer("%--%",            "igraph")
conflict_prefer("now",             "simmer")
conflict_prefer("reset",           "simmer")
conflict_prefer("rollback",        "simmer")
conflict_prefer("separate",        "simmer")

register_simulation_server <- function(input, output, session) {
  # Simulation tab -----
  
  # Selected choices for filtering tables -----
  selected_choices <- sim_metrics
  
  # Lists -----
  
  max_values <- list(
    generation_rate_increase = 2.0,
    collection_rate_increase = 2.0,
    handling_rate_increase = 2.0,
    recycling_rate_increase = 2.0,
    storage_cost_per_unit = 2.0,
    collection_cost_per_unit = 2.0,
    processing_cost_per_unit = 2.0,
    transportation_cost_per_unit = 2.0,
    overflow_penalty_per_unit = 5.0,
    recycling_revenue_per_unit = 5.0,
    avoided_disposal_cost_per_unit = 2.0,
    storage_capacity = 2000,
    runtime = 365
  )
  
  default_values <- sim_defaults()
  
  # Define reactive values -----
  
  ## Load parameters
  
  load_parameters <- reactive({
    list(
      regions = unlist(strsplit(as.character(input$regions), ",")),
      wood_waste_types = unlist(strsplit(
        as.character(input$wood_waste_types), ","
      )),
      generation_rate_increase = as.numeric(input$generation_rate_increase),
      collection_rate_increase = as.numeric(input$collection_rate_increase),
      handling_rate_increase = as.numeric(input$handling_rate_increase),
      recycling_rate_increase = as.numeric(input$recycling_rate_increase),
      storage_cost_per_unit = as.numeric(input$storage_cost_per_unit),
      collection_cost_per_unit = as.numeric(input$collection_cost_per_unit),
      processing_cost_per_unit = as.numeric(input$processing_cost_per_unit),
      transportation_cost_per_unit = as.numeric(input$transportation_cost_per_unit),
      overflow_penalty_per_unit = as.numeric(input$overflow_penalty_per_unit),
      recycling_revenue_per_unit = as.numeric(input$recycling_revenue_per_unit),
      avoided_disposal_cost_per_unit = as.numeric(input$avoided_disposal_cost_per_unit),
      storage_capacity = as.numeric(input$storage_capacity),
      runtime = as.numeric(input$runtime)
    )
  })
  
  ## Latest run (engine result) and distance matrix

  sim_run <- reactiveVal(NULL)
  distance_matrix_rv <- reactiveVal(NULL)
  results <- reactive({ req(sim_run()); sim_run()$events })
  costs <- reactive({ req(sim_run()); sim_run()$costs })

  # Function to update feedback for a specific input
  update_feedback <- function(input_value, max_value, input_id) {
    # Hide feedback
    hideFeedback(input_id)
    
    # Debug print
    # print(paste("Validating", input_id, "- Input value:", input_value, "Max value:", max_value))
    
    if (is.na(input_value) |
        is.null(input_value) | input_value == "" | input_value == " ") {
      showFeedbackDanger(inputId = input_id, text = "Input cannot be empty.")
      return(FALSE)
    } else if (!is.numeric(input_value)) {
      showFeedbackDanger(inputId = input_id, text = "Input must be a number.")
      return(FALSE)
    }
    
    if (input_value <= 0) {
      showFeedbackDanger(inputId = input_id, text = "Value cannot be negative or zero.")
      return(FALSE)
    } else if (input_value > max_value) {
      showFeedbackDanger(inputId = input_id,
                         text = paste("Value cannot be greater than", max_value))
      return(FALSE)
    } else {
      showFeedbackSuccess(inputId = input_id, text = "Value is valid.")
      return(TRUE)
    }
  }
  
  # Reactive value to store overall validation status
  validation_status <- reactiveVal(rep(TRUE, length(max_values)))
  
  # Add feedback for inputs -----
  
  # Initial validation for all inputs
  observe({
    status <- sapply(seq_along(max_values), function(i) {
      input_name <- names(max_values)[i]
      update_feedback(input[[input_name]], max_values[[input_name]], input_name)
    })
    validation_status(status)
  })
  
  # Set up observers for each input
  # lapply(seq_along(max_values), function(i) {
  #  input_name <- names(max_values)[i]
  #  observeEvent(input[[input_name]], {
  #    update_feedback(input[[input_name]], max_values[[input_name]], input_name)
  #  }, ignoreNULL = FALSE)
  #})
  
  # Disable run button if any input is invalid
  observe({
    if (all(validation_status())) {
      shinyjs::enable("run_simulation")
    } else {
      shinyjs::disable("run_simulation")
    }
  })
  
  # Inputs for scenario selection -----
  observe({
    user_params <- load_parameters()
    params_loaded <- sim_params(input$scenario, input$season, user_params)
    params <- params_loaded$params
    
    output$seasonal_multiplier_display <- renderText({
      paste("Current Seasonal Generation Multiplier: ",
            params$seasonal_multiplier)
    })
    
    updateNumericInput(session,
                       "generation_rate_increase",
                       value = params$generation_rate_increase)
    updateNumericInput(session,
                       "collection_rate_increase",
                       value = params$collection_rate_increase)
    updateNumericInput(session,
                       "handling_rate_increase",
                       value = params$handling_rate_increase)
    updateNumericInput(session,
                       "recycling_rate_increase",
                       value = params$recycling_rate_increase)
    updateNumericInput(session,
                       "storage_cost_per_unit",
                       value = params$storage_cost_per_unit)
    updateNumericInput(session,
                       "collection_cost_per_unit",
                       value = params$collection_cost_per_unit)
    updateNumericInput(session,
                       "processing_cost_per_unit",
                       value = params$processing_cost_per_unit)
    updateNumericInput(session,
                       "transportation_cost_per_unit",
                       value = params$transportation_cost_per_unit)
    updateNumericInput(session,
                       "overflow_penalty_per_unit",
                       value = params$overflow_penalty_per_unit)
    updateNumericInput(session,
                       "recycling_revenue_per_unit",
                       value = params$recycling_revenue_per_unit)
    updateNumericInput(
      session,
      "avoided_disposal_cost_per_unit",
      value = params$avoided_disposal_cost_per_unit
    )
    updateNumericInput(session, "storage_capacity", value = params$storage_capacity)
    updateNumericInput(session, "runtime", value = params$runtime)
  })
  
  # Scenario change
  observeEvent(input$scenario, {
    user_params <- load_parameters()
    params_loaded <- sim_params(input$scenario, input$season, user_params)
    params <- params_loaded$params
    changes <- params_loaded$changes
    
    # Show notifications for changes
    for (change in changes) {
      showNotification(
        change,
        duration = 5,
        closeButton = TRUE,
        type = "message"
      )
    }
    
  })
  
  # Reset parameters -----
  observeEvent(input$reset_parameters, {
    updateNumericInput(session,
                       "generation_rate_increase",
                       value = default_values$generation_rate_increase)
    updateNumericInput(session,
                       "collection_rate_increase",
                       value = default_values$collection_rate_increase)
    updateNumericInput(session,
                       "handling_rate_increase",
                       value = default_values$handling_rate_increase)
    updateNumericInput(session,
                       "recycling_rate_increase",
                       value = default_values$recycling_rate_increase)
    updateNumericInput(session,
                       "storage_cost_per_unit",
                       value = default_values$storage_cost_per_unit)
    updateNumericInput(session,
                       "collection_cost_per_unit",
                       value = default_values$collection_cost_per_unit)
    updateNumericInput(session,
                       "processing_cost_per_unit",
                       value = default_values$processing_cost_per_unit)
    updateNumericInput(session,
                       "transportation_cost_per_unit",
                       value = default_values$transportation_cost_per_unit)
    updateNumericInput(session,
                       "overflow_penalty_per_unit",
                       value = default_values$overflow_penalty_per_unit)
    updateNumericInput(session,
                       "recycling_revenue_per_unit",
                       value = default_values$recycling_revenue_per_unit)
    updateNumericInput(
      session,
      "avoided_disposal_cost_per_unit",
      value = default_values$avoided_disposal_cost_per_unit
    )
    updateNumericInput(session, "storage_capacity", value = default_values$storage_capacity)
    updateNumericInput(session, "runtime", value = default_values$runtime)
  })
  
  # Generate distance matrix -----
  observeEvent(input$generate_matrix, {
    regions <- unlist(strsplit(input$regions, ","))
    if (length(regions) != input$num_regions) {
      showFeedbackDanger("regions",
                         "Number of regions does not match the input number of regions.")
    } else {
      hideFeedback("regions")
      distance_matrix_rv(sim_distance_matrix(regions))
    }
    
    updateTabItems(session, "tabs", "distance_matrix")
  })
  
  # Run simulation -----
  observeEvent(input$run_simulation, {
    withProgress(message = 'Running simulation...', value = 0, {
      params <- sim_params(input$scenario, input$season, load_parameters())$params
      run <- run_wood_waste_sim(params)
      incProgress(1)
    })

    distance_matrix_rv(run$distance_matrix)
    sim_run(run)
    updateTabItems(session, "tabs", "outputs")
  })

  # Simulation Output -----
  
  # Update region and waste type selections based on the parameters
  observeEvent(sim_run(), {
    params <- sim_run()$params
    updateSelectInput(
      session,
      "selected_region",
      choices = params$regions,
      selected = params$regions
    )
    updateSelectInput(
      session,
      "selected_waste_type",
      choices = params$wood_waste_types,
      selected = params$wood_waste_types
    )
    updateSelectInput(session,
                      "selected_metric",
                      choices = selected_choices,
                      selected = selected_choices)
  })
  
  output$simulation_output <- renderDT({
    req(sim_run())
    sim_run()$totals |>
      filter(Region %in% input$selected_region,
             WasteType %in% input$selected_waste_type,
             Metric %in% input$selected_metric) |>
      datatable(options = list(pageLength = 13, autoWidth = TRUE)) |>
      formatRound(columns = c("Value"), digits = 2)
  })

  # Synthetic-data disclosure notice (plan 009 step 5).
  # NOTE: a matching uiOutput("simulation_notice") is in simulation_ui.R (top of the main simulation tab)
  # (out of scope here; tracked for plan 010).
  output$simulation_notice <- renderUI({
    req(sim_run())
    tags$div(
      class = "alert alert-warning",
      tags$strong("Synthetic demonstration data — "),
      "values are randomly generated placeholders (runif draws), not real measurements."
    )
  })


  # Cumulative Totals -----
  
  output$cumulative_totals <- renderDT({
    req(sim_run())
    data <- results()
    total_generation <- sum(data |> filter(Event == "Generation") |> pull(Amount))
    total_collection <- sum(data |> filter(Event == "Collection") |> pull(Amount))
    total_handling <- sum(data |> filter(Event == "Handling") |> pull(Amount))
    total_recycling <- sum(data |> filter(Event == "Recycling") |> pull(Amount))
    
    cumulative_data <- data.frame(
      Metric = c(
        "Total waste generated (kg)",
        "Total waste collected (kg)",
        "Total waste handled (kg)",
        "Total waste recycled (kg)"
      ),
      Value = c(
        total_generation,
        total_collection,
        total_handling,
        total_recycling
      )
    )
    
    datatable(cumulative_data, options = list(pageLength = 10)) |>
      formatRound(columns = c("Value"), digits = 2)
  })
  
  # Data Plots -----
  
  generate_plot_data <- function(event_type) {
    req(sim_run())
    data <- results()
    data <- data |>
      filter(
        Event == event_type,
        Region %in% input$regions_selected,
        WasteType %in% input$waste_types_selected,
        Scenario %in% input$scenario# ,
        # Time >= input$time_start,
        # Time <= input$time_end
      ) |>
      # arrange(Time) |>
      group_by(Region, WasteType) |>
      mutate(CumulativeAmount = cumsum(Amount))
    # e.g. recycling starts at t = 20, so shorter runs have no recycling events
    validate(need(nrow(data) > 0, paste("No", tolower(event_type), "events for this selection.")))
    data
  }
  
  get_plot_parameters <- function(plot_type) {
    y_label <- ifelse(plot_type == "cumulative",
                      "Cumulative Amount",
                      "Amount")
    y_column <- ifelse(plot_type == "cumulative",
                       "CumulativeAmount",
                       "Amount")
    list(y_label = y_label, y_column = y_column)
  }
  
  # Observer to update select inputs based on the results
  
  observeEvent(results(), {
    req(nrow(results()) > 0)
    updateSelectInput(
      session,
      "regions_selected",
      choices = unique(results()$Region),
      selected = unique(results()$Region)
    )
    
    updateSelectInput(
      session,
      "waste_types_selected",
      choices = unique(results()$WasteType),
      selected = unique(results()$WasteType)
    )
    
    updateNumericInput(
      session,
      "time_start",
      value = min(results()$Time),
      min = min(results()$Time)
    )
    
    updateNumericInput(
      session,
      "time_end",
      value = max(results()$Time),
      min = min(results()$Time)
    )
  })
  
  output$generation_plot <- renderPlotly({
    plot_data <- generate_plot_data("Generation")
    plot_params <- get_plot_parameters(input$plot_type)
    p <- ggplot(plot_data, aes(
      x = Time,
      y = !!sym(plot_params$y_column),
      color = interaction(Region, WasteType)
    )) +
      geom_line() +
      labs(title = "Waste Generation Over Time",
           x = "Time",
           y = plot_params$y_label) +
      theme_bw()
    ggplotly(p) |>
      layout(xaxis = list(autorange = TRUE),
             yaxis = list(autorange = TRUE))
  })
  
  output$collection_plot <- renderPlotly({
    plot_data <- generate_plot_data("Collection")
    plot_params <- get_plot_parameters(input$plot_type)
    p <- ggplot(plot_data, aes(
      x = Time,
      y = !!sym(plot_params$y_column),
      color = interaction(Region, WasteType)
    )) +
      geom_line() +
      labs(title = "Waste Collection Over Time",
           x = "Time",
           y = plot_params$y_label) +
      theme_bw()
    ggplotly(p) |>
      layout(xaxis = list(autorange = TRUE),
             yaxis = list(autorange = TRUE))
  })
  
  output$handling_plot <- renderPlotly({
    plot_data <- generate_plot_data("Handling")
    plot_params <- get_plot_parameters(input$plot_type)
    p <- ggplot(plot_data, aes(
      x = Time,
      y = !!sym(plot_params$y_column),
      color = interaction(Region, WasteType)
    )) +
      geom_line() +
      labs(title = "Waste Handling Over Time",
           x = "Time",
           y = plot_params$y_label) +
      theme_bw()
    ggplotly(p) |>
      layout(xaxis = list(autorange = TRUE),
             yaxis = list(autorange = TRUE))
  })
  
  output$recycling_plot <- renderPlotly({
    plot_data <- generate_plot_data("Recycling")
    plot_params <- get_plot_parameters(input$plot_type)
    p <- ggplot(plot_data, aes(
      x = Time,
      y = !!sym(plot_params$y_column),
      color = interaction(Region, WasteType)
    )) +
      geom_line() +
      labs(title = "Waste Recycling Over Time",
           x = "Time",
           y = plot_params$y_label) +
      theme_bw()
    ggplotly(p) |>
      layout(xaxis = list(autorange = TRUE),
             yaxis = list(autorange = TRUE))
  })
  
  # Results Table -----
  
  output$results_table <- renderDT({
    req(sim_run())
    datatable(results(), options = list(pageLength = 10, autoWidth = TRUE))
  })
  
  # Cost Plots -----
  
  generate_cost_plot_data <- function(cost_type) {
    req(sim_run())
    data <- costs()
    data <- data |>
      dplyr::select(Time, Region, WasteType, !!sym(cost_type)) |>
      arrange(Time)
    return(data)
  }
  
  output$collection_cost_plot <- renderPlotly({
    plot_data <- generate_cost_plot_data("CollectionCost")
    p <- ggplot(plot_data, aes(
      x = Time,
      y = !!sym("CollectionCost"),
      color = interaction(Region, WasteType)
    )) +
      geom_line() +
      labs(title = "Collection Cost Over Time", x = "Time", y = "Collection Cost") +
      theme_bw()
    ggplotly(p) |>
      layout(xaxis = list(autorange = TRUE),
             yaxis = list(autorange = TRUE))
  })
  
  output$processing_cost_plot <- renderPlotly({
    plot_data <- generate_cost_plot_data("ProcessingCost")
    p <- ggplot(plot_data, aes(
      x = Time,
      y = !!sym("ProcessingCost"),
      color = interaction(Region, WasteType)
    )) +
      geom_line() +
      labs(title = "Processing Cost Over Time", x = "Time", y = "Processing Cost") +
      theme_bw()
    ggplotly(p) |>
      layout(xaxis = list(autorange = TRUE),
             yaxis = list(autorange = TRUE))
  })
  
  output$transportation_cost_plot <- renderPlotly({
    plot_data <- generate_cost_plot_data("TransportationCost")
    p <- ggplot(plot_data, aes(
      x = Time,
      y = !!sym("TransportationCost"),
      color = interaction(Region, WasteType)
    )) +
      geom_line() +
      labs(title = "Transportation Cost Over Time", x = "Time", y = "Transportation Cost") +
      theme_bw()
    ggplotly(p) |>
      layout(xaxis = list(autorange = TRUE),
             yaxis = list(autorange = TRUE))
  })
  
  output$recycling_revenue_plot <- renderPlotly({
    plot_data <- generate_cost_plot_data("RecyclingRevenue")
    p <- ggplot(plot_data, aes(
      x = Time,
      y = !!sym("RecyclingRevenue"),
      color = interaction(Region, WasteType)
    )) +
      geom_line() +
      labs(title = "Recycling Revenue Over Time", x = "Time", y = "Recycling Revenue") +
      theme_bw()
    ggplotly(p) |>
      layout(xaxis = list(autorange = TRUE),
             yaxis = list(autorange = TRUE))
  })
  
  # Costs Table -----
  
  output$costs_table <- renderDT({
    req(sim_run())
    datatable(costs(), options = list(pageLength = 10, autoWidth = FALSE))
  })
  
  # Distance Matrix -----
  
  output$distance_matrix <- renderTable({
    req(distance_matrix_rv())
  }, rownames = TRUE, colnames = TRUE)
  
  output$distance_matrix_plot <- renderPlotly({
    distance_matrix <- distance_matrix_rv()
    req(distance_matrix)
    regions <- unlist(strsplit(input$regions, ","))
    melted_matrix <- melt(distance_matrix)
    colnames(melted_matrix) <- c("Region1", "Region2", "Distance")
    
    plot_ly(
      data = melted_matrix,
      x = ~ Region2,
      y = ~ Region1,
      z = ~ Distance,
      type = "heatmap",
      colorscale = "Reds",
      colorbar = list(title = "Distance")
    ) |>
      layout(
        xaxis = list(title = "Region", side = "top"),
        yaxis = list(title = "Region", autorange = "reversed") 
      )
    
  })
  
  # Force Directed Plot -----
  
  output$force_directed_plot <- renderPlotly({
    distance_matrix <- distance_matrix_rv()
    req(distance_matrix)
    regions <- unlist(strsplit(input$regions, ","))
    distance_matrix
    colnames(distance_matrix) <- regions
    rownames(distance_matrix) <- regions
    
    # Create an igraph object from the distance matrix
    graph <- graph_from_adjacency_matrix(
      as.matrix(distance_matrix),
      mode = "max",
      weighted = TRUE,
      diag = FALSE
    )
    
    # Get the layout for the graph
    layout <- layout_with_fr(graph)
    
    # Create a data frame for the nodes
    nodes <- data.frame(id = V(graph)$name,
                        x = layout[, 1],
                        y = layout[, 2])
    
    # Create a data frame for the edges
    edges <- as.data.frame(as_edgelist(graph))
    colnames(edges) <- c("from", "to")
    edges$weight <- E(graph)$weight
    
    # Plot the graph with plotly
    plot_ly() |>
      add_markers(
        data = nodes,
        x = ~ x,
        y = ~ y,
        text = ~ id,
        mode = 'markers+text',
        textposition = 'top center',
        hoverinfo = 'text'
      ) |>
      add_segments(
        data = edges,
        x = ~ nodes$x[match(edges$from, nodes$id)],
        xend = ~ nodes$x[match(edges$to, nodes$id)],
        y = ~ nodes$y[match(edges$from, nodes$id)],
        yend = ~ nodes$y[match(edges$to, nodes$id)],
        line = list(color = 'black', width = ~ weight / 5)
      ) |>
      layout(
        title = "Force-Directed Layout of Regions",
        xaxis = list(
          title = "",
          showgrid = FALSE,
          zeroline = FALSE
        ),
        yaxis = list(
          title = "",
          showgrid = FALSE,
          zeroline = FALSE
        )
      )
  })
  
  
  # Export Data -----
  
  output$download_data <- downloadHandler(
    filename = function() {
      "simulation_data.csv"
    },
    content = function(file) {
      write.csv(results(), file)
    }
  )
  
  # Generate Reports ----- WIP
  
  output$download_report <- downloadHandler(
    filename = function() {
      "simulation_report.html"
    },
    content = function(file) {
      # Self-contained report: write a temp Rmd, knit it, copy out.
      # NOTE: the simulation figures are synthetic placeholders (see plan 009 step 5).
      tmp <- tempfile(fileext = ".Rmd")
      writeLines(c(
        "---",
        "title: 'Wood Waste Simulation Report (synthetic data)'",
        "output: html_document",
        "params:",
        "  data: NULL",
        "---",
        "",
        "> **Note:** simulation outputs are synthetic placeholder draws, not real measurements.",
        "",
        "```{r echo=FALSE}",
        "knitr::kable(params$data)",
        "```"
      ), tmp)
      out <- rmarkdown::render(tmp, params = list(data = results()), quiet = TRUE)
      file.copy(out, file)
    }
  )

}
