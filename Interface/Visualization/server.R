library(shiny)
library(simmer)
library(dplyr)
library(ggplot2)
library(DT)
library(plotly)
library(shinyjs)
library(shinydashboard)
library(shinyBS)
library(reshape2)
library(igraph)

shinyServer(function(input, output, session) {
  
  # Lists ----------
  
  seasons <- list(
    winter = 0.8,
    spring = 1.0,
    summer = 1.2,
    fall = 1.0
  )
    
  default_values <- list(
    generation_rate_increase = 1,
    collection_rate_increase = 1,
    handling_rate_increase = 1,
    recycling_rate_increase = 1,
    storage_cost_per_unit = 0.5,
    collection_cost_per_unit = 1,
    processing_cost_per_unit = 2,
    transportation_cost_per_unit = 0.3,
    overflow_penalty_per_unit = 2,
    recycling_revenue_per_unit = 2,
    avoided_disposal_cost_per_unit = 1,
    storage_capacity = 500,
    runtime = 100
  )
  
  # Load scenario parameters -----------------------------------------------------------
  
  load_scenario_parameters <- function(scenario, user_params) {
    params <- user_params
    if (scenario == "increased_waste_generation") {
      params$generation_rate_increase <- 2
    } else if (scenario == "improved_collection_efficiency") {
      params$collection_rate_increase <- 1.5
    } else if (scenario == "improved_handling_efficiency") {
      params$handling_rate_increase <- 1.5
    } else if (scenario == "enhanced_recycling_programs") {
      params$recycling_rate_increase <- 2
      params$recycling_revenue_per_unit <- 5
      params$avoided_disposal_cost_per_unit <- 2
    } else if (scenario == "storage_capacity_expansion") {
      params$storage_capacity <- 1000
    } else if (scenario == "penalties_for_overflow") {
      params$overflow_penalty_per_unit <- 5
    } else if (scenario == "different_transportation_costs") {
      params$transportation_cost_per_unit <- 0.5
    } else if (scenario == "seasonal_variations") {
        if (input$season == "Year-long") {
          params$seasonal_variations <- TRUE
        } else {
          params$seasonal_multiplier <- seasons[[tolower(input$season)]]
        }
    }
    return(params)
  }
  
  # Load parameters -----------------------------------------------------------
   
  load_parameters <- reactive({
    list(
      regions = unlist(strsplit(as.character(input$regions), ",")),
      wood_waste_types = unlist(strsplit(as.character(input$wood_waste_types), ",")),
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
      storage_capacity = as.numeric(input$storage_capacity)
    )
  })
  
  # Define reactive values for event and cost data -----------
  results <- reactiveVal(data.frame(
    Time = numeric(),
    Region = character(),
    WasteType = character(),
    Event = character(),
    Amount = numeric(),
    stringsAsFactors = FALSE
  ))
  
  costs <- reactiveVal(data.frame(
    Time = numeric(),
    Region = character(),
    WasteType = character(),
    CollectionCost = numeric(),
    ProcessingCost = numeric(),
    TransportationCost = numeric(),
    RecyclingRevenue = numeric(),
    stringsAsFactors = FALSE
  ))
   
  # Logging events and costs -----------------------------------------------------------
  log_event <- function(env, region, waste_type, event) {
    time <- now(env)
    amount <- get_attribute(env, "waste_amount")
    
    # Event data
    event_data <- data.frame(
      Time = time,
      Region = region,
      WasteType = waste_type,
      Event = event,
      Amount = amount,
      stringsAsFactors = FALSE
    )
    
    # Cost data
    cost_data <- data.frame(
      Time = time,
      Region = region,
      WasteType = waste_type,
      CollectionCost = get_global(env, paste0("total_collection_cost_", region, "_", waste_type)),
      ProcessingCost = get_global(env, paste0("total_processing_cost_", region, "_", waste_type)),
      TransportationCost = get_global(env, paste0("total_transportation_cost_", region, "_", waste_type)),
      RecyclingRevenue = get_global(env, paste0("total_recycling_revenue_", region, "_", waste_type)),
      stringsAsFactors = FALSE
    )
    
    # Append data to results and costs
    results(rbind(results(), event_data))
    costs(rbind(costs(), cost_data))
  }
  
  create_trajectories <- function(env, params, region, waste_type) {
    generate_waste <- trajectory(paste("Generate Waste -", region, "-", waste_type)) |>
      seize("storage", 1) |>
      set_attribute("waste_amount", function() {
        base_amount <- runif(1, 10, 30)
        if (input$scenario == "seasonal_variations") {
          if (params$seasonal_variations) {
            multiplier <- seasons[[sample(names(seasons), 1)]]
          } else {
            multiplier <- params$seasonal_multiplier
          }
          base_amount * multiplier
        } else {
          base_amount
        }
      }) |>
      set_global(paste0("total_waste_generated_", region, "_", waste_type), function() {
        waste_generated <- get_attribute(env, "waste_amount")
        get_global(env, paste0("total_waste_generated_", region, "_", waste_type)) + waste_generated
      }) |>
      set_global(paste0("waste_to_collect_", region, "_", waste_type), function() {
        waste_generated <- get_attribute(env, "waste_amount")
        get_global(env, paste0("waste_to_collect_", region, "_", waste_type)) + waste_generated
      }) |>
      set_global(paste0("overflow_penalty_", region, "_", waste_type), function() {
        waste_amount <- get_attribute(env, "waste_amount")
        result <- handle_overflow(env, region, waste_type, waste_amount)
        get_global(env, paste0("overflow_penalty_", region, "_", waste_type)) + result$overflow_penalty
      }) |>
      set_global(paste0("total_waste_stored_", region, "_", waste_type), function() {
        waste_amount <- get_attribute(env, "waste_amount")
        result <- handle_overflow(env, region, waste_type, waste_amount)
        result$new_storage
      }) |>
      set_global(paste0("total_storage_cost_", region, "_", waste_type), function() {
        # Get the current amount of waste
        waste_amount <- get_attribute(env, "waste_amount")
        
        # Calculate the current cost
        current_storage_cost <- waste_amount * params$storage_cost_per_unit
        
        # Update the global total cost
        get_global(env, paste0("total_storage_cost_", region, "_", waste_type)) + current_storage_cost
      }) |> 
      set_attribute("log_event", function() log_event(env, region, waste_type, "Generation")) |> 
      timeout(function() rexp(1, rate = 0.5)) |>
      release("storage", 1) 
    
    collect_waste <- trajectory(paste("Collect Waste -", region, "-", waste_type)) |>
      timeout(10) |>
      seize("collection_truck") |>
      seize("storage", 1) |>
      set_attribute("waste_amount", function() {
        waste_to_collect <- get_global(env, paste0("waste_to_collect_", region, "_", waste_type))
        if (!is.na(waste_to_collect) && waste_to_collect > 5) {
          amount <- runif(1, 5, min(25, waste_to_collect))
          if (is.na(amount)) {
            amount <- 0
          }
          amount
        } else if (!is.na(waste_to_collect) && waste_to_collect > 0) {
          waste_to_collect  # Collect the remaining waste if it's less than 5
        } else {
          0
        }
      }) |>
      set_global(paste0("waste_to_collect_", region, "_", waste_type), function() {
        waste_to_collect <- get_global(env, paste0("waste_to_collect_", region, "_", waste_type))
        waste_amount <- get_attribute(env, "waste_amount")
        waste_to_collect - waste_amount
      }) |>
      set_global(paste0("total_waste_collected_", region, "_", waste_type), function() {
        get_global(env, paste0("total_waste_collected_", region, "_", waste_type)) + get_attribute(env, "waste_amount")
      }) |>
      set_global(paste0("total_collection_cost_", region, "_", waste_type), function() {
        waste_amount <- get_attribute(env, "waste_amount")
        current_collection_cost <- waste_amount * params$collection_cost_per_unit
        get_global(env, paste0("total_collection_cost_", region, "_", waste_type)) + current_collection_cost
      }) |>
      set_global(paste0("total_transportation_cost_", region, "_", waste_type), function() {
        waste_amount <- get_attribute(env, "waste_amount")
        current_transportation_cost <- waste_amount * params$transportation_cost_per_unit
        get_global(env, paste0("total_transportation_cost_", region, "_", waste_type)) + current_transportation_cost
      }) |>
      set_global(paste0("total_waste_stored_", region, "_", waste_type), function() {
        current_storage <- get_global(env, paste0("total_waste_stored_", region, "_", waste_type))
        waste_amount <- get_attribute(env, "waste_amount")
        max(current_storage - waste_amount, 0)
      }) |>
      set_attribute("log_event", function() log_event(env, region, waste_type, "Collection")) |> 
      timeout(function() rexp(1, rate = 1.5)) |>
      release("storage", 1) |>
      release("collection_truck")
    
    
    handle_waste <- trajectory(paste("Handle Waste -", region, "-", waste_type)) |>
      timeout(20) |>
      seize("collection_truck") |>
      seize("waste_processor") |>
      set_attribute("waste_amount", function() runif(1, 2, 15)) |>
      set_global(paste0("total_waste_handled_", region, "_", waste_type), function() {
        get_global(env, paste0("total_waste_handled_", region, "_", waste_type)) + get_attribute(env, "waste_amount")
      }) |>
      set_global(paste0("total_processing_cost_", region, "_", waste_type), function() {
        waste_amount <- get_attribute(env, "waste_amount")
        current_processing_cost <- waste_amount * params$processing_cost_per_unit
        get_global(env, paste0("total_processing_cost_", region, "_", waste_type)) + current_processing_cost
      })  |>
      set_attribute("log_event", function() log_event(env, region, waste_type, "Handling")) |>
      timeout(function() rexp(1, rate = 0.7)) |>
      release("waste_processor") |>
      release("collection_truck") 
    
    recycle_waste <- trajectory(paste("Recycle Waste -", region, "-", waste_type)) |>
      timeout(20) |>
      seize("recycling_facility") |>
      set_attribute("waste_amount", function() runif(1, 1, 7)) |>
      set_global(paste0("total_waste_recycled_", region, "_", waste_type), function() {
        recycled_amount <- get_attribute(env, "waste_amount")
        get_global(env, paste0("total_waste_recycled_", region, "_", waste_type)) + recycled_amount
      }) |>
      set_global(paste0("total_recycling_revenue_", region, "_", waste_type), function() {
        recycled_amount <- get_attribute(env, "waste_amount")
        current_revenue <- recycled_amount * params$recycling_revenue_per_unit
        get_global(env, paste0("total_recycling_revenue_", region, "_", waste_type)) + current_revenue
      }) |>
      set_global(paste0("total_avoided_disposal_cost_", region, "_", waste_type), function() {
        recycled_amount <- get_attribute(env, "waste_amount")
        current_avoided_disposal_cost <- recycled_amount * params$avoided_disposal_cost_per_unit
        get_global(env, paste0("total_avoided_disposal_cost_", region, "_", waste_type)) + current_avoided_disposal_cost
      }) |> 
      set_attribute("log_event", function() log_event(env, region, waste_type, "Recycling")) |> 
      timeout(function() rexp(1, rate = 0.3)) |>
      release("recycling_facility")
    
    list(generate_waste = generate_waste, collect_waste = collect_waste, handle_waste = handle_waste, recycle_waste = recycle_waste)
  }
  
  handle_overflow <- function(env, region, waste_type, waste_amount) {
    current_storage <- get_global(env, paste0("total_waste_stored_", region, "_", waste_type))
    new_storage <- current_storage + waste_amount
    overflow_penalty <- 0
    if (!is.na(new_storage) && new_storage > input$storage_capacity) {
      overflow_amount <- new_storage - input$storage_capacity
      overflow_penalty <- overflow_amount * input$overflow_penalty_per_unit
      new_storage <- input$storage_capacity
    } else {
      overflow_penalty <- 0
    }
    list(new_storage = new_storage, overflow_penalty = overflow_penalty)
  }
  
  balance_storage_across_regions <- function(env, params, distance_matrix) {
    transportation_costs <- matrix(0, nrow = length(params$regions), ncol = length(params$wood_waste_types),
                                   dimnames = list(params$regions, params$wood_waste_types))
    
    for (waste_type in params$wood_waste_types) {
      total_storages <- sapply(params$regions, function(region) get_global(env, paste0("total_waste_stored_", region, "_", waste_type)))
      total_storage <- sum(total_storages)
      avg_storage <- total_storage / length(params$regions)
      
      cat("Initial storage for waste type:", waste_type, "\n")
      for (region in params$regions) {
        initial_storage <- get_global(env, paste0("total_waste_stored_", region, "_", waste_type))
        cat("  Region:", region, "Initial storage (kg):", initial_storage, "\n")
      }
      
      for (region in params$regions) {
        current_storage <- get_global(env, paste0("total_waste_stored_", region, "_", waste_type))
        transfer_amount <- avg_storage - current_storage
        
        if (transfer_amount > 0) {
          for (source_region in params$regions) {
            if (source_region != region) {
              source_storage <- get_global(env, paste0("total_waste_stored_", source_region, "_", waste_type))
              if (source_storage > avg_storage) {
                actual_transfer_amount <- min(transfer_amount, source_storage - avg_storage)
                if (actual_transfer_amount > 0) {
                  distance <- distance_matrix[source_region, region]
                  transport_cost <- actual_transfer_amount * params$transportation_cost_per_unit * distance
                  
                  current_storage <- current_storage + actual_transfer_amount
                  source_storage <- source_storage - actual_transfer_amount
                  transfer_amount <- transfer_amount - actual_transfer_amount
                  
                  transportation_costs[source_region, waste_type] <- transportation_costs[source_region, waste_type] + transport_cost
                  transportation_costs[region, waste_type] <- transportation_costs[region, waste_type] + transport_cost
                  
                  if (transfer_amount <= 0) break
                }
              }
            }
          }
        } else if (transfer_amount < 0) {
          for (target_region in params$regions) {
            if (target_region != region) {
              target_storage <- get_global(env, paste0("total_waste_stored_", target_region, "_", waste_type))
              if (target_storage < avg_storage) {
                actual_transfer_amount <- min(-transfer_amount, avg_storage - target_storage)
                if (actual_transfer_amount > 0) {
                  distance <- distance_matrix[region, target_region]
                  transport_cost <- actual_transfer_amount * params$transportation_cost_per_unit * distance
                  
                  current_storage <- current_storage - actual_transfer_amount
                  target_storage <- target_storage + actual_transfer_amount
                  transfer_amount <- transfer_amount + actual_transfer_amount
                  
                  transportation_costs[region, waste_type] <- transportation_costs[region, waste_type] + transport_cost
                  transportation_costs[target_region, waste_type] <- transportation_costs[target_region, waste_type] + transport_cost
                  
                  if (transfer_amount >= 0) break
                }
              }
            }
          }
        }
        
        cat("Final storage in region", region, "for waste type", waste_type, ":", current_storage, "kg\n")
      }
    }
    
    return(transportation_costs)
  }
  
  generate_distance_matrix <- function(regions) {
    n <- length(regions)
    matrix <- matrix(runif(n * n, min = 1, max = 100), nrow = n, ncol = n)
    diag(matrix) <- 0
    colnames(matrix) <- regions
    rownames(matrix) <- regions
    return(matrix)
  }
  
  # Add feedback for negative inputs -----------------------------------------------------------
  observe({
    for (input_name in c("generation_rate_increase", "collection_rate_increase", "handling_rate_increase", 
                         "recycling_rate_increase", "storage_cost_per_unit", "collection_cost_per_unit", 
                         "processing_cost_per_unit", "transportation_cost_per_unit", 
                         "overflow_penalty_per_unit", "recycling_revenue_per_unit", 
                         "avoided_disposal_cost_per_unit", "storage_capacity")) {
      if (input[[input_name]] < 0) {
        showFeedbackDanger(inputId = input_name, text = "Value cannot be negative")
      } else {
        hideFeedback(inputId = input_name)
      }
    }
  })
  
  # Inputs for scenario selection -----------------------------------------------------------
  observe({
    scenario <- input$scenario
    user_params <- load_parameters()
    params <- load_scenario_parameters(scenario, user_params)
    updateNumericInput(session, "generation_rate_increase", value = params$generation_rate_increase)
    updateNumericInput(session, "collection_rate_increase", value = params$collection_rate_increase)
    updateNumericInput(session, "handling_rate_increase", value = params$handling_rate_increase)
    updateNumericInput(session, "recycling_rate_increase", value = params$recycling_rate_increase)
    updateNumericInput(session, "storage_cost_per_unit", value = params$storage_cost_per_unit)
    updateNumericInput(session, "collection_cost_per_unit", value = params$collection_cost_per_unit)
    updateNumericInput(session, "processing_cost_per_unit", value = params$processing_cost_per_unit)
    updateNumericInput(session, "transportation_cost_per_unit", value = params$transportation_cost_per_unit)
    updateNumericInput(session, "overflow_penalty_per_unit", value = params$overflow_penalty_per_unit)
    updateNumericInput(session, "recycling_revenue_per_unit", value = params$recycling_revenue_per_unit)
    updateNumericInput(session, "avoided_disposal_cost_per_unit", value = params$avoided_disposal_cost_per_unit)
    updateNumericInput(session, "storage_capacity", value = params$storage_capacity)
  })
  
  # Reset parameters -----------------------------------------------------------
  observeEvent(input$reset_parameters, {
    updateNumericInput(session, "generation_rate_increase", value = default_values$generation_rate_increase)
    updateNumericInput(session, "collection_rate_increase", value = default_values$collection_rate_increase)
    updateNumericInput(session, "handling_rate_increase", value = default_values$handling_rate_increase)
    updateNumericInput(session, "recycling_rate_increase", value = default_values$recycling_rate_increase)
    updateNumericInput(session, "storage_cost_per_unit", value = default_values$storage_cost_per_unit)
    updateNumericInput(session, "collection_cost_per_unit", value = default_values$collection_cost_per_unit)
    updateNumericInput(session, "processing_cost_per_unit", value = default_values$processing_cost_per_unit)
    updateNumericInput(session, "transportation_cost_per_unit", value = default_values$transportation_cost_per_unit)
    updateNumericInput(session, "overflow_penalty_per_unit", value = default_values$overflow_penalty_per_unit)
    updateNumericInput(session, "recycling_revenue_per_unit", value = default_values$recycling_revenue_per_unit)
    updateNumericInput(session, "avoided_disposal_cost_per_unit", value = default_values$avoided_disposal_cost_per_unit)
    updateNumericInput(session, "storage_capacity", value = default_values$storage_capacity)
  })
  
  # Dynamic UI for parameters -----------------------------------------------------------
  output$dynamic_ui <- renderUI({
    fluidRow(
      textInput("regions", "Regions (comma-separated):", "North,South,East,West"),
      textInput("wood_waste_types", "Wood Waste Types (comma-separated):", "Construction,Demolition,Packaging"),
      numericInput("generation_rate_increase", "Adjust Generation Multiplier:", 1),
      numericInput("collection_rate_increase", "Adjust Collection Multiplier:", 1),
      numericInput("handling_rate_increase", "Adjust Handling Multiplier:", 1),
      numericInput("recycling_rate_increase", "Adjust Recycling Multiplier:", 1),
      numericInput("storage_cost_per_unit", "Storage Cost per Unit:", 0.5),
      numericInput("collection_cost_per_unit", "Collection Cost per Unit:", 1),
      numericInput("processing_cost_per_unit", "Processing Cost per Unit:", 2),
      numericInput("transportation_cost_per_unit", "Transportation Cost per Unit:", 0.3),
      numericInput("overflow_penalty_per_unit", "Overflow Penalty per Unit:", 2),
      numericInput("recycling_revenue_per_unit", "Recycling Revenue per Unit:", 2),
      numericInput("avoided_disposal_cost_per_unit", "Avoided Disposal Cost per Unit:", 1),
      numericInput("storage_capacity", "Storage Capacity per Region:", 500)
    )
  })
  
  # Generate distance matrix -----------------------------------------------------------
  observeEvent(input$generate_matrix, {
    regions <- unlist(strsplit(input$regions, ","))
    if (length(regions) != input$num_regions) {
      showFeedbackDanger("regions", "Number of regions does not match the input number of regions.")
    } else {
      hideFeedback("regions")
      distance_matrix <- generate_distance_matrix(regions)
      output$distance_matrix <- renderTable({
        distance_matrix
      }, rownames = TRUE, colnames = TRUE)
    }
    
    updateTabItems(session, "tabs", "distance_matrix")
  })
  
  # Run simulation -----------------------------------------------------------
  observeEvent(input$run_simulation, {
    
    # Clear previous results and costs
    results(data.frame(Time = numeric(), Region = character(), WasteType = character(), Event = character(), Amount = numeric(), stringsAsFactors = FALSE))
    costs(data.frame(Time = numeric(), Region = character(), WasteType = character(), CollectionCost = numeric(), ProcessingCost = numeric(), TransportationCost = numeric(), RecyclingRevenue = numeric(), stringsAsFactors = FALSE))
    
    
    withProgress(message = 'Running simulation...', value = 0, {
      
      scenario <- input$scenario
      user_params <- load_parameters()
      params <- load_scenario_parameters(scenario, user_params)
      
      env <- simmer("Wood Waste Flow Simulation")
      
      for (region in params$regions) {
        for (waste_type in params$wood_waste_types) {
          env <- env |>
            add_global(paste0("total_waste_generated_", region, "_", waste_type), 0) |>
            add_global(paste0("total_waste_collected_", region, "_", waste_type), 0) |>
            add_global(paste0("total_waste_handled_", region, "_", waste_type), 0) |>
            add_global(paste0("total_waste_stored_", region, "_", waste_type), 0) |>
            add_global(paste0("total_waste_recycled_", region, "_", waste_type), 0) |>
            add_global(paste0("overflow_penalty_", region, "_", waste_type), 0) |>
            add_global(paste0("waste_to_collect_", region, "_", waste_type), 0) |>
            add_global(paste0("total_storage_cost_", region, "_", waste_type), 0) |>
            add_global(paste0("total_collection_cost_", region, "_", waste_type), 0) |>
            add_global(paste0("total_processing_cost_", region, "_", waste_type), 0) |>
            add_global(paste0("total_transportation_cost_", region, "_", waste_type), 0) |>
            add_global(paste0("total_recycling_revenue_", region, "_", waste_type), 0) |>
            add_global(paste0("total_avoided_disposal_cost_", region, "_", waste_type), 0)
        }
      }
      
      env <- env |>
        add_resource("storage", capacity = params$storage_capacity * length(params$regions) * length(params$wood_waste_types)) |>
        add_resource("collection_truck", capacity = length(params$regions)) |>
        add_resource("waste_processor", capacity = length(params$regions)) |>
        add_resource("recycling_facility", capacity = length(params$regions))
      
      
      trajectories <- list()
      for (region in params$regions) {
        for (waste_type in params$wood_waste_types) {
          trajs <- create_trajectories(env, params, region, waste_type)
          trajectories[[paste(region, waste_type, sep = "_")]] <- trajs
          env <- env |>
            add_generator(paste0("waste_gen_", region, "_", waste_type), trajs$generate_waste, function() rexp(1, rate = 0.5 * ifelse(is.null(params$generation_rate_increase), 1, params$generation_rate_increase))) |>
            add_generator(paste0("waste_collector_", region, "_", waste_type), trajs$collect_waste, function() rexp(1, rate = 1.5 * ifelse(is.null(params$collection_rate_increase), 1, params$collection_rate_increase))) |>
            add_generator(paste0("waste_handler_", region, "_", waste_type), trajs$handle_waste, function() rexp(1, rate = 1 * ifelse(is.null(params$handling_rate_increase), 1, params$handling_rate_increase))) |>
            add_generator(paste0("waste_recycler_", region, "_", waste_type), trajs$recycle_waste, function() rexp(1, rate = 1 * ifelse(is.null(params$recycling_rate_increase), 1, params$recycling_rate_increase)))
        }
      }
      
      incProgress(0.5)
      env |> run(until = 100)
      # env |> run(until = 365) # Imitating a year-long simulation
      incProgress(0.8)
      
      distance_matrix <- generate_distance_matrix(params$regions)
      incProgress(0.9)
      transportation_costs <- balance_storage_across_regions(env, params, distance_matrix)
      incProgress(1)
      
      updateTabItems(session, "tabs", "outputs")
      
      # Simulation Output -----------------------------------------------------------
      
      generate_waste_type_table <- function(region, waste_type, env, params) {
        data.frame(
          Metric = c("Total waste generated (kg)", "Total waste collected (kg)", "Total waste handled (kg)",
                     "Total waste stored (kg)", "Total waste recycled (kg)", "Overflow penalty",
                     "Total storage cost", "Total collection cost", "Total processing cost",
                     "Total transportation cost", "Transportation cost after balancing", "Total recycling revenue", "Total avoided disposal cost"),
          Value = c(get_global(env, paste0("total_waste_generated_", region, "_", waste_type)),
                    get_global(env, paste0("total_waste_collected_", region, "_", waste_type)),
                    get_global(env, paste0("total_waste_handled_", region, "_", waste_type)),
                    get_global(env, paste0("total_waste_stored_", region, "_", waste_type)),
                    get_global(env, paste0("total_waste_recycled_", region, "_", waste_type)),
                    get_global(env, paste0("overflow_penalty_", region, "_", waste_type)),
                    get_global(env, paste0("total_storage_cost_", region, "_", waste_type)),
                    get_global(env, paste0("total_collection_cost_", region, "_", waste_type)),
                    get_global(env, paste0("total_processing_cost_", region, "_", waste_type)),
                    get_global(env, paste0("total_transportation_cost_", region, "_", waste_type)),
                    transportation_costs[region, waste_type],
                    get_global(env, paste0("total_recycling_revenue_", region, "_", waste_type)),
                    get_global(env, paste0("total_avoided_disposal_cost_", region, "_", waste_type)))
        )
      }
      
      # Generate the simulation output tables
      generate_simulation_output <- function(params, env) {
        simulation_data <- data.frame(
          Region = character(),
          WasteType = character(),
          Metric = character(),
          Value = numeric(),
          stringsAsFactors = FALSE
        )
        
        for (region in params$regions) {
          for (waste_type in params$wood_waste_types) {
            waste_data <- generate_waste_type_table(region, waste_type, env, params)
            waste_data <- cbind(Region = region, WasteType = waste_type, waste_data)
            simulation_data <- rbind(simulation_data, waste_data)
          }
        }
        
        datatable(simulation_data, options = list(pageLength = 13, autoWidth = TRUE))
      }
      
      output$simulation_output <- renderDT({
        generate_simulation_output(params, env)
      })
      
      # Cumulative Totals -----------------------------------------------------------
      
      output$cumulative_totals <- renderDT({
        data <- results()
        total_generation <- sum(data |> filter(Event == "Generation") |> pull(Amount))
        total_collection <- sum(data |> filter(Event == "Collection") |> pull(Amount))
        total_handling <- sum(data |> filter(Event == "Handling") |> pull(Amount))
        total_recycling <- sum(data |> filter(Event == "Recycling") |> pull(Amount))
        
        cumulative_data <- data.frame(
          Metric = c("Total waste generated (kg)", 
                     "Total waste collected (kg)", 
                     "Total waste handled (kg)", 
                     "Total waste recycled (kg)"),
          Value = c(total_generation, 
                    total_collection, 
                    total_handling, 
                    total_recycling)
        )
        
        datatable(cumulative_data, options = list(pageLength = 10))
      })
      
      # Data Plots -----------------------------------------------------------
      
      generate_plot_data <- function(event_type) {
        data <- results()
        data <- data |> 
          filter(Event == event_type) |>
          arrange(Time) |>
          group_by(Region, WasteType) |>
          mutate(CumulativeAmount = cumsum(Amount))
        data
      }
      
      get_plot_parameters <- function(plot_type) {
        y_label <- ifelse(plot_type == "cumulative", "Cumulative Amount", "Amount")
        y_column <- ifelse(plot_type == "cumulative", "CumulativeAmount", "Amount")
        list(y_label = y_label, y_column = y_column)
      }
      
      output$generation_plot <- renderPlotly({
        plot_data <- generate_plot_data("Generation")
        plot_params <- get_plot_parameters(input$plot_type)
        p <- ggplot(plot_data, aes(x = Time, y = !!sym(plot_params$y_column), color = interaction(Region, WasteType))) +
          geom_line() +
          labs(title = "Waste Generation Over Time",
               x = "Time",
               y = plot_params$y_label) +
          theme_bw()
        ggplotly(p)
      })
      
      output$collection_plot <- renderPlotly({
        plot_data <- generate_plot_data("Collection")
        plot_params <- get_plot_parameters(input$plot_type)
        p <- ggplot(plot_data, aes(x = Time, y = !!sym(plot_params$y_column), color = interaction(Region, WasteType))) +
          geom_line() +
          labs(title = "Waste Collection Over Time",
               x = "Time",
               y = plot_params$y_label) +
          theme_bw()
        ggplotly(p)
      })
      
      output$handling_plot <- renderPlotly({
        plot_data <- generate_plot_data("Handling")
        plot_params <- get_plot_parameters(input$plot_type)
        p <- ggplot(plot_data, aes(x = Time, y = !!sym(plot_params$y_column), color = interaction(Region, WasteType))) +
          geom_line() +
          labs(title = "Waste Handling Over Time",
               x = "Time",
               y = plot_params$y_label) +
          theme_bw()
        ggplotly(p)
      })
      
      output$recycling_plot <- renderPlotly({
        plot_data <- generate_plot_data("Recycling")
        plot_params <- get_plot_parameters(input$plot_type)
        p <- ggplot(plot_data, aes(x = Time, y = !!sym(plot_params$y_column), color = interaction(Region, WasteType))) +
          geom_line() +
          labs(title = "Waste Recycling Over Time",
               x = "Time",
               y = plot_params$y_label) +
          theme_bw()
        ggplotly(p)
      })
      
      # Results Table -----------------------------------------------------------
      
      output$results_table <- renderDT({
        datatable(results(), options = list(pageLength = 10, autoWidth = TRUE))
      })
      
      # Cost Plots -----------------------------------------------------------
      
      generate_cost_plot_data <- function(cost_type) {
        data <- costs()
        data <- data |>
          select(Time, Region, WasteType, !!sym(cost_type)) |>
          arrange(Time)
        return(data)
      }
      
      output$collection_cost_plot <- renderPlotly({
        plot_data <- generate_cost_plot_data("CollectionCost")
        p <- ggplot(plot_data, aes(x = Time, y = !!sym("CollectionCost"), color = interaction(Region, WasteType))) +
          geom_line() +
          labs(title = "Collection Cost Over Time",
               x = "Time",
               y = "Collection Cost") +
          theme_bw()
        ggplotly(p)
      })
      
      output$processing_cost_plot <- renderPlotly({
        plot_data <- generate_cost_plot_data("ProcessingCost")
        p <- ggplot(plot_data, aes(x = Time, y = !!sym("ProcessingCost"), color = interaction(Region, WasteType))) +
          geom_line() +
          labs(title = "Processing Cost Over Time",
               x = "Time",
               y = "Processing Cost") +
          theme_bw()
        ggplotly(p)
      })
      
      output$transportation_cost_plot <- renderPlotly({
        plot_data <- generate_cost_plot_data("TransportationCost")
        p <- ggplot(plot_data, aes(x = Time, y = !!sym("TransportationCost"), color = interaction(Region, WasteType))) +
          geom_line() +
          labs(title = "Transportation Cost Over Time",
               x = "Time",
               y = "Transportation Cost") +
          theme_bw()
        ggplotly(p)
      })
      
      output$recycling_revenue_plot <- renderPlotly({
        plot_data <- generate_cost_plot_data("RecyclingRevenue")
        p <- ggplot(plot_data, aes(x = Time, y = !!sym("RecyclingRevenue"), color = interaction(Region, WasteType))) +
          geom_line() +
          labs(title = "Recycling Revenue Over Time",
               x = "Time",
               y = "Recycling Revenue") +
          theme_bw()
        ggplotly(p)
      })
      
      # Costs Table -----------------------------------------------------------
      
      output$costs_table <- renderDT({
        datatable(costs(), options = list(pageLength = 10, autoWidth = TRUE))
      })
      
      # Distance Matrix -----------------------------------------------------------
      
      output$distance_matrix <- renderTable({
        distance_matrix
      }, rownames = TRUE, colnames = TRUE)
      
      output$distance_matrix_plot <- renderPlotly({
        regions <- unlist(strsplit(input$regions, ","))
        melted_matrix <- melt(distance_matrix)
        colnames(melted_matrix) <- c("Region1", "Region2", "Distance")
        
        plot_ly(
          data = melted_matrix,
          x = ~Region2,
          y = ~Region1,
          z = ~Distance,
          type = "heatmap",
          colorscale = "Reds",
          colorbar = list(title = "Distance")
        ) %>%
          layout(
            xaxis = list(title = "Region", side = "top"),  # Flip the order of regions on the x-axis
            yaxis = list(title = "Region", autorange = "reversed")  # Flip the order of regions on the y-axis
          )
        
      })
      
      # Force Directed Plot -----
      
      output$force_directed_plot <- renderPlotly({
        regions <- unlist(strsplit(input$regions, ","))
        distance_matrix
        colnames(distance_matrix) <- regions
        rownames(distance_matrix) <- regions
        
        # Create an igraph object from the distance matrix
        graph <- graph_from_adjacency_matrix(as.matrix(distance_matrix), mode = "undirected", weighted = TRUE, diag = FALSE)
        
        # Get the layout for the graph
        layout <- layout_with_fr(graph)
        
        # Create a data frame for the nodes
        nodes <- data.frame(
          id = V(graph)$name,
          x = layout[, 1],
          y = layout[, 2]
        )
        
        # Create a data frame for the edges
        edges <- as.data.frame(as_edgelist(graph))
        colnames(edges) <- c("from", "to")
        edges$weight <- E(graph)$weight
        
        # Plot the graph with plotly
        plot_ly() %>%
          add_markers(data = nodes, x = ~x, y = ~y, text = ~id, mode = 'markers+text', textposition = 'top center', hoverinfo = 'text') %>%
          add_segments(data = edges, x = ~nodes$x[match(edges$from, nodes$id)], 
                       xend = ~nodes$x[match(edges$to, nodes$id)], 
                       y = ~nodes$y[match(edges$from, nodes$id)], 
                       yend = ~nodes$y[match(edges$to, nodes$id)], 
                       line = list(color = 'black', width = ~weight/5)) %>%
          layout(title = "Force-Directed Layout of Regions",
                 xaxis = list(title = "", showgrid = FALSE, zeroline = FALSE),
                 yaxis = list(title = "", showgrid = FALSE, zeroline = FALSE))
      })
      
      })
    })
  
  }
)
