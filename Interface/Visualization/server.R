library(shiny)
library(simmer)
library(dplyr)
library(ggplot2)
library(DT)
library(plotly)

shinyServer(function(input, output, session) {
  
  # Define a function to load scenario parameters
  load_scenario_parameters <- function(scenario) {
    params <- list(
      regions = c("North", "South", "East", "West"),
      wood_waste_types = c("Construction", "Demolition", "Packaging"),
      storage_cost_per_unit = 0.5,
      collection_cost_per_unit = 1,
      processing_cost_per_unit = 2,
      transportation_cost_per_unit = 0.3,
      overflow_penalty_per_unit = 2,
      recycling_revenue_per_unit = 3,
      avoided_disposal_cost_per_unit = 1,
      storage_capacity = 500
    )
    
    if (scenario == "increased_waste_generation") {
      params$generation_rate_increase = 2
    } else if (scenario == "improved_collection_efficiency") {
      params$collection_rate_increase = 1.5
    } else if (scenario == "enhanced_recycling_programs") {
      params$recycling_revenue_per_unit = 5
      params$avoided_disposal_cost_per_unit = 2
    } else if (scenario == "storage_capacity_expansion") {
      params$storage_capacity = 1000
    } else if (scenario == "penalties_for_overflow") {
      params$overflow_penalty_per_unit = 5
    } else if (scenario == "different_transportation_costs") {
      params$transportation_cost_per_unit = 0.5
    } else if (scenario == "seasonal_variations") {
      params$seasonal_variations = TRUE
    }
    
    return(params)
  }
  
  load_parameters <- reactive({
    list(
      regions = unlist(strsplit(input$regions, ",")),
      wood_waste_types = unlist(strsplit(input$wood_waste_types, ",")),
      storage_cost_per_unit = input$storage_cost_per_unit,
      collection_cost_per_unit = input$collection_cost_per_unit,
      processing_cost_per_unit = input$processing_cost_per_unit,
      transportation_cost_per_unit = input$transportation_cost_per_unit,
      overflow_penalty_per_unit = input$overflow_penalty_per_unit,
      recycling_revenue_per_unit = input$recycling_revenue_per_unit,
      avoided_disposal_cost_per_unit = input$avoided_disposal_cost_per_unit,
      storage_capacity = input$storage_capacity
    )
  })
  
  # Update UI based on selected scenario
  observe({
    scenario <- input$scenario
    params <- load_scenario_parameters(scenario)
    
    updateNumericInput(session, "storage_cost_per_unit", value = params$storage_cost_per_unit)
    updateNumericInput(session, "collection_cost_per_unit", value = params$collection_cost_per_unit)
    updateNumericInput(session, "processing_cost_per_unit", value = params$processing_cost_per_unit)
    updateNumericInput(session, "transportation_cost_per_unit", value = params$transportation_cost_per_unit)
    updateNumericInput(session, "overflow_penalty_per_unit", value = params$overflow_penalty_per_unit)
    updateNumericInput(session, "recycling_revenue_per_unit", value = params$recycling_revenue_per_unit)
    updateNumericInput(session, "avoided_disposal_cost_per_unit", value = params$avoided_disposal_cost_per_unit)
    updateNumericInput(session, "storage_capacity", value = params$storage_capacity)
  })
  
  output$dynamic_ui <- renderUI({
    fluidRow(
      textInput("regions", "Regions (comma-separated):", "North,South,East,West"),
      textInput("wood_waste_types", "Wood Waste Types (comma-separated):", "Construction,Demolition,Packaging"),
      numericInput("storage_cost_per_unit", "Storage Cost per Unit:", 0.5),
      numericInput("collection_cost_per_unit", "Collection Cost per Unit:", 1),
      numericInput("processing_cost_per_unit", "Processing Cost per Unit:", 2),
      numericInput("transportation_cost_per_unit", "Transportation Cost per Unit:", 0.3),
      numericInput("overflow_penalty_per_unit", "Overflow Penalty per Unit:", 2),
      numericInput("recycling_revenue_per_unit", "Recycling Revenue per Unit:", 3),
      numericInput("avoided_disposal_cost_per_unit", "Avoided Disposal Cost per Unit:", 1),
      numericInput("storage_capacity", "Storage Capacity per Region:", 500)
    )
  })
  
  # Initialize a data frame to store the simulation results
  results <- reactiveVal(data.frame(
    Time = numeric(),
    Region = character(),
    WasteType = character(),
    Event = character(),
    Amount = numeric(),
    stringsAsFactors = FALSE
  ))
  
  log_event <- function(env, region, waste_type, event) {
    time <- now(env)
    amount <- get_attribute(env, "waste_amount")
    new_entry <- data.frame(
      Time = time,
      Region = region,
      WasteType = waste_type,
      Event = event,
      Amount = amount,
      stringsAsFactors = FALSE
    )
    results(rbind(results(), new_entry))
  }
  
  create_trajectories <- function(env, params, region, waste_type) {
    generate_waste <- trajectory(paste("Generate Waste -", region, "-", waste_type)) |>
      seize("storage", 1) |>
      set_attribute("waste_amount", function() runif(1, 10, 30)) |>
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
        waste_amount <- get_attribute(env, "waste_amount")
        get_global(env, paste0("total_storage_cost_", region, "_", waste_type)) + waste_amount * params$storage_cost_per_unit
      }) |>
      timeout(function() rexp(1, rate = 0.5)) |>
      release("storage", 1) |>
      set_attribute("log_event", function() log_event(env, region, waste_type, "Generation"))
    
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
        get_global(env, paste0("total_collection_cost_", region, "_", waste_type)) + waste_amount * params$collection_cost_per_unit
      }) |>
      set_global(paste0("total_transportation_cost_", region, "_", waste_type), function() {
        waste_amount <- get_attribute(env, "waste_amount")
        get_global(env, paste0("total_transportation_cost_", region, "_", waste_type)) + waste_amount * params$transportation_cost_per_unit
      }) |>
      set_global(paste0("total_waste_stored_", region, "_", waste_type), function() {
        current_storage <- get_global(env, paste0("total_waste_stored_", region, "_", waste_type))
        waste_amount <- get_attribute(env, "waste_amount")
        max(current_storage - waste_amount, 0)
      }) |>
      timeout(function() rexp(1, rate = 1.5)) |>
      release("storage", 1) |>
      release("collection_truck") |>
      set_attribute("log_event", function() log_event(env, region, waste_type, "Collection"))
    
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
        get_global(env, paste0("total_processing_cost_", region, "_", waste_type)) + waste_amount * params$processing_cost_per_unit
      }) |>
      set_global(paste0("total_waste_recycled_", region, "_", waste_type), function() {
        recycled_amount <- runif(1, 1, 7)
        get_global(env, paste0("total_waste_recycled_", region, "_", waste_type)) + recycled_amount
      }) |>
      set_global(paste0("total_recycling_revenue_", region, "_", waste_type), function() {
        recycled_amount <- get_attribute(env, "waste_amount")
        get_global(env, paste0("total_recycling_revenue_", region, "_", waste_type)) + recycled_amount * params$recycling_revenue_per_unit
      }) |>
      set_global(paste0("total_avoided_disposal_cost_", region, "_", waste_type), function() {
        recycled_amount <- get_attribute(env, "waste_amount")
        get_global(env, paste0("total_avoided_disposal_cost_", region, "_", waste_type)) + recycled_amount * params$avoided_disposal_cost_per_unit
      }) |>
      timeout(function() rexp(1, rate = 0.7)) |>
      release("waste_processor") |>
      release("collection_truck") |>
      set_attribute("log_event", function() log_event(env, region, waste_type, "Handling")) |>
      set_attribute("log_event", function() log_event(env, region, waste_type, "Recycling"))
    
    list(generate_waste = generate_waste, collect_waste = collect_waste, handle_waste = handle_waste)
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
  
  balance_storage_across_regions <- function(env, params) {
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
                  current_storage <- current_storage + actual_transfer_amount
                  source_storage <- source_storage - actual_transfer_amount
                  transfer_amount <- transfer_amount - actual_transfer_amount
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
                  current_storage <- current_storage - actual_transfer_amount
                  target_storage <- target_storage + actual_transfer_amount
                  transfer_amount <- transfer_amount + actual_transfer_amount
                  if (transfer_amount >= 0) break
                }
              }
            }
          }
        }
        
        cat("Final storage in region", region, "for waste type", waste_type, ":", current_storage, "kg\n")
      }
    }
  }
  
  observeEvent(input$run_simulation, {
    withProgress(message = 'Running simulation...', value = 0, {
      # Reset results before running the simulation
      results(data.frame(
        Time = numeric(),
        Region = character(),
        WasteType = character(),
        Event = character(),
        Amount = numeric(),
        stringsAsFactors = FALSE
      ))
      
      scenario <- input$scenario
      params <- load_scenario_parameters(scenario)
      
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
        add_resource("waste_processor", capacity = length(params$regions))
      
      trajectories <- list()
      for (region in params$regions) {
        for (waste_type in params$wood_waste_types) {
          trajs <- create_trajectories(env, params, region, waste_type)
          trajectories[[paste(region, waste_type, sep = "_")]] <- trajs
          env <- env |>
            add_generator(paste0("waste_gen_", region, "_", waste_type), trajs$generate_waste, function() rexp(1, rate = 0.5 / ifelse(is.null(params$generation_rate_increase), 1, params$generation_rate_increase))) |>
            add_generator(paste0("waste_collector_", region, "_", waste_type), trajs$collect_waste, function() rexp(1, rate = 1.5 / ifelse(is.null(params$collection_rate_increase), 1, params$collection_rate_increase))) |>
            add_generator(paste0("waste_handler_", region, "_", waste_type), trajs$handle_waste, function() rexp(1, rate = 1))
        }
      }
      
      incProgress(0.5)
      env %>% run(until = 100)
      incProgress(0.9)
      
      balance_storage_across_regions(env, params)
      incProgress(1)
      
      output$simulation_output <- renderPrint({
        for (region in params$regions) {
          for (waste_type in params$wood_waste_types) {
            cat("Region:", region, "Waste Type:", waste_type, "\n")
            cat("  Total waste generated (kg):", get_global(env, paste0("total_waste_generated_", region, "_", waste_type)), "\n")
            cat("  Total waste collected (kg):", get_global(env, paste0("total_waste_collected_", region, "_", waste_type)), "\n")
            cat("  Total waste handled (kg):", get_global(env, paste0("total_waste_handled_", region, "_", waste_type)), "\n")
            cat("  Total waste stored (kg):", get_global(env, paste0("total_waste_stored_", region, "_", waste_type)), "\n")
            cat("  Total waste recycled (kg):", get_global(env, paste0("total_waste_recycled_", region, "_", waste_type)), "\n")
            cat("  Overflow penalty:", get_global(env, paste0("overflow_penalty_", region, "_", waste_type)), "\n")
            cat("  Total storage cost:", get_global(env, paste0("total_storage_cost_", region, "_", waste_type)), "\n")
            cat("  Total collection cost:", get_global(env, paste0("total_collection_cost_", region, "_", waste_type)), "\n")
            cat("  Total processing cost:", get_global(env, paste0("total_processing_cost_", region, "_", waste_type)), "\n")
            cat("  Total transportation cost:", get_global(env, paste0("total_transportation_cost_", region, "_", waste_type)), "\n")
            cat("  Total recycling revenue:", get_global(env, paste0("total_recycling_revenue_", region, "_", waste_type)), "\n")
            cat("  Total avoided disposal cost:", get_global(env, paste0("total_avoided_disposal_cost_", region, "_", waste_type)), "\n")
            cat("\n")
          }
        }
        
        total_storage_cost <- 0
        for (region in params$regions) {
          for (waste_type in params$wood_waste_types) {
            total_storage_cost <- total_storage_cost + get_global(env, paste0("total_waste_stored_", region, "_", waste_type)) * params$storage_cost_per_unit
          }
        }
        cat("Total storage cost:", total_storage_cost, "\n")
      })
      
      # Add cumulative totals for each event
      output$cumulative_totals <- renderPrint({
        data <- results()
        total_generation <- sum(data %>% filter(Event == "Generation") %>% pull(Amount))
        total_collection <- sum(data %>% filter(Event == "Collection") %>% pull(Amount))
        total_handling <- sum(data %>% filter(Event == "Handling") %>% pull(Amount))
        total_recycling <- sum(data %>% filter(Event == "Recycling") %>% pull(Amount))
        
        cat("Cumulative Totals:\n")
        cat("  Total waste generated (kg):", total_generation, "\n")
        cat("  Total waste collected (kg):", total_collection, "\n")
        cat("  Total waste handled (kg):", total_handling, "\n")
        cat("  Total waste recycled (kg):", total_recycling, "\n")
      })
      
      generate_plot_data <- function(event_type) {
        data <- results()
        data <- data %>% filter(Event == event_type) %>%
          arrange(Time) %>%
          group_by(Region, WasteType) %>%
          mutate(CumulativeAmount = cumsum(Amount))
        data
      }
      
      output$generation_plot <- renderPlotly({
        plot_data <- generate_plot_data("Generation")
        p <- ggplot(plot_data, aes(x = Time, y = if(input$plot_type == "cumulative") CumulativeAmount else Amount, color = interaction(Region, WasteType))) +
          geom_line() +
          labs(title = "Waste Generation Over Time",
               x = "Time",
               y = if(input$plot_type == "cumulative") "Cumulative Amount of Waste Generated" else "Amount of Waste Generated") +
          theme_minimal()
        ggplotly(p)
      })
      
      output$collection_plot <- renderPlotly({
        plot_data <- generate_plot_data("Collection")
        p <- ggplot(plot_data, aes(x = Time, y = if(input$plot_type == "cumulative") CumulativeAmount else Amount, color = interaction(Region, WasteType))) +
          geom_line() +
          labs(title = "Waste Collection Over Time",
               x = "Time",
               y = if(input$plot_type == "cumulative") "Cumulative Amount of Waste Collected" else "Amount of Waste Collected") +
          theme_minimal()
        ggplotly(p)
      })
      
      output$handling_plot <- renderPlotly({
        plot_data <- generate_plot_data("Handling")
        p <- ggplot(plot_data, aes(x = Time, y = if(input$plot_type == "cumulative") CumulativeAmount else Amount, color = interaction(Region, WasteType))) +
          geom_line() +
          labs(title = "Waste Handling Over Time",
               x = "Time",
               y = if(input$plot_type == "cumulative") "Cumulative Amount of Waste Handled" else "Amount of Waste Handled") +
          theme_minimal()
        ggplotly(p)
      })
      
      output$recycling_plot <- renderPlotly({
        plot_data <- generate_plot_data("Recycling")
        p <- ggplot(plot_data, aes(x = Time, y = if(input$plot_type == "cumulative") CumulativeAmount else Amount, color = interaction(Region, WasteType))) +
          geom_line() +
          labs(title = "Waste Recycling Over Time",
               x = "Time",
               y = if(input$plot_type == "cumulative") "Cumulative Amount of Waste Recycled" else "Amount of Waste Recycled") +
          theme_minimal()
        ggplotly(p)
      })
      
      output$results_table <- renderDT({
        datatable(results(), options = list(pageLength = 10, autoWidth = TRUE))
      })
    })
  })
})
