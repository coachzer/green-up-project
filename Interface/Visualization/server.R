# install.packages("shiny", "simmer", "stringr", "dplyr", "tidyr", "ggplot2", "readr", "DT", "plotly", "shinyjs", "shinydashboard", "shinyBS", "reshape2", "igraph")

library(shiny)
library(simmer)
library(stringr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(DT)
library(plotly)
library(shinyjs)
library(shinydashboard)
library(shinyBS)
library(reshape2)
library(igraph)

# Do not forget to set your working directory to the Visualization folder
setwd("C:/Users/kovac/Desktop/Work/Green UP Project/green-up-project/Interface/Visualization")

# Colorblind-friendly palette
color_palette <- c("waste_from_producers_no_record" = "#E69F00",  # Orange
                   "waste_from_producers_with_record" = "#56B4E9",  # Sky blue
                   "waste_from_collectors_RS" = "#009E73",  # Green
                   "waste_from_processors_RS" = "#F0E442")  # Yellow

# data ----
## import from the data folder

### Generation ----
gnr_data <- read_csv("data/gnr_combined.csv")

#### data for gnr_data infoBox ----

# total waste
total_waste <- gnr_data |>
  summarize(
    total_generated = sum(generated_in_the_year, na.rm = TRUE)
  )

total_waste <- round(total_waste$total_generated, 2)

# total waste treated by the original generator
total_treated_by_producer <- gnr_data |>
  summarize(
    total_treated_by_producer = sum(waste_treated_by_original_generator, na.rm = TRUE)
  )

total_treated_by_producer <- round(total_treated_by_producer$total_treated_by_producer, 2)

# total waste transferred for treatment in RS
totaL_transferred_RS <- gnr_data |>
  summarize(
    total_transferred_RS = sum(waste_transferred_for_treatment_in_RS, na.rm = TRUE)
  )

total_transferred_RS <- round(totaL_transferred_RS$total_transferred_RS, 2)

# total sent to EU
total_sent_EU <- gnr_data |>
  summarize(
    total_sent_EU = sum(waste_sent_for_treatment_EU, na.rm = TRUE)
  )

total_sent_EU <- round(total_sent_EU$total_sent_EU, 2)

# total sent outside of EU
total_sent_outside_EU <- gnr_data |>
  summarize(
    total_sent_outside_EU = sum(waste_sent_for_treatment_outside_EU, na.rm = TRUE)
  )

total_sent_outside_EU <- round(total_sent_outside_EU$total_sent_outside_EU, 2)

### Collection ----
coll_storage_data <- read_csv("data/coll_storage_combined.csv")
coll_received_data <- read_csv("data/coll_received_combined.csv")
coll_municipal_data <- read_csv("data/coll_municipal_combined.csv")
coll_municipal_collected_data <- read_csv("data/coll_municipal_collected_combined.csv")
coll_management_data <- read_csv("data/coll_management_combined.csv")
#### coll_storage_data ----

# yearly data plot

yearly_data <- coll_storage_data |>
  group_by(year) |>
  summarize(
    total_start = sum(waste_stored_start_year, na.rm = TRUE),
    total_end = sum(waste_stored_end_year, na.rm = TRUE)
  )

# variant ----
# Prepare the yearly data
yearly_data <- coll_storage_data |>
  group_by(year) |>
  summarize(
    total_start = sum(waste_stored_start_year, na.rm = TRUE),
    total_end = sum(waste_stored_end_year, na.rm = TRUE)
  )

# Create the variant data
variant_data <- yearly_data |>
  arrange(year) |>
  mutate(
    end_year = paste0(year, " End"),
    start_next_year = paste0(year + 1, " Start"),
    end_amount = total_end,
    start_amount = lead(total_start),
    difference = lead(total_start) - total_end
  ) |>
  select(end_year, start_next_year, end_amount, start_amount, difference) |>
  pivot_longer(
    cols = c(end_year, start_next_year),
    names_to = "type",
    values_to = "year"
  ) |>
  mutate(
    amount = ifelse(type == "end_year", end_amount, start_amount),
    difference = ifelse(type == "start_next_year", difference, 0),
    cumulative = cumsum(amount),
    color_category = case_when(
      type == "end_year" ~ "End Year",
      difference > 0 ~ "Increase",
      difference < 0 ~ "Decrease",
      TRUE ~ "No Change"
    )
  ) |>
  filter(!is.na(start_amount))

# Create a new column for ordered factor
variant_data$year <- with(variant_data, 
                          paste(year, ifelse(type == "end_year", "", ""), sep = " "))
# Ensure 'year' is a factor with the desired order
variant_data$year <- factor(variant_data$year, 
                            levels = unique(variant_data$year))

# Prepare data for side-by-side bars
variant_data_long <- variant_data |>
  pivot_longer(
    cols = c(amount, difference),
    names_to = "bar_type",
    values_to = "value"
  ) |>
  mutate(
    bar_category = case_when(
      bar_type == "amount" & type == "end_year" ~ "End Year",
      bar_type == "amount" & type != "end_year" ~ "Start Amount",
      bar_type == "difference" & color_category == "Increase" ~ "Increase",
      bar_type == "difference" & color_category == "Decrease" ~ "Decrease",
      TRUE ~ "No Change"
    )
  )

# Define the desired order of bar categories
desired_order <- c("Start Amount", "Increase", "End Year", "Decrease", "No Change")

# Reorder the data based on the desired order of bar categories
variant_data_long <- variant_data_long |> 
  mutate(bar_category = factor(bar_category, levels = desired_order)) |> 
  arrange(bar_category)

# Create the variant waterfall plot with side-by-side bars
variant_plot <- ggplot(variant_data_long, aes(x = year, y = value, fill = bar_category)) +
  geom_col(color = "black", aes(
    text = paste0(
      "Year: ",
      year,
      "<br>",
      "Type: ",
      bar_type,
      "<br>",
      "Value: ",
      round(value, 2),
      " tons<br>"
    )
  )) +
  geom_text(
    aes(
      label = ifelse(value > 0, round(value, 1), ifelse(value == 0, NA, round(value, 1))),
      y = ifelse(value >= 0, value, value) + 0.05 * max(value)
    ),
    position = position_dodge(width = 0.7),
    vjust = -0.5,
    size = 3
  ) +
  scale_fill_manual(
    values = c(
      "End Year" = "#4169E1",
      "Increase" = "#006400",
      "Start Amount" = "#808080",
      "Decrease" = "#8B0000",
      "No Change" = "#D3D3D3"
    ),
    name = "Type"
  ) +
  labs(x = "Year", y = "Waste Amount (tons)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Convert variant plot to plotly for interactivity
variant_plot <- ggplotly(variant_plot, tooltip = "text") |> 
  layout(xaxis = list(autorange = TRUE), yaxis = list(autorange = TRUE))

#### coll_received_data ----
##### data for coll_received_data infoBox 
total_waste_collected <- round(sum(coll_received_data$total_waste_collected, na.rm = TRUE), 2)

highest_region <- coll_received_data |>
  group_by(statistical_region) |>
  summarize(total = sum(total_waste_collected, na.rm = TRUE)) |>
  arrange(desc(total)) |>
  slice(1) |>
  pull(statistical_region)

waste_from_producers_no_record <- round(sum(coll_received_data$waste_from_producers_no_record, na.rm = TRUE), 2)

waste_from_producers_with_record <- round(sum(coll_received_data$waste_from_producers_with_record, na.rm = TRUE), 2)

waste_from_collectors_RS <- round(sum(coll_received_data$waste_from_collectors_RS, na.rm = TRUE), 2)

waste_from_processors_RS <- round(sum(coll_received_data$waste_from_processors_RS, na.rm = TRUE), 2)

##### yearly data received plot

yearly_data_received <- coll_received_data |>
  group_by(year) |>
  summarize(
    total_collected = sum(total_waste_collected, na.rm = TRUE),
    from_producers_no_record = sum(waste_from_producers_no_record, na.rm = TRUE),
    from_producers_with_record = sum(waste_from_producers_with_record, na.rm = TRUE),
    from_collectors_RS = sum(waste_from_collectors_RS, na.rm = TRUE),
    from_processors_RS = sum(waste_from_processors_RS, na.rm = TRUE)
  )

##### stacked, faceted, grouped plots
# Reshape data for plotting
df_long_received <- melt(coll_received_data,
                         id.vars = c("statistical_region", "type_of_waste", "year"),
                         measure.vars = c("waste_from_producers_no_record", "waste_from_producers_with_record", "waste_from_collectors_RS", "waste_from_processors_RS"),
                         variable.name = "source", value.name = "total_collected"
)

# Calculate total collected waste (excluding total_waste_collected)
total_collected_per_region <- df_long_received |>
  group_by(statistical_region) |>
  summarize(total_collected = sum(total_collected, na.rm = TRUE)) |>
  arrange(desc(total_collected))

# Reorder the regions in descending order of total_collected
df_long_received$statistical_region <- factor(df_long_received$statistical_region, 
                                              levels = total_collected_per_region$statistical_region)

# Stacked bar plot
t_stacked <- ggplot(df_long_received, aes(x = statistical_region, y = total_collected, fill = source)) +
  geom_col() + 
  labs(
    title = "Waste Received by Source",
    x = "Statistical Region", y = "Total Collected Waste"
  ) +
  scale_fill_manual(
    values = color_palette,
    labels = c("From Producers (No Record)", "From Producers (With Record)", "From Collectors (RS)", "From Processors (RS)")
  ) +
  # coord_flip() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Faceted bar plot by source
t_faceted <- ggplot(df_long_received, aes(x = statistical_region, y = total_collected, fill = source)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ source, ncol = 1, scales = "free_y") +  # Create a facet for each source
  labs(
    title = "Waste Received by Source",
    x = "Statistical Region", y = "Total Collected Waste"
  ) +
  scale_fill_manual(
    values = color_palette,
    labels = c("From Producers (No Record)", "From Producers (With Record)", "From Collectors (RS)", "From Processors (RS)")
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Grouped bar plot
t_grouped <- ggplot(df_long_received, aes(x = statistical_region, y = total_collected, fill = source)) +
  geom_bar(stat = "identity", position = "dodge") +  # Group the bars by source
  labs(
    title = "Waste Received by Source",
    x = "Statistical Region", y = "Total Collected Waste"
  ) +
  scale_fill_manual(
    values = color_palette,
    labels = c("From Producers (No Record)", "From Producers (With Record)", "From Collectors (RS)", "From Processors (RS)")
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#### coll_municipal_data ----

# Reshape data for plotting
df_long_municipal <- melt(
  coll_municipal_data,
  id.vars = c(
    "year",
    "statistical_region",
    "name_of_municipality",
    "type_of_waste"
  ),
  measure.vars = c("total_waste_collected"),
  variable.name = "source",
  value.name = "total_collected"
)

t10 <- ggplot(df_long_municipal, aes(x = year, y = total_collected, color = name_of_municipality, group = type_of_waste)) +
  geom_line() +
  geom_point() +
  facet_wrap(~statistical_region, scales = "free_y") +
  labs(title = "Total Waste Collected by Municipality and Region (2018-2022)",
       x = "Year",
       y = "Total Waste Collected (in tons)",
       color = "Municipality") +
  theme_minimal()

# Calculate total waste for each municipality
top_municipalities <- df_long_municipal |>
  group_by(name_of_municipality) |>
  summarise(total_waste = sum(total_collected, na.rm = TRUE)) |>
  top_n(5, total_waste) |>
  pull(name_of_municipality)

# Filter the original dataset
df_top_municipal <- df_long_municipal |>
  filter(name_of_municipality %in% top_municipalities)

# Plot
t11 <- ggplot(df_top_municipal, aes(x = year, y = total_collected, color = name_of_municipality, group = name_of_municipality)) +
  geom_line() +
  geom_point() +
  labs(title = "Total Waste Collected by Top 5 Municipalities (2018-2022)",
       x = "Year",
       y = "Total Waste Collected (in tons)",
       color = "Municipality") +
  facet_wrap(~type_of_waste) +
  theme_minimal() +
  theme(legend.position = "bottom")

#### coll_municipal_collected_data ----

df_long_collected <- melt(
  coll_municipal_collected_data,
  id.vars = c(
    "year",
    "statistical_region",
    "name_of_municipality",
    "type_of_waste"
  ),
  measure.vars = c("total_waste_collected"),
  variable.name = "source",
  value.name = "total_collected"
)

# 1. Time series plot of total waste collected by year
plot_total_waste_by_year <- function(data) {
  # Summarize total waste collected per year
  summarized_data <- data |>
    group_by(year) |>
    summarize(total_collected = sum(total_collected, na.rm = TRUE)) |>
    ungroup()
  
  # Create the time series plot using Plotly
  p <- plot_ly(
    data = summarized_data,
    x = ~year,
    y = ~total_collected,
    type = 'scatter',
    mode = 'lines+markers',
    marker = list(size = 6),
    line = list(width = 2)
  ) |>
    layout(
      title = "Total Waste Collected by Year",
      xaxis = list(title = "Year", range = c(min(data$year), max(data$year))),
      yaxis = list(title = "Total Waste Collected"),
      showlegend = FALSE
    )
  
  return(p)
}

# 2. Bar plot of waste collected by municipality for a specific year
plot_waste_by_municipality <- function(data, selected_year, top_n) {
  # Summarize data for the selected year and top_n municipalities
  summarized_data <- data |>
    filter(year == selected_year) |>
    group_by(name_of_municipality) |>
    summarize(total_collected = sum(total_collected, na.rm = TRUE)) |>
    top_n(top_n, total_collected) |>
    ungroup() 
  
  # Create the bar plot using Plotly
  p <- plot_ly(
    data = summarized_data,
    x = ~reorder(name_of_municipality, -total_collected),
    y = ~total_collected,
    type = 'bar'
  ) |>
    layout(
      title = paste("Top", top_n, "Municipalities by Waste Collected in", selected_year),
      xaxis = list(title = "Municipality", tickangle = -45),
      yaxis = list(title = "Total Waste Collected"),
      showlegend = FALSE
    )
  
  return(p)
}

# 3. Stacked bar plot of waste types by year
plot_waste_types_by_year <- function(data) {
  # Summarize data by year and type of waste
  summarized_data <- data |>
    group_by(year, type_of_waste) |>
    summarize(total_collected = sum(total_collected, na.rm = TRUE)) |>
    ungroup()
  
  # Create the stacked bar plot using Plotly
  p <- plot_ly(
    data = summarized_data,
    x = ~as.factor(year),
    y = ~total_collected,
    color = ~type_of_waste,
    type = 'bar',
    text = ~paste("Year:", year, "<br>Total Collected:", total_collected),
    hoverinfo = 'text'
  ) |>
    layout(
      title = "Waste Types Collected by Year",
      xaxis = list(title = "Year"),
      yaxis = list(title = "Total Waste Collected"),
      barmode = 'stack', # This creates the stacked bar effect
      legend = list(title=list(text='Type of Waste'))
    )
  
  return(p)
}

# 4. Heatmap of waste collection by statistical region and year
plot_heatmap_by_region <- function(data) {
  # Summarize data by year and statistical region
  summarized_data <- data |>
    group_by(year, statistical_region) |>
    summarize(total_collected = sum(total_collected, na.rm = TRUE)) |>
    ungroup()
  
  # Create the heatmap using Plotly
  p <- plot_ly(
    data = summarized_data,
    x = ~as.factor(year),
    y = ~statistical_region,
    z = ~total_collected,
    type = "heatmap",
    colors = colorRamp(c("blue", "yellow", "red")), # You can customize colors here
    colorbar = list(title = "Total Collected")
  ) |>
    layout(
      title = "Heatmap of Waste Collection by Region and Year",
      xaxis = list(title = "Year"),
      yaxis = list(title = "Statistical Region")
    )
  
  return(p)
}

#### coll_management_data ----

# Reshape data for plotting
df_long_management <- melt(
  coll_management_data,
  id.vars = c(
    "statistical_region",
    "type_of_waste",
    "year"
  ),
  measure.vars = c(
    "waste_handed_to_collectors_RS",
    "waste_delivered_to_operators_RS",
    "waste_sent_to_EU",
    "waste_sent_to_non_EU"
  ),
  variable.name = "source",
  value.name = "total_waste_given_away"
)

### Treatment ----
#### trt_storage_data ----
trt_storage_data <- read_csv("data/trt_storage_combined.csv")
#### trt_collected_data ----
trt_collected_data <- read_csv("data/trt_collected_combined.csv")
#### trt_treatment_data ----
trt_treatment_data <- read_csv("data/trt_treatment_combined.csv")

#### trt_municipal_waste_received_data ----
trt_municipal_waste_received_data <- read_csv("data/trt_municipal_waste_received_combined.csv")
#### trt_input_treatment_data ----
trt_input_treatment_data <- read_csv("data/trt_input_treatment_combined.csv")

trt_input_treatment_data <- trt_input_treatment_data |> 
  group_by(year, type_of_waste) |>
  summarise(mass_change = sum(mass_change, na.rm = TRUE)) |>
  ungroup()

# shinyServer ----
shinyServer(function(input, output, session) {
  
  # Hidden UI elements -----
  observeEvent(input$sidebarItemExpanded, {
    if(input$sidebarItemExpanded == "ANALYSIS"){
      updateTabItems(session, "tabs", selected = "hiddenAnalysis")
    }
    
    if(input$sidebarItemExpanded == "SIMULATION"){
      updateTabItems(session, "tabs", selected = "hiddenSimulation")
    }
    
  })
  
  # Analysis tab -----
  
  ## Generation ----
  
  # Load data for generation
  output$generationData <- DT::renderDataTable({
    gnr_data
  })
  
  # infoBox for total waste generated 
  output$totalWasteGenerated <- renderInfoBox({
    infoBox(
      "Total Waste Generated",
      paste(total_waste, " tons"),
      icon = icon("trash"),
      color = "purple"
    )
  })
  
  # infoBox for total waste treated by the original generator
  output$totalTreatedByProducer <- renderInfoBox({
    infoBox(
      "Total Waste Treated by Producer",
      paste(total_treated_by_producer, " tons"),
      icon = icon("recycle"),
      color = "green"
    )
  })
  
  # infoBox for total waste transferred for treatment in RS
  output$totalTransferredRS <- renderInfoBox({
    infoBox(
      "Total Waste Transferred for Treatment in RS",
      paste(total_transferred_RS, " tons"),
      icon = icon("truck"),
      color = "blue"
    )
  })
  
  # infoBox for total waste sent to EU
  output$totalSentEU <- renderInfoBox({
    infoBox(
      "Total Waste Sent to EU",
      paste(total_sent_EU, " tons"),
      icon = icon("plane"),
      color = "yellow"
    )
  })
  
  # infoBox for total waste sent outside of EU
  output$totalSentOutsideEU <- renderInfoBox({
    infoBox(
      "Total Waste Sent Outside of EU",
      paste(total_sent_outside_EU, " tons"),
      icon = icon("globe"),
      color = "red"
    )
  })
  
  # Total waste by year plot
  output$totalWasteByYear <- renderPlotly({
    # Summarize total generated waste by year
    total_waste_by_year <- gnr_data |>
      group_by(year) |>
      summarize(total_generated_waste = sum(generated_in_the_year, na.rm = TRUE))
    
    # Create the plotly plot
    plot_ly() |>
      add_trace(
        data = total_waste_by_year,
        x = ~year,
        y = ~total_generated_waste,
        type = 'scatter',
        mode = 'lines+markers'
      ) |>
      layout(
        xaxis = list(title = "Year", autorange = TRUE),
        yaxis = list(title = "Total Generated Waste (tons)", autorange = TRUE),
        hovermode = "x"
      )
  })
  
  # Waste by region and year plot
  output$wasteByRegionYear <- renderPlotly({
    # Summarize the total generated waste by region and year
    waste_by_region_year <- gnr_data |>
      # Combine the two region name variations
      mutate(statistical_region = ifelse(statistical_region %in% c("JUGOVZHODNA SLOVENIJA", "JUGOVZHODNASLOVENIJA"), 
                                         "JUGOVZHODNA SLOVENIJA", 
                                         statistical_region)) |>
      group_by(statistical_region, year) |>
      summarize(total_generated_waste = sum(generated_in_the_year, na.rm = TRUE)) |>
      ungroup()
    
    plot_ly() |>
      add_trace(
        data = waste_by_region_year,
        x = ~year,
        y = ~total_generated_waste,
        color = ~statistical_region,
        type = 'scatter',
        mode = 'lines+markers'
      ) |>
      layout(
        xaxis = list(title = "Year", autorange = TRUE),
        yaxis = list(title = "Total Generated Waste (tons)", autorange = TRUE),
        hovermode = "x"
      )
  })
  
  # Waste by type and year plot
  output$wasteByTypeYear <- renderPlotly({
    # Summarize the total generated waste by type and year
    waste_by_type_year <- gnr_data |>
      group_by(type_of_waste, year) |>
      summarize(total_generated_waste = sum(generated_in_the_year, na.rm = TRUE)) |>
      ungroup()
    
    plot_ly() |>
      add_trace(
        data = waste_by_type_year,
        x = ~year,
        y = ~total_generated_waste,
        color = ~type_of_waste,
        type = 'scatter',
        mode = 'lines+markers'
      ) |>
      layout(
        xaxis = list(title = "Year", autorange = TRUE),
        yaxis = list(title = "Total Generated Waste (tons)", autorange = TRUE),
        hovermode = "x"
      )
  })
  
  # Waste transferred by year plot
  output$wasteTransferred <- renderPlotly({
    # Summarize the total waste transferred by year
    waste_transferred <- gnr_data |>
      group_by(year) |>
      summarize(total_sent = sum(waste_transferred_for_treatment_in_RS, na.rm = TRUE))
    
    plot_ly(
      waste_transferred,
      x = ~year,
      y = ~total_sent,
      type = 'scatter',
      mode = 'lines+markers'
    ) |>
      layout(
        xaxis = list(title = "Year", autorange = TRUE),
        yaxis = list(title = "Total Waste Transferred (tons)", autorange = TRUE),
        hovermode = "x"
      )
  })
  
  # Waste transferred by region and year plot
  output$wasteTransferredByRegionYear <- renderPlotly({
    # Summarize the total waste transferred by region and year
    waste_transferred_by_region_year <- gnr_data |>
      # Combine the two region name variations
      mutate(statistical_region = ifelse(statistical_region %in% c("JUGOVZHODNA SLOVENIJA", "JUGOVZHODNASLOVENIJA"), 
                                         "JUGOVZHODNA SLOVENIJA", 
                                         statistical_region)) |>
      group_by(statistical_region, year) |>
      summarize(total_sent = sum(waste_transferred_for_treatment_in_RS, na.rm = TRUE)) |>
      ungroup()
    
    plot_ly(
      waste_transferred_by_region_year,
      x = ~ year,
      y = ~ total_sent,
      color = ~ statistical_region,
      type = 'scatter',
      mode = 'lines+markers'
    ) |>
      layout(
        xaxis = list(title = "Year", autorange = TRUE),
        yaxis = list(title = "Total Waste Transferred (tons)", autorange = TRUE),
        hovermode = "x"
      )
  })
  
  # Waste transferred by type and year plot
  output$wasteTransferredByTypeYear <- renderPlotly({
    # Summarize the total waste transferred by type and year
    waste_transferred_by_type_year <- gnr_data |>
      group_by(type_of_waste, year) |>
      summarize(total_sent = sum(waste_transferred_for_treatment_in_RS, na.rm = TRUE)) |>
      ungroup()
    
    plot_ly(
      waste_transferred_by_type_year,
      x = ~ year,
      y = ~ total_sent,
      color = ~ type_of_waste,
      type = 'scatter',
      mode = 'lines+markers'
    ) |>
      layout(
        xaxis = list(title = "Year", autorange = TRUE),
        yaxis = list(title = "Total Waste Transferred (tons)", autorange = TRUE),
        hovermode = "x"
      )
  })
  
  # Waste stored at the end of the year plot
  output$wasteStoredEndYear <- renderPlotly({
    # Summarize the total waste stored by year
    waste_stored_at_the_end_year <- gnr_data |>
      group_by(year) |>
      summarize(total_stored = sum(temporarily_stored_end_year, na.rm = TRUE))
    
    plot_ly(
      waste_stored_at_the_end_year,
      x = ~ year,
      y = ~ total_stored,
      type = 'scatter',
      mode = 'lines+markers'
    ) |>
      layout(
        xaxis = list(title = "Year", autorange = TRUE),
        yaxis = list(title = "Total Waste Stored (tons)", autorange = TRUE),
        hovermode = "x"
      )
    
  })
  
  # Waste stored at the end of the year by region plot
  output$wasteStoredEndYearByRegionYear <- renderPlotly({
    # Summarize the total waste stored by region and year
    waste_stored_at_the_end_year_by_region <- gnr_data |>
      # Combine the two region name variations
      mutate(statistical_region = ifelse(statistical_region %in% c("JUGOVZHODNA SLOVENIJA", "JUGOVZHODNASLOVENIJA"), 
                                         "JUGOVZHODNA SLOVENIJA", 
                                         statistical_region)) |>
      # Group by the modified region and year
      group_by(statistical_region, year) |>
      summarize(total_stored = sum(temporarily_stored_end_year, na.rm = TRUE)) |>
      ungroup()
    
    
    plot_ly(
      waste_stored_at_the_end_year_by_region,
      x = ~ year,
      y = ~ total_stored,
      color = ~ statistical_region,
      type = 'scatter',
      mode = 'lines+markers'
    ) |>
      layout(
        xaxis = list(title = "Year", autorange = TRUE),
        yaxis = list(title = "Total Waste Stored (tons)", autorange = TRUE),
        hovermode = "x"
      )
    
  })
  
  # Waste stored at the end of the year by type plot
  output$wasteStoredEndYearByTypeYear <- renderPlotly({
    # Summarize the total waste stored by type of waste and year
    waste_stored_at_the_end_year_by_type <- gnr_data |>
      group_by(type_of_waste, year) |>
      summarize(total_stored = sum(temporarily_stored_end_year, na.rm = TRUE)) |>
      ungroup()
    
    plot_ly(
      waste_stored_at_the_end_year_by_type,
      x = ~ year,
      y = ~ total_stored,
      color = ~ type_of_waste,
      type = 'scatter',
      mode = 'lines+markers'
    ) |>
      layout(
        xaxis = list(title = "Year", autorange = TRUE),
        yaxis = list(title = "Total Waste Stored (tons)", autorange = TRUE),
        hovermode = "x"
      )
    
  })
  
  ## Collection ----
  
  ### Storage ----
  
  # Render the selected plot based on user input
  output$selectedPlot1 <- renderPlotly({
    if (input$plot_selection == "Total Wood Waste Over Time") {
      t <- ggplot(yearly_data, aes(x = year)) +
        geom_point(aes(y = total_start, color = "Start of Year")) +
        geom_point(aes(y = total_end, color = "End of Year")) +
        geom_line(aes(y = total_start, color = "Start of Year")) +
        geom_line(aes(y = total_end, color = "End of Year")) +
        scale_x_continuous("year", labels = as.character(yearly_data$year), breaks = yearly_data$year) +  # Show all x labels
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
      
      ggplotly(t) |> 
        layout(xaxis = list(autorange = TRUE), yaxis = list(autorange = TRUE))
    } else {
      variant_plot  # This is the variant plot
    }
  })
  
  ### Received ----
  
  # Load data for storage
  output$storageData <- DT::renderDataTable({
    coll_storage_data
  })
  
  #### infoBox for total waste collected
  output$totalReceivedWaste <- renderInfoBox({
    infoBox(
      "Total Waste Collected",
      paste(total_waste_collected, " tons"),
      icon = icon("trash"),
      color = "purple"
    )
  })
  
  #### infoBox for highest waste producing region
  output$highestWasteProducingRegion <- renderInfoBox({
    infoBox(
      "Highest Waste Producing Region",
      highest_region,
      icon = icon("globe"),
      color = "red"
    )
  })
  
  #### infoBox for waste from producers no record
  output$totalWasteFromProducersNoRecord <- renderInfoBox({
    infoBox(
      "Waste from Producers no Record",
      paste(waste_from_producers_no_record, " tons"),
      icon = icon("truck"),
      color = "green",  # Use a predefined color class for default styling
      fill = FALSE
    )
  })
  
  #### infoBox for waste from producers with record
  output$totalWasteFromProducers <- renderInfoBox({
    infoBox(
      "Waste from Producers with Record",
      paste(waste_from_producers_with_record, " tons"),
      icon = icon("truck"),
      color = "green",  # Use a predefined color class for default styling
      fill = FALSE
    )
  })
  
  #### infoBox for waste from collectors in RS
  output$totalWasteFromCollectors <- renderInfoBox({
    infoBox(
      "Waste from Collectors in RS",
      paste(waste_from_collectors_RS),
      icon = icon("truck"),
      color = "blue",  # Use a predefined color class for default styling
      fill = FALSE
    )
  })
  
  #### infoBox for waste from processors in RS
  output$totalWasteFromProcessors <- renderInfoBox({
    infoBox(
      "Waste from Processors in RS",
      paste(waste_from_processors_RS, " tons"),
      icon = icon("truck"),
      color = "blue",  # Use a predefined color class for default styling
      fill = FALSE
    )
  })
  
  #### plot yearly data received
  output$totalWasteReceivedByYear <- renderPlotly({
    
    # Create a named vector for color mapping using the new names
    color_mapping <- setNames(color_palette, names(source_mapping))
    
    # Line and point
    t <- ggplot(yearly_data_received, aes(x = year)) +
      geom_point(aes(y = total_collected, color = "Total Collected")) +
      geom_line(aes(y = total_collected, color = "Total Collected")) +
      geom_point(aes(y = from_producers_no_record, color = "From Producers (No Record)")) +
      geom_line(aes(y = from_producers_no_record, color = "From Producers (No Record)")) +
      geom_point(aes(y = from_producers_with_record, color = "From Producers (With Record)")) +
      geom_line(aes(y = from_producers_with_record, color = "From Producers (With Record)")) +
      geom_point(aes(y = from_collectors_RS, color = "From Collectors (RS)")) +
      geom_line(aes(y = from_collectors_RS, color = "From Collectors (RS)")) +
      geom_point(aes(y = from_processors_RS, color = "From Processors (RS)")) +
      geom_line(aes(y = from_processors_RS, color = "From Processors (RS)")) +
      scale_color_manual(values = color_mapping) +
      labs(
        y = "Waste Collected",
        color = "Source"
      ) +
      theme_minimal()
    
    ggplotly(t) |> 
      layout(xaxis = list(autorange = TRUE), yaxis = list(autorange = TRUE))
  })
  
  source_mapping <- c(
    "From Producers (No Record)" = "waste_from_producers_no_record",
    "From Producers (With Record)" = "waste_from_producers_with_record",
    "From Collectors (RS)" = "waste_from_collectors_RS",
    "From Processors (RS)" = "waste_from_processors_RS"
  )
  
  # Update the choices for the region filter based on the data
  updateSelectizeInput(session, "region_filter", 
                       choices = na.omit(unique(df_long_received$statistical_region)),
                       selected = na.omit(unique(df_long_received$statistical_region)),
                       options = list(maxItems = length(unique(df_long_received$statistical_region))))
  
  
  filtered_data <- reactive({
    req(input$region_filter, input$source_filter)
    
    # Map the user-friendly names back to the data values
    mapped_sources <- source_mapping[input$source_filter]
    
    data <- df_long_received |> 
      filter(statistical_region %in% input$region_filter,
             source %in% mapped_sources)
    
    return(data)
  })
  
  # Reactive plot selection based on user input and filtered data
  datasetInput <- reactive({
    data <- filtered_data()
    
    if(nrow(data) == 0) {
      return(ggplotly(ggplot() + 
                        annotate("text", x = 1, y = 1, label = "No data available for the selected filters") +
                        theme_void()))
    }
    
    # Create a named vector for color mapping using the new names
    color_mapping <- setNames(color_palette, names(source_mapping))
    
    # Create a factor with levels in the desired order for consistent coloring
    data$source <- factor(data$source, levels = source_mapping, labels = names(source_mapping))
    
    # Summarize the total collected waste by region
    data_summarized <- data |>
      group_by(statistical_region, source) |>
      summarize(total_collected = sum(total_collected, na.rm = TRUE)) |>
      ungroup()
    
    # Create the base plot
    base_plot <- ggplot(data_summarized, aes(x = statistical_region, y = total_collected, fill = source)) +
      labs(x = "Statistical Region", y = "Total Collected Waste") +
      scale_fill_manual(values = color_mapping, name = "Source") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    # Add specific geom based on plot type
    plot <- switch(
      input$plot_type_coll_received,
      "stacked" = base_plot + geom_col(),
      "faceted" = base_plot + geom_bar(stat = "identity", position = "dodge") +
        facet_wrap( ~ source, ncol = 1, scales = "free_y"),
      "grouped" = base_plot + geom_bar(stat = "identity", position = "dodge"),
      base_plot + geom_col()  # Default to stacked plot
    )
    
    ggplotly(plot, tooltip = c("x", "y", "fill")) |>
      layout(
        hovermode = "x",
        xaxis = list(autorange = TRUE),
        yaxis = list(autorange = TRUE)
      )
  })
  
  # Render the selected plot
  output$selectedPlot2 <- renderPlotly({
    datasetInput()  # Call the reactive plot
  })
  
  ### Municipal ----
  
  # Create a reactive expression to hold unique regions
  unique_regions <- unique(df_long_municipal$statistical_region)
  
  # Update the choices for both select inputs
  updateSelectInput(session, "region1", 
                    choices = unique_regions,
                    selected = unique_regions[1])
  
  # Update the choice for the type of waste
  updateSelectInput(session, "waste_type", 
                    choices = unique(df_long_municipal$type_of_waste),
                    selected = unique(df_long_municipal$type_of_waste)[1])
  
  # Update the number of top municipalities
  updateNumericInput(session, "top_n_municipal", 
                     min = 1, 
                     max = unique(df_long_municipal$name_of_municipality), 
                     value = 5)
  
  # Reactive expression to filter data based on selected regions and top n municipalities
  filtered_municipal_data <- reactive({
    top_municipalities <- df_long_municipal |>
      filter(statistical_region %in% c(input$region1, input$region2) &
               type_of_waste == input$waste_type) |>
      group_by(name_of_municipality) |>
      summarize(total_collected = sum(total_collected, na.rm = TRUE), .groups = 'drop') |>
      arrange(desc(total_collected)) |>
      slice_max(order_by = total_collected, n = input$top_n_municipal, with_ties = FALSE)  # Select top n municipalities
    
    df_long_municipal |>
      filter(name_of_municipality %in% top_municipalities$name_of_municipality &
               statistical_region %in% c(input$region1, input$region2) &
               type_of_waste == input$waste_type)
  })
  
  # Calculate the municipality that collected the most waste
  max_waste_municipality <- reactive({
    filtered_municipal_data() |>
      group_by(name_of_municipality) |>
      summarize(total_collected = sum(total_collected, na.rm = TRUE)) |>
      arrange(desc(total_collected)) |>
      slice(1) |>
      pull(name_of_municipality)
  })
  
  # Compute the municipality that collected the most waste
  output$maxWasteMunicipality <- renderInfoBox({
    max_municipality <- filtered_municipal_data() |>
      group_by(name_of_municipality) |>
      summarize(total_waste = sum(total_collected)) |>
      arrange(desc(total_waste)) |>
      slice(1)  # Select the municipality with the highest waste collected
    
    infoBox(
      width = 12,
      title = "Top Municipality",   # Add title here
      value = max_municipality$name_of_municipality,
      subtitle = paste("Collected:", max_municipality$total_waste, "tons"),
      icon = icon("recycle"),  # Add icon if desired
      color = "green"
    )
  })
  
  # Render the comparison plot for top n municipalities
  output$municipalComparison <- renderPlotly({
    # Create a filtered dataset for plotting
    filtered_data <- filtered_municipal_data()
    
    # Create the plot using Plotly
    p <- plot_ly(data = filtered_data,
                 x = ~year,
                 y = ~total_collected,
                 color = ~name_of_municipality,
                 type = 'scatter',
                 mode = 'lines+markers',
                 colors = 'Set3') |> 
      layout(title = "Waste Collected by Municipality",
             xaxis = list(title = "Year", 
                          tickvals = unique(filtered_data$year)),
             yaxis = list(title = "Total Waste Collected (in tons)"),
             showlegend = TRUE)
    
    return(p)
  })
  
  
  ### Municipal Collected ----
  
  # Update the choices for both select inputs
  updateSelectInput(session, "selected_year", 
                    choices = unique(df_long_collected$year),
                    selected = unique(df_long_collected$year)[1])
  
  output$plot1 <- renderPlotly({
    plot_total_waste_by_year(df_long_collected)
  })
  
  output$plot2 <- renderPlotly({
    req(input$selected_year, input$top_n)  
    plot_waste_by_municipality(df_long_collected, input$selected_year, input$top_n)
  })
  
  output$plot3 <- renderPlotly({
    plot_waste_types_by_year(df_long_collected)
  })
  
  output$plot4 <- renderPlotly({
    plot_heatmap_by_region(df_long_collected)
  })
  
  ### Management ----
  
  # Reactive data
  filtered_management_data <- reactive({
    df_long_management |>
      filter(
        year >= input$yearRange[1] & year <= input$yearRange[2],
        statistical_region %in% input$region,
        type_of_waste %in% input$wasteType
      )
  })
  
  # Update input choices
  observe({
    updateSelectInput(session,
                      "region",
                      choices = unique(df_long_management$statistical_region))
    updateSelectInput(session,
                      "wasteType",
                      choices = unique(df_long_management$type_of_waste))
    available_years <- df_long_management$year
    updateSliderInput(
      session,
      "yearRange",
      min = min(available_years, na.rm = TRUE),
      max = max(available_years, na.rm = TRUE),
      value = c(
        min(available_years, na.rm = TRUE),
        max(available_years, na.rm = TRUE)
      )
    )
  })
  
  # Total Waste Trend
  output$totalWasteTrend <- renderPlotly({
    df_long_management |>
      group_by(year) |>
      summarise(total_waste = sum(total_waste_given_away, na.rm = TRUE)) |>
      plot_ly(x = ~year, y = ~total_waste, type = 'scatter', mode = 'lines+markers') |>
      layout(xaxis = list(title = "Year"),
             yaxis = list(title = "Total Waste"))
  })
  
  # Waste by Region
  output$wasteByRegion <- renderPlotly({
    df_long_management |>
      group_by(statistical_region) |>
      summarise(total_waste = sum(total_waste_given_away, na.rm = TRUE)) |>
      plot_ly(x = ~reorder(statistical_region, -total_waste), y = ~total_waste, type = 'bar') |>
      layout(xaxis = list(title = "Region"),
             yaxis = list(title = "Total Waste"))
  })
  
  # Waste by Type
  output$wasteByType <- renderPlotly({
    df_long_management |>
      group_by(type_of_waste) |>
      summarise(total_waste = sum(total_waste_given_away, na.rm = TRUE)) |>
      plot_ly(x = ~reorder(type_of_waste, -total_waste), y = ~total_waste, type = 'bar') |>
      layout(xaxis = list(title = "Waste Type"),
             yaxis = list(title = "Total Waste"))
  })
  
  # Detailed Plot
  output$detailedPlot <- renderPlotly({
    req(input$region, input$wasteType)
    
    filtered_management_data() |>
      plot_ly(x = ~year, y = ~total_waste_given_away, color = ~source, type = 'scatter', mode = 'lines+markers') |>
      layout(title = paste("Detailed Waste Analysis for", input$region, "-", input$wasteType),
             xaxis = list(title = "Year"),
             yaxis = list(title = "Total Waste"),
             legend = list(title = "Waste Source"))
  })
  
  ## Treatment ----
  
  ### Storage ----
  
  # Observe waste type input and update region based on the selected waste type
  observeEvent(input$waste_type_trt_storage, {
    data_filtered <- trt_storage_data
    
    # Filter data based on selected waste type
    if (!is.null(input$waste_type_trt_storage) && length(input$waste_type_trt_storage) > 0) {
      data_filtered <- data_filtered[data_filtered$type_of_waste %in% input$waste_type_trt_storage, ]
    }
    
    # Update region selectInput based on filtered data, using isolate to avoid triggering region observer
    isolate({
      updateSelectInput(
        session,
        "region_trt_storage",
        choices = unique(data_filtered$statistical_region),
        selected = input$region_trt_storage  # Keep the current selection if valid
      )
    })
  })
  
  # Observe region input and update waste type based on the selected region
  observeEvent(input$region_trt_storage, {
    data_filtered <- trt_storage_data
    
    # Filter data based on selected region
    if (!is.null(input$region_trt_storage) &&
        length(input$region_trt_storage) > 0) {
      data_filtered <- data_filtered[data_filtered$statistical_region %in% input$region_trt_storage, ]
    }
    
    # Update waste type selectInput based on filtered data, using isolate to avoid triggering waste type observer
    isolate({
      updateSelectInput(
        session,
        "waste_type_trt_storage",
        choices = unique(data_filtered$type_of_waste),
        selected = input$waste_type_trt_storage  # Keep the current selection if valid
      )
    })
  })
  
  # Update input choices
  observe({
    data <- trt_storage_data
    updateSelectizeInput(
      session,
      "waste_type_trt_storage",
      choices = unique(data$type_of_waste)
    )
    updateSelectizeInput(
      session,
      "region_trt_storage",
      choices = unique(data$statistical_region)
    )
  })
  
  # Filter data based on inputs
  filtered_storage_data <- reactive({
    req(input$year_range_trt_storage, input$waste_type_trt_storage, input$region_trt_storage)
    data <- trt_storage_data
    
    filtered <- data |> 
      filter(
        year >= input$year_range_trt_storage[1] & year <= input$year_range_trt_storage[2],  # Filter by year range
        type_of_waste %in% input$waste_type_trt_storage,
        statistical_region %in% input$region_trt_storage
      )
    
    filtered
  })
  
  # Process data for plotting
  processed_data <- reactive({
    req(nrow(filtered_storage_data()) > 0)
    complete_data <- filtered_storage_data() |>
      complete(
        year,
        nesting(statistical_region, type_of_waste),
        fill = list(
          waste_stored_start_year = 0,
          waste_stored_end_year = 0
        )
      ) |>
      arrange(statistical_region, type_of_waste, year) |>
      group_by(statistical_region, type_of_waste) |>
      mutate(previous_end_year = lag(waste_stored_end_year, 1)) |>
      ungroup() |>
      mutate(
        outside_period_next_start = ifelse(is.na(lead(waste_stored_start_year)), TRUE, FALSE),
        outside_period_prev_end = ifelse(is.na(previous_end_year), TRUE, FALSE),
        next_year_start = lead(waste_stored_start_year),
        previous_end_year = ifelse(is.na(previous_end_year), 0, previous_end_year),
        difference = waste_stored_start_year - previous_end_year
      )
    
    threshold <- 0.1
    complete_data <- complete_data |>
      mutate(
        significant_change = case_when(
          difference > (previous_end_year * threshold) ~ "Increase",
          difference < -(previous_end_year * threshold) ~ "Decrease",
          TRUE ~ "No Significant Change"
        )
      )
    
    complete_data
  })
  
  output$waterfall_plot <- renderPlotly({
    req(nrow(processed_data()) > 0) 
    data <- processed_data()
    
    variant_data <- data |>
      arrange(year) |>
      mutate(
        end_year = paste0(as.numeric(year), " End"),
        start_next_year = paste0(as.numeric(year) + 1, " Start"),
        end_amount = waste_stored_end_year,
        start_amount = lead(waste_stored_start_year),
        difference = lead(waste_stored_start_year) - waste_stored_end_year
      ) |>
      select(end_year, start_next_year, end_amount, start_amount, difference) |>
      pivot_longer(
        cols = c(end_year, start_next_year),
        names_to = "type",
        values_to = "year"
      ) |>
      mutate(
        amount = ifelse(type == "end_year", end_amount, start_amount),
        difference = ifelse(type == "start_next_year", difference, 0),
        cumulative = cumsum(amount),
        color_category = case_when(
          type == "end_year" ~ "End Year",
          difference > 0 ~ "Increase",
          difference < 0 ~ "Decrease",
          TRUE ~ "No Change"
        )
      ) |>
      filter(!is.na(start_amount))
    
    variant_data$year <- with(variant_data, 
                              paste(year, ifelse(type == "end_year", "", ""), sep = " "))
    variant_data$year <- factor(variant_data$year, 
                                levels = unique(variant_data$year))
    
    variant_data_long <- variant_data |>
      pivot_longer(
        cols = c(amount, difference),
        names_to = "bar_type",
        values_to = "value"
      ) |>
      mutate(
        bar_category = case_when(
          bar_type == "amount" & type == "end_year" ~ "End Year",
          bar_type == "amount" & type != "end_year" ~ "Start Amount",
          bar_type == "difference" & color_category == "Increase" ~ "Increase",
          bar_type == "difference" & color_category == "Decrease" ~ "Decrease",
          TRUE ~ "No Change"
        )
      )
    
    desired_order <- c("Start Amount", "Increase", "End Year", "Decrease", "No Change")
    variant_data_long <- variant_data_long |> 
      mutate(bar_category = factor(bar_category, levels = desired_order)) |> 
      arrange(bar_category)
    
    p <- ggplot(variant_data_long, aes(x = year, y = value, fill = bar_category)) +
      geom_col(position = position_identity(), color = "black") +
      geom_text(aes(label = ifelse(value > 0, round(value, 1), ifelse(value == 0, NA, round(value, 1))),
                    y = ifelse(value >= 0, value, value) + 0.05 * max(value)),
                position = position_dodge(width = 0.9),
                vjust = -0.5, size = 3) +
      scale_fill_manual(values = c("End Year" = "#4169E1", 
                                   "Increase" = "#006400",
                                   "Start Amount" = "#808080",
                                   "Decrease" = "#8B0000",
                                   "No Change" = "#D3D3D3"),
                        name = "Type") +
      labs(x = "Year", y = "Waste Amount (tons)") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p, tooltip = c("x", "y", "fill")) |> 
      layout(xaxis = list(autorange = TRUE), yaxis = list(autorange = TRUE))
  }) 
  
  ### Collected ----
  
  # Summarize total waste received by year
  yearly_total <- trt_collected_data |>
    group_by(year) |>
    summarize(total_waste = sum(total_waste_received, na.rm = TRUE))
  
  # Create the plotly line chart for total waste by year
  output$plot_yearly <- renderPlotly({
    plot_ly(yearly_total, x = ~year, y = ~total_waste, type = 'scatter', mode = 'lines+markers') |>
      layout(
        # title = "Total Waste Received Over Years",
        xaxis = list(title = "Year"),
        yaxis = list(title = "Total Waste Received")
      )
  })
  
  # Summarize total waste received by statistical region
  region_total <- trt_collected_data |>
    group_by(statistical_region) |>
    summarize(total_waste = sum(total_waste_received, na.rm = TRUE)) |>
    arrange(desc(total_waste))
  
  # Create the plotly bar chart for total waste by region
  output$plot_region <- renderPlotly({
    plot_ly(
      region_total,
      x = ~reorder(statistical_region, -total_waste),
      y = ~ total_waste,
      type = 'bar'
    ) |>
      layout(
        # title = "Total Waste Received by Statistical Region",
        xaxis = list(title = "Statistical Region"),
        yaxis = list(title = "Total Waste Received")
      )
  })
  
  # Summarize total waste received by type of waste
  waste_type_total <- trt_collected_data |>
    group_by(type_of_waste) |>
    summarize(total_waste = sum(total_waste_received, na.rm = TRUE)) |>
    arrange(desc(total_waste))
  
  # Create the plotly bar chart for total waste by type of waste
  output$plot_waste_type <- renderPlotly({
    plot_ly(
      waste_type_total,
      x = ~reorder(type_of_waste, -total_waste),
      y = ~ total_waste,
      type = 'bar'
    ) |>
      layout(
        # title = "Total Waste Received by Type of Waste",
        xaxis = list(title = "Type of Waste"),
        yaxis = list(title = "Total Waste Received")
      )
  })
  
  ### Treatment ----
  
  # Update the choices for waste types and regions based on available data
  observe({
    data <- trt_treatment_data
    updateSelectizeInput(
      session,
      "waste_type_trt_treatment",
      choices = unique(data$type_of_waste),
      selected = unique(data$type_of_waste),
      options = list(plugins = list('remove_button'))
    )
    updateSelectizeInput(
      session,
      "region_trt_treatment",
      choices = unique(data$statistical_region),
      selected = unique(data$statistical_region),
      options = list(plugins = list('remove_button'))
    )
  })
  
  # Filter the data based on user inputs
  filtered_treatment_data <- reactive({
    data <- trt_treatment_data |>
      filter(
        year >= input$year_range_trt_treatment[1] &
          year <= input$year_range_trt_treatment[2],
        # Filter by year range
        type_of_waste %in% input$waste_type_trt_treatment,
        # Filter by selected waste types
        statistical_region %in% input$region_trt_treatment,
        # Filter by selected regions
        statistical_region != "NEOPREDELJENO"  # Exclude the "NEOPREDELJENO" region
      )
    
    # Summarize the data by year and waste type
    aggregated_data <- data |>
      group_by(year, type_of_waste) |>
      summarize(
        total_waste = sum(waste_entering_treatment_process, na.rm = TRUE),
        .groups = 'drop'
      )
    
    return(aggregated_data)
  })
  
  # Create the Plotly plot for waste treatment over time by waste type
  output$plot_waste_treatment <- renderPlotly({
    data <- filtered_treatment_data()
    
    # Create a Plotly plot
    p <- plot_ly(data, x = ~year, y = ~total_waste, color = ~type_of_waste, type = 'scatter', mode = 'lines+markers') %>%
      layout(
        title = "Waste Treatment Over Time by Waste Type (in tons)",
        xaxis = list(title = "Year", autorange = TRUE),
        yaxis = list(title = "Total Waste Entering Treatment Process", autorange = TRUE),
        legend = list(position = "bottom")
      )
    
    return(p)
  })
  
  ### Municipal Waste Received ----
  
  slovenia_map <- sf::st_read("../../map.geojson")
  
  # Prepare map data
  waste_by_municipality <- trt_municipal_waste_received_data |>
    group_by(statistical_region, name_of_municipality) |>
    summarise(avg_waste = mean(waste_collected_by_municipality, na.rm = TRUE)) |>
    ungroup() |>
    mutate(name_of_municipality = str_replace_all(name_of_municipality, " ", ""))
  
  slovenia_map <- slovenia_map |>
    mutate(NAME_1 = str_to_upper(NAME_1),
           NAME_2 = str_to_upper(NAME_2))
  
  slovenia_map_with_data <- slovenia_map |>
    left_join(waste_by_municipality, by = c("NAME_2" = "name_of_municipality")) |>
    mutate(statistical_region = ifelse(is.na(statistical_region), NAME_1, statistical_region))
  
  # Render waste map
  output$wasteMap <- renderPlotly({
    static_map <- ggplot(slovenia_map_with_data) +
      geom_sf(aes(fill = avg_waste, text = paste(
        "Municipality:", NAME_2, 
        "<br>",
        ifelse(is.na(avg_waste), 
               "No data available", 
               paste("Average Waste:", round(avg_waste, 2))
        )
      ))) +         
      viridis::scale_fill_viridis(
        option = "plasma", 
        name = "Average Waste\n(2018-2021)", 
        labels = scales::comma,
        na.value = "grey80"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12),
        legend.position = "right",
        axis.text = element_blank(),
        axis.ticks = element_blank()
      )
    
    ggplotly(static_map, tooltip = "text") |>
      layout(
        hoverlabel = list(bgcolor = "white", font = list(family = "Arial", size = 12)),
        xaxis = list(autorange = TRUE), 
        yaxis = list(autorange = TRUE)
      ) 
  })
  
  # Observe and update the top_n_trt_municipal slider dynamically based on the selected year range
  observe({
    filtered_data <- trt_municipal_waste_received_data |>
      filter(year >= input$year_range_trt_municipal[1] & year <= input$year_range_trt_municipal[2])
    
    # Summarize total waste by municipality for the selected year range
    total_waste_by_municipality <- filtered_data |>
      group_by(year, name_of_municipality) |>
      summarise(total_waste = sum(waste_collected_by_municipality, na.rm = TRUE)) |>
      ungroup()
    
    # Get the top municipalities by total waste for the selected year range
    top_municipalities <- total_waste_by_municipality |>
      group_by(name_of_municipality) |>
      summarise(total_waste = sum(total_waste)) |>
      arrange(desc(total_waste))
    
    # Update top_n_trt_municipal slider with the maximum number of municipalities in the filtered range
    num_municipalities <- nrow(top_municipalities)
    updateSliderInput(session, "top_n_trt_municipal", max = num_municipalities)
  })
  
  # Render trend plot
  output$trendPlot <- renderPlotly({
    # Filter data based on selected year range
    filtered_data <- trt_municipal_waste_received_data |>
      filter(year >= input$year_range_trt_municipal[1] & year <= input$year_range_trt_municipal[2])
    
    # Summarize total waste by municipality and year
    total_waste_by_municipality <- filtered_data |>
      group_by(year, name_of_municipality) |>
      summarise(total_waste = sum(waste_collected_by_municipality, na.rm = TRUE)) |>
      ungroup()
    
    # Get the top N municipalities for the selected range
    top_municipalities <- total_waste_by_municipality |>
      group_by(name_of_municipality) |>
      summarise(total_waste = sum(total_waste)) |>
      arrange(desc(total_waste)) |>
      head(input$top_n_trt_municipal) |>
      pull(name_of_municipality)
    
    # Filter the dataset for the top N municipalities
    filtered_data <- total_waste_by_municipality |>
      filter(name_of_municipality %in% top_municipalities)
    
    # Create plot
    trend_plot <- ggplot(filtered_data,
                         aes(x = year, y = total_waste, color = name_of_municipality)) +
      geom_point() +
      geom_line() +
      theme_minimal() +
      labs(x = "Year", y = "Waste Collected")
    
    ggplotly(trend_plot) |>
      layout(xaxis = list(autorange = TRUE),
             yaxis = list(autorange = TRUE))
  })
  
  # Render region comparison plot
  output$regionPlot <- renderPlotly({
    region_comparison <- trt_municipal_waste_received_data |>
      group_by(statistical_region) |>
      summarise(total_waste = sum(waste_collected_by_municipality, na.rm = TRUE)) |>
      ggplot(aes(
        x = reorder(statistical_region, -total_waste),
        y = total_waste,
        fill = statistical_region
      )) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(x = "Statistical Region", y = "Waste Collected") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(region_comparison) |>
      layout(xaxis = list(autorange = TRUE),
             yaxis = list(autorange = TRUE))
  })
  
  ### Input Treatment ----
  
  # Update filters
  observe({
    updateSelectizeInput(
      session,
      "year_filter",
      choices = sort(unique(trt_input_treatment_data$year)),
      selected = unique(trt_input_treatment_data$year),
      options = list(plugins = list('remove_button'))
    )
    updateSelectizeInput(
      session,
      "waste_type_filter",
      choices = sort(unique(
        trt_input_treatment_data$type_of_waste
      )),
      selected = unique(trt_input_treatment_data$type_of_waste),
      options = list(plugins = list('remove_button'))
    )
  })
  
  # Filter data based on user input
  filtered_input_data <- reactive({
    trt_input_treatment_data |>
      filter(year %in% input$year_filter, type_of_waste %in% input$waste_type_filter)
  })
  
  # Mass Change by Year
  output$inputTreatmentByYear <- renderPlotly({
    plot_ly(filtered_input_data(), x = ~year, y = ~mass_change, type = 'bar', color = ~type_of_waste) |>
      layout(#title = "Mass Change During Treatment Over the Years by Waste Type",
             xaxis = list(title = "Year"),
             yaxis = list(title = "Total Mass Change"))
  })
  
  # Mass Change by Operation - NOT USED
  output$inputTreatmentByOperation <- renderPlotly({
    plot_ly(filtered_input_data(), x = ~year, y = ~mass_change, color = ~treatment_operation, type = 'bar') |>
      layout(title = "Mass Change During Treatment by Operation",
             xaxis = list(title = "Year"),
             yaxis = list(title = "Total Mass Change"))
  })
  
  # Simulation tab -----
  
  # Selected choices for filtering tables -----
  selected_choices <- c(
    "Total waste generated (kg)",
    "Total waste collected (kg)",
    "Total waste handled (kg)",
    "Total waste stored (kg)",
    "Total waste recycled (kg)",
    "Overflow penalty",
    "Total storage cost",
    "Total collection cost",
    "Total processing cost",
    "Total transportation cost",
    "Transportation cost after balancing",
    "Total recycling revenue",
    "Total avoided disposal cost"
  )
  
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
  
  seasons <- list(
    winter = 0.8,
    spring = 1.0,
    summer = 1.2,
    fall = 1.0
  )
  
  default_values <- list(
    generation_rate_increase = 1.0,
    collection_rate_increase = 1.0,
    handling_rate_increase = 1.0,
    recycling_rate_increase = 1.0,
    storage_cost_per_unit = 0.5,
    collection_cost_per_unit = 1.0,
    processing_cost_per_unit = 2.0,
    transportation_cost_per_unit = 0.3,
    overflow_penalty_per_unit = 2.0,
    recycling_revenue_per_unit = 2.0,
    avoided_disposal_cost_per_unit = 1.0,
    storage_capacity = 500,
    runtime = 50
  )
  
  # Define intelligence parameters
  intelligence_params <- list(
    # Collect waste if more than 80% of storage capacity is reached
    collection_threshold = 0.8,
    # Adjust collection frequency based on waste accumulation
    adaptive_collection_rate = FALSE,
    # Maximum amount to collect in one go
    max_collection_amount = 25
  )
  
  # Load scenario parameters -----
  
  load_scenario_parameters <- function(scenario, user_params) {
    params <- user_params
    changes <- list()
    
    if (scenario == "increased_waste_generation") {
      params$generation_rate_increase <- 2
      changes <- c(changes, "Generation rate increase set to 2")
    } else if (scenario == "improved_collection_efficiency") {
      params$collection_rate_increase <- 1.5
      changes <- c(changes, "Collection rate increase set to 1.5")
    } else if (scenario == "improved_handling_efficiency") {
      params$handling_rate_increase <- 1.5
      changes <- c(changes, "Handling rate increase set to 1.5")
    } else if (scenario == "enhanced_recycling_programs") {
      params$recycling_rate_increase <- 2
      params$recycling_revenue_per_unit <- 5
      params$avoided_disposal_cost_per_unit <- 2
      changes <- c(
        changes,
        "Recycling rate increase set to 2",
        "Recycling revenue per unit set to 5",
        "Avoided disposal cost per unit set to 2"
      )
    } else if (scenario == "storage_capacity_expansion") {
      params$storage_capacity <- 1000
      changes <- c(changes, "Storage capacity set to 1000")
    } else if (scenario == "penalties_for_overflow") {
      params$overflow_penalty_per_unit <- 5
      changes <- c(changes, "Overflow penalty per unit set to 5")
    } else if (scenario == "different_transportation_costs") {
      params$transportation_cost_per_unit <- 0.5
      changes <- c(changes, "Transportation cost per unit set to 0.5")
    } else if (scenario == "seasonal_variations") {
      if (isTruthy(input$season) && input$season == "Year-long") {
        params$seasonal_variations <- TRUE
        params$seasonal_multiplier <- 1.1  # Assuming a default multiplier of 1 for year-long
        changes <- c(
          changes,
          paste(
            "Seasonal variations set to",
            params$seasonal_multipler,
            "for",
            input$season
          )
        )
      } else if (isTruthy(input$season)) {
        params$seasonal_variations <- FALSE
        params$seasonal_multiplier <- seasons[[tolower(input$season)]]
        changes <- c(
          changes,
          paste0(
            "Seasonal multiplier set to",
            params$seasonal_multiplier,
            "for",
            input$season
          )
        )
      } else {
        params$seasonal_variations <- FALSE
        params$seasonal_multiplier <- 1
        changes <- c(changes, "Seasonal variations disabled")
      }
    }
    
    return(list(params = params, changes = changes))
  }
  
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
  
  ## Event and cost data
  
  results <- reactiveVal(
    data.frame(
      Time = numeric(),
      Region = character(),
      WasteType = character(),
      Event = character(),
      Amount = numeric(),
      Scenario = character(),
      stringsAsFactors = FALSE
    )
  )
  
  costs <- reactiveVal(
    data.frame(
      Time = numeric(),
      Region = character(),
      WasteType = character(),
      CollectionCost = numeric(),
      ProcessingCost = numeric(),
      TransportationCost = numeric(),
      RecyclingRevenue = numeric(),
      Scenario = character(),
      stringsAsFactors = FALSE
    )
  )
  
  # Logging events and costs -----
  log_event <- function(env,
                        region,
                        waste_type,
                        event,
                        scenario = input$scenario) {
    time <- now(env)
    amount <- get_attribute(env, "waste_amount")
    
    # Event data
    event_data <- data.frame(
      Time = time,
      Region = region,
      WasteType = waste_type,
      Event = event,
      Amount = amount,
      Scenario = scenario,
      stringsAsFactors = FALSE
    )
    
    # Cost data
    cost_data <- data.frame(
      Time = time,
      Region = region,
      WasteType = waste_type,
      CollectionCost = get_global(
        env,
        paste0("total_collection_cost_", region, "_", waste_type)
      ),
      ProcessingCost = get_global(
        env,
        paste0("total_processing_cost_", region, "_", waste_type)
      ),
      TransportationCost = get_global(
        env,
        paste0("total_transportation_cost_", region, "_", waste_type)
      ),
      RecyclingRevenue = get_global(
        env,
        paste0("total_recycling_revenue_", region, "_", waste_type)
      ),
      Scenario = scenario,
      stringsAsFactors = FALSE
    )
    
    # Append data to results and costs
    results(rbind(results(), event_data))
    costs(rbind(costs(), cost_data))
  }
  
  
  # Functions -----
  
  # Function to handle waste amount calculation
  calculate_waste_amount <- function(env,
                                     region,
                                     params,
                                     waste_type,
                                     min_val = 10,
                                     max_val = 30) {
    base_amount <- runif(1, min_val, max_val)
    
    multiplier <- if (!is.null(params$seasonal_variations) &&
                      params$seasonal_variations) {
      seasons[[sample(names(seasons), 1)]]
    } else if (!is.null(params$seasonal_multiplier)) {
      params$seasonal_multiplier
    } else {
      1
    }
    
    # Add time multiplier
    
    current_time <- now(env)
    
    time_multiplier <- if (current_time > 50) {
      1.5
    } else {
      1
    }
    
    base_amount * multiplier * time_multiplier
  }
  
  # Function to create trajectories for waste generation,
  # collection, handling, and recycling
  create_trajectories <- function(env, params, region, waste_type) {
    # Generate waste -----
    generate_waste <- trajectory(paste("Generate Waste -", region, "-", waste_type)) |>
      # seize("storage", 1) |>
      seize(paste0("storage_", region, "_", waste_type), 1) |>
      set_attribute("waste_amount", function()
        calculate_waste_amount(env, region, params, waste_type)) |>
      set_global(paste0("total_waste_generated_", region, "_", waste_type), function() {
        waste_generated <- get_attribute(env, "waste_amount")
        get_global(env,
                   paste0("total_waste_generated_", region, "_", waste_type)) + waste_generated
      }) |>
      set_global(paste0("waste_to_collect_", region, "_", waste_type), function() {
        # Realistically we are not able to collect all the waste generated
        # but collection trajectory decides how much to collect
        get_attribute(env, "waste_amount")
      }) |>
      set_attribute("log_event", function()
        log_event(env, region, waste_type, "Generation")) |>
      timeout(function()
        rexp(1, rate = 0.5)) |>
      # release("storage", 1) |>
      release(paste0("storage_", region, "_", waste_type))
    
    # Collect waste with intelligence -----
    collect_waste <- trajectory(paste("Collect Waste -", region, "-", waste_type)) |>
      timeout(function() {
        # Adjust collection frequency based on waste accumulation
        if (intelligence_params$adaptive_collection_rate) {
          waste_to_collect <- ifelse(is.na(get_global(
            env,
            paste0("waste_to_collect_", region, "_", waste_type)
          )), 0, get_global(
            env,
            paste0("waste_to_collect_", region, "_", waste_type)
          ))
          storage_capacity <- params$storage_capacity
          if (!is.na(waste_to_collect) &&
              waste_to_collect > storage_capacity * intelligence_params$collection_threshold) {
            # Increase collection frequency if waste exceeds threshold
            return(rexp(1, rate = 2)) # More frequent collection
          } else {
            return(rexp(1, rate = 1)) # Normal collection frequency
          }
        } else {
          return(10)
        }
      }) |>
      seize("collection_truck") |>
      seize(paste0("storage_", region, "_", waste_type), 1) |>
      set_attribute("waste_amount", function() {
        waste_to_collect <- ifelse(is.na(get_global(
          env,
          paste0("waste_to_collect_", region, "_", waste_type)
        )), 0, get_global(
          env,
          paste0("waste_to_collect_", region, "_", waste_type)
        ))
        if (!is.na(waste_to_collect) && waste_to_collect > 0) {
          amount <- runif(
            1,
            min(
              intelligence_params$max_collection_amount,
              waste_to_collect
            ) * 0.8,
            min(
              intelligence_params$max_collection_amount,
              waste_to_collect
            )
          )
          if (is.na(amount)) {
            amount <- 0
          }
          amount
        } else {
          0
        }
      }) |>
      set_global(paste0("total_waste_collected_", region, "_", waste_type), function() {
        get_global(env,
                   paste0("total_waste_collected_", region, "_", waste_type)) + get_attribute(env, "waste_amount")
      }) |>
      set_global(paste0("total_waste_stored_", region, "_", waste_type), function() {
        current_storage <- get_global(env,
                                      paste0("total_waste_stored_", region, "_", waste_type))
        new_storage <- get_attribute(env, "waste_amount") + current_storage
        # print(paste("Current storage: ", (min(current_storage + new_waste, params$storage_capacity))))
        print("------")
        print(paste("Amount stored: ", new_storage))
        print(paste("Storage capacity: ", params$storage_capacity))
        min(new_storage, params$storage_capacity)
        
        # waste_amount <- get_attribute(env, "waste_amount")
        # result <- handle_overflow(env, region, waste_type, waste_amount)
        # result$new_storage
      }) |>
      set_global(paste0("total_collection_cost_", region, "_", waste_type), function() {
        waste_amount <- get_attribute(env, "waste_amount")
        current_collection_cost <- waste_amount * params$collection_cost_per_unit
        get_global(env,
                   paste0("total_collection_cost_", region, "_", waste_type)) + current_collection_cost
      }) |>
      set_global(paste0("overflow_penalty_", region, "_", waste_type), function() {
        waste_amount <- get_attribute(env, "waste_amount")
        result <- handle_overflow(env, region, waste_type, waste_amount)
        print(paste("Storage after handle_overflow: ", result$new_storage))
        get_global(env,
                   paste0("overflow_penalty_", region, "_", waste_type)) + result$overflow_penalty
      }) |>
      set_global(paste0("total_storage_cost_", region, "_", waste_type), function() {
        # Get the current amount of waste
        waste_amount <- get_attribute(env, "waste_amount")
        
        # Calculate the current cost
        current_storage_cost <- waste_amount * params$storage_cost_per_unit
        
        # Update the global total cost
        get_global(env,
                   paste0("total_storage_cost_", region, "_", waste_type)) + current_storage_cost
      }) |>
      set_global(paste0("total_transportation_cost_", region, "_", waste_type), function() {
        waste_amount <- get_attribute(env, "waste_amount")
        current_transportation_cost <- waste_amount * params$transportation_cost_per_unit
        get_global(env,
                   paste0("total_transportation_cost_", region, "_", waste_type)) + current_transportation_cost
      }) |>
      set_global(paste0("waste_to_handle_", region, "_", waste_type), function() {
        get_attribute(env, "waste_amount")
      }) |>
      # Ensure check_waste_consistency runs during the simulation
      #set_attribute("check_consistency", function() {
      #  check_waste_consistency(env, region, waste_type)
      #  1.0  # Return DOUBLE as set_attribute requires a return value
      #}) |>
      set_attribute("log_event", function()
        log_event(env, region, waste_type, "Collection")) |>
      timeout(function()
        rexp(1, rate = 1.5)) |>
      # release("storage", 1) |>
      release(paste0("storage_", region, "_", waste_type)) |>
      release("collection_truck")
    
    # Handle waste -----
    handle_waste <- trajectory(paste("Handle Waste -", region, "-", waste_type)) |>
      timeout(15) |>
      seize("collection_truck") |>
      seize("waste_processor") |>
      set_attribute("waste_amount", function() {
        waste_to_handle <- get_global(env, paste0("waste_to_handle_", region, "_", waste_type))
        if (!is.na(waste_to_handle) && waste_to_handle > 0) {
          amount <- runif(1, waste_to_handle * 0.8, waste_to_handle)
          if (is.na(amount)) {
            amount <- 0
          }
          amount
        } else {
          0
        }
      }) |>
      set_global(paste0("total_waste_handled_", region, "_", waste_type), function() {
        get_global(env,
                   paste0("total_waste_handled_", region, "_", waste_type)) + get_attribute(env, "waste_amount")
      }) |>
      set_global(paste0("total_processing_cost_", region, "_", waste_type), function() {
        waste_amount <- get_attribute(env, "waste_amount")
        current_processing_cost <- waste_amount * params$processing_cost_per_unit
        get_global(env,
                   paste0("total_processing_cost_", region, "_", waste_type)) + current_processing_cost
      })  |>
      set_global(paste0("waste_to_recycle_", region, "_", waste_type), function() {
        get_attribute(env, "waste_amount")
      }) |>
      set_attribute("log_event", function()
        log_event(env, region, waste_type, "Handling")) |>
      timeout(function()
        rexp(1, rate = 1)) |>
      release("waste_processor") |>
      release("collection_truck")
    
    # Recycle waste -----
    recycle_waste <- trajectory(paste("Recycle Waste -", region, "-", waste_type)) |>
      timeout(20) |>
      seize("recycling_facility") |>
      set_attribute("waste_amount", function() {
        waste_to_recycle <- get_global(env,
                                       paste0("waste_to_recycle_", region, "_", waste_type))
        if (!is.na(waste_to_recycle) && waste_to_recycle > 0) {
          amount <- runif(1, waste_to_recycle * 0.8, waste_to_recycle)
          if (is.na(amount)) {
            amount <- 0
          }
          amount
        } else {
          0
        }
      }) |>
      set_global(paste0("total_waste_recycled_", region, "_", waste_type), function() {
        recycled_amount <- get_attribute(env, "waste_amount")
        get_global(env,
                   paste0("total_waste_recycled_", region, "_", waste_type)) + recycled_amount
      }) |>
      set_global(paste0("total_recycling_revenue_", region, "_", waste_type), function() {
        recycled_amount <- get_attribute(env, "waste_amount")
        current_revenue <- recycled_amount * params$recycling_revenue_per_unit
        get_global(env,
                   paste0("total_recycling_revenue_", region, "_", waste_type)) + current_revenue
      }) |>
      set_global(paste0("total_avoided_disposal_cost_", region, "_", waste_type), function() {
        recycled_amount <- get_attribute(env, "waste_amount")
        current_avoided_disposal_cost <- recycled_amount * params$avoided_disposal_cost_per_unit
        get_global(env,
                   paste0("total_avoided_disposal_cost_", region, "_", waste_type)) + current_avoided_disposal_cost
      }) |>
      set_attribute("log_event", function()
        log_event(env, region, waste_type, "Recycling")) |>
      timeout(function()
        rexp(1, rate = 0.5)) |>
      release("recycling_facility")
    
    list(
      generate_waste = generate_waste,
      collect_waste = collect_waste,
      handle_waste = handle_waste,
      recycle_waste = recycle_waste
    )
  }
  
  # Check waste consistency
  check_waste_consistency <- function(env, region, waste_type) {
    total_generated <- get_global(env,
                                  paste0("total_waste_generated_", region, "_", waste_type))
    total_collected <- get_global(env,
                                  paste0("total_waste_collected_", region, "_", waste_type))
    remaining_to_collect <- get_global(env, paste0("waste_to_collect_", region, "_", waste_type))
    
    expected_collected <- total_generated - remaining_to_collect
    
    if (abs(total_collected - expected_collected) > 1e-6) {
      # Allow for small floating-point discrepancies
      print("In IF")
      warning(paste(
        "Inconsistency detected in",
        region,
        waste_type,
        "waste collection:"
      ))
      warning(paste("Total generated:", total_generated))
      warning(paste("Total collected:", total_collected))
      warning(paste("Remaining to collect:", remaining_to_collect))
      warning(paste("Expected collected:", expected_collected))
    } else {
      print("------")
      print("In ELSE")
      print(paste("Total generated:", total_generated))
      print(paste("Total collected:", total_collected))
      print(paste("Remaining to collect:", remaining_to_collect))
      print(paste("Expected collected:", expected_collected))
      print(paste(
        "Waste collection consistency check passed for",
        region,
        waste_type
      ))
    }
  }
  
  # Function to handle overflow
  handle_overflow <- function(env, region, waste_type, waste_amount) {
    current_storage <- get_global(env,
                                  paste0("total_waste_stored_", region, "_", waste_type))
    new_storage <- current_storage + waste_amount
    overflow_penalty <- 0
    
    if (is.na(new_storage)) {
      overflow_penalty <- 0
    }
    
    if (new_storage >= input$storage_capacity) {
      overflow_amount <- new_storage - input$storage_capacity
      overflow_penalty <- overflow_amount * input$overflow_penalty_per_unit
      new_storage <- input$storage_capacity
    } else {
      overflow_penalty <- 0
    }
    list(new_storage = new_storage, overflow_penalty = overflow_penalty)
  }
  
  # Function to balance storage across regions
  balance_storage_across_regions <- function(env, params, distance_matrix) {
    transportation_costs <- matrix(
      0,
      nrow = length(params$regions),
      ncol = length(params$wood_waste_types),
      dimnames = list(params$regions, params$wood_waste_types)
    )
    
    for (waste_type in params$wood_waste_types) {
      total_storages <- sapply(params$regions, function(region)
        get_global(
          env,
          paste0("total_waste_stored_", region, "_", waste_type)
        ))
      total_storage <- sum(total_storages)
      avg_storage <- total_storage / length(params$regions)
      
      cat("Initial storage for waste type:", waste_type, "\n")
      for (region in params$regions) {
        initial_storage <- get_global(env,
                                      paste0("total_waste_stored_", region, "_", waste_type))
        cat("  Region:",
            region,
            "Initial storage (kg):",
            initial_storage,
            "\n")
      }
      
      for (region in params$regions) {
        current_storage <- get_global(env,
                                      paste0("total_waste_stored_", region, "_", waste_type))
        transfer_amount <- avg_storage - current_storage
        
        if (transfer_amount > 0) {
          for (source_region in params$regions) {
            if (source_region != region) {
              source_storage <- get_global(env,
                                           paste0(
                                             "total_waste_stored_",
                                             source_region,
                                             "_",
                                             waste_type
                                           ))
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
                  
                  if (transfer_amount <= 0)
                    break
                }
              }
            }
          }
        } else if (transfer_amount < 0) {
          for (target_region in params$regions) {
            if (target_region != region) {
              target_storage <- get_global(env,
                                           paste0(
                                             "total_waste_stored_",
                                             target_region,
                                             "_",
                                             waste_type
                                           ))
              if (target_storage < avg_storage) {
                actual_transfer_amount <- min(-transfer_amount,
                                              avg_storage - target_storage)
                if (actual_transfer_amount > 0) {
                  distance <- distance_matrix[region, target_region]
                  transport_cost <- actual_transfer_amount * params$transportation_cost_per_unit * distance
                  
                  current_storage <- current_storage - actual_transfer_amount
                  target_storage <- target_storage + actual_transfer_amount
                  transfer_amount <- transfer_amount + actual_transfer_amount
                  
                  transportation_costs[region, waste_type] <- transportation_costs[region, waste_type] + transport_cost
                  transportation_costs[target_region, waste_type] <- transportation_costs[target_region, waste_type] + transport_cost
                  
                  if (transfer_amount >= 0)
                    break
                }
              }
            }
          }
        }
        
        cat(
          "Final storage in region",
          region,
          "for waste type",
          waste_type,
          ":",
          current_storage,
          "kg\n"
        )
      }
    }
    
    return(transportation_costs)
  }
  
  # Function to generate a random distance matrix
  generate_distance_matrix <- function(regions) {
    n <- length(regions)
    matrix <- matrix(runif(n * n, min = 1, max = 100),
                     nrow = n,
                     ncol = n)
    diag(matrix) <- 0
    colnames(matrix) <- regions
    rownames(matrix) <- regions
    return(matrix)
  }
  
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
    # validation_status(status)
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
    params_loaded <- load_scenario_parameters(input$scenario, user_params)
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
    params_loaded <- load_scenario_parameters(input$scenario, user_params)
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
      distance_matrix <- generate_distance_matrix(regions)
      output$distance_matrix <- renderTable({
        distance_matrix
      }, rownames = TRUE, colnames = TRUE)
    }
    
    updateTabItems(session, "tabs", "distance_matrix")
  })
  
  # Run simulation -----
  observeEvent(input$run_simulation, {
    # Clear previous results and costs
    results(
      data.frame(
        Time = numeric(),
        Region = character(),
        WasteType = character(),
        Event = character(),
        Amount = numeric(),
        Scenario = character(),
        stringsAsFactors = FALSE
      )
    )
    costs(
      data.frame(
        Time = numeric(),
        Region = character(),
        WasteType = character(),
        CollectionCost = numeric(),
        ProcessingCost = numeric(),
        TransportationCost = numeric(),
        RecyclingRevenue = numeric(),
        Scenario = character(),
        stringsAsFactors = FALSE
      )
    )
    
    
    withProgress(message = 'Running simulation...', value = 0, {
      user_params <- load_parameters()
      result <- load_scenario_parameters(input$scenario, user_params)
      params <- result$params
      
      env <- simmer("Wood Waste Flow Simulation")
      
      for (region in params$regions) {
        for (waste_type in params$wood_waste_types) {
          env <- env |>
            add_global(paste0("total_waste_generated_", region, "_", waste_type),
                       0) |>
            add_global(paste0("total_waste_collected_", region, "_", waste_type),
                       0) |>
            add_global(paste0("total_waste_handled_", region, "_", waste_type),
                       0) |>
            add_global(paste0("total_waste_stored_", region, "_", waste_type),
                       0) |>
            add_global(paste0("total_waste_recycled_", region, "_", waste_type),
                       0) |>
            add_global(paste0("overflow_penalty_", region, "_", waste_type),
                       0) |>
            add_global(paste0("waste_to_collect_", region, "_", waste_type),
                       0) |>
            add_global(paste0("total_storage_cost_", region, "_", waste_type),
                       0) |>
            add_global(paste0("total_collection_cost_", region, "_", waste_type),
                       0) |>
            add_global(paste0("total_processing_cost_", region, "_", waste_type),
                       0) |>
            add_global(paste0(
              "total_transportation_cost_",
              region,
              "_",
              waste_type
            ),
            0) |>
            add_global(paste0("total_recycling_revenue_", region, "_", waste_type),
                       0) |>
            add_global(paste0(
              "total_avoided_disposal_cost_",
              region,
              "_",
              waste_type
            ),
            0)
        }
      }
      
      for (region in params$regions) {
        for (waste_type in params$wood_waste_types) {
          env <- env |>
            add_resource(paste0("storage_", region, "_", waste_type),
                         capacity = params$storage_capacity)
        }
      }
      
      env <- env |>
        add_resource(
          "storage",
          capacity = params$storage_capacity * length(params$regions) * length(params$wood_waste_types)
        ) |>
        add_resource("collection_truck", capacity = length(params$regions)) |>
        add_resource("waste_processor", capacity = length(params$regions)) |>
        add_resource("recycling_facility", capacity = length(params$regions))
      
      
      trajectories <- list()
      for (region in params$regions) {
        for (waste_type in params$wood_waste_types) {
          trajs <- create_trajectories(env, params, region, waste_type)
          trajectories[[paste(region, waste_type, sep = "_")]] <- trajs
          env <- env |>
            add_generator(paste0("waste_gen_", region, "_", waste_type), trajs$generate_waste, function()
              rexp(
                1,
                rate = 0.5 * ifelse(
                  is.null(params$generation_rate_increase) ||
                    is.na(params$generation_rate_increase),
                  1,
                  params$generation_rate_increase
                )
              )) |>
            add_generator(paste0("waste_collector_", region, "_", waste_type), trajs$collect_waste, function()
              rexp(
                1,
                rate = 1.5 * ifelse(
                  is.null(params$collection_rate_increase) ||
                    is.na(params$collection_rate_increase),
                  1,
                  params$collection_rate_increase
                )
              )) |>
            add_generator(paste0("waste_handler_", region, "_", waste_type), trajs$handle_waste, function()
              rexp(
                1,
                rate = 1 * ifelse(
                  is.null(params$handling_rate_increase) ||
                    is.na(params$handling_rate_increase),
                  1,
                  params$handling_rate_increase
                )
              )) |>
            add_generator(paste0("waste_recycler_", region, "_", waste_type), trajs$recycle_waste, function()
              rexp(
                1,
                rate = 1 * ifelse(
                  is.null(params$recycling_rate_increase) ||
                    is.na(params$recycling_rate_increase),
                  1,
                  params$recycling_rate_increase
                )
              ))
        }
      }
      
      incProgress(0.5)
      
      env |>
        run(until = default_values$runtime)
      # env |> run(until = 365) # Imitating a year-long simulation
      
      incProgress(0.8)
      
      distance_matrix <- generate_distance_matrix(params$regions)
      incProgress(0.9)
      transportation_costs <- balance_storage_across_regions(env, params, distance_matrix)
      incProgress(1)
      
      updateTabItems(session, "tabs", "outputs")
      
      # Simulation Output -----
      
      generate_waste_type_table <- function(region, waste_type, env, params) {
        data.frame(
          Metric = c(
            "Total waste generated (kg)",
            "Total waste collected (kg)",
            "Total waste handled (kg)",
            "Total waste stored (kg)",
            "Total waste recycled (kg)",
            "Overflow penalty",
            "Total storage cost",
            "Total collection cost",
            "Total processing cost",
            "Total transportation cost",
            "Transportation cost after balancing",
            "Total recycling revenue",
            "Total avoided disposal cost"
          ),
          Value = c(
            get_global(
              env,
              paste0("total_waste_generated_", region, "_", waste_type)
            ),
            get_global(
              env,
              paste0("total_waste_collected_", region, "_", waste_type)
            ),
            get_global(
              env,
              paste0("total_waste_handled_", region, "_", waste_type)
            ),
            get_global(
              env,
              paste0("total_waste_stored_", region, "_", waste_type)
            ),
            get_global(
              env,
              paste0("total_waste_recycled_", region, "_", waste_type)
            ),
            get_global(
              env,
              paste0("overflow_penalty_", region, "_", waste_type)
            ),
            get_global(
              env,
              paste0("total_storage_cost_", region, "_", waste_type)
            ),
            get_global(
              env,
              paste0("total_collection_cost_", region, "_", waste_type)
            ),
            get_global(
              env,
              paste0("total_processing_cost_", region, "_", waste_type)
            ),
            get_global(
              env,
              paste0("total_transportation_cost_", region, "_", waste_type)
            ),
            transportation_costs[region, waste_type],
            get_global(
              env,
              paste0("total_recycling_revenue_", region, "_", waste_type)
            ),
            get_global(
              env,
              paste0(
                "total_avoided_disposal_cost_",
                region,
                "_",
                waste_type
              )
            )
          )
        )
      }
      
      # Update region and waste type selections based on the parameters
      observeEvent(params, {
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
      }, ignoreNULL = FALSE)
      
      # Generate the simulation output tables
      generate_simulation_output <- function(params,
                                             env,
                                             selected_regions,
                                             selected_waste_types,
                                             selected_metrics) {
        simulation_data <- data.frame(
          Region = character(),
          WasteType = character(),
          Metric = character(),
          Value = numeric(),
          stringsAsFactors = FALSE
        )
        
        for (region in selected_regions) {
          for (waste_type in selected_waste_types) {
            waste_data <- generate_waste_type_table(region, waste_type, env, params)
            waste_data <- waste_data[waste_data$Metric %in% selected_metrics, ]
            waste_data <- cbind(Region = region,
                                WasteType = waste_type,
                                waste_data)
            simulation_data <- rbind(simulation_data, waste_data)
          }
        }
        
        datatable(simulation_data,
                  options = list(pageLength = 13, autoWidth = TRUE)) |>
          formatRound(columns = c("Value"), digits = 2)
      }
      
      output$simulation_output <- renderDT({
        generate_simulation_output(
          params,
          env,
          input$selected_region,
          input$selected_waste_type,
          input$selected_metric
        )
      })
      
      # Cumulative Totals -----
      
      output$cumulative_totals <- renderDT({
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
        datatable(results(), options = list(pageLength = 10, autoWidth = TRUE))
      })
      
      # Cost Plots -----
      
      generate_cost_plot_data <- function(cost_type) {
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
        datatable(costs(), options = list(pageLength = 10, autoWidth = FALSE))
      })
      
      # Distance Matrix -----
      
      output$distance_matrix <- renderTable({
        distance_matrix
      }, rownames = TRUE, colnames = TRUE)
      
      output$distance_matrix_plot <- renderPlotly({
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
            # Flip the order of regions on the x-axis
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
          # Generate report content
          report_content <- rmarkdown::render("report_template.Rmd", params = list(data = results()))
          file.copy(report_content, file)
        }
      )
      
    })
  })
  
})
