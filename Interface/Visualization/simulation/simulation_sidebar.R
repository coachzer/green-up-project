# Simulation sidebar entry — DORMANT. Not included by the running app's ui.R.
# To revive: see Interface/Visualization/simulation/README.md
simulation_menu_item <- function() {
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
}
