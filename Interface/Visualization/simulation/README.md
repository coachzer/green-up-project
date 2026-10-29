# Simulation Feature (DORMANT)

The simulation feature was relocated here in plan 010 to remove its dependencies
(`simmer`, `igraph`) from the live app's startup. The feature is preserved intact,
including all bug fixes from plan 009.

## How to re-enable

Three wiring steps are needed:

1. **server.R** — inside `shinyServer(function(input, output, session) { ... })`,
   source the simulation files and call:
   ```r
   source("simulation/simulation_server.R")
   register_simulation_server(input, output, session)
   ```

2. **ui.R** — splice the simulation tab bodies into the `tabItems(...)` call:
   ```r
   source("simulation/simulation_ui.R", local = TRUE)
   # Then inside tabItems(...), add:
   do.call(tabItems, c(list(<existing tabItem calls>), simulation_tab_items()))
   ```
   Or simply add `simulation_tab_items()` items directly inside `tabItems(...)`.

3. **ui.R** — add the sidebar entry into `sidebarMenu(...)`:
   ```r
   source("simulation/simulation_sidebar.R", local = TRUE)
   # Then inside sidebarMenu(...), add:
   simulation_menu_item()
   ```

## Layout

- `engine.R` — the model, with no Shiny dependency:
  `sim_params(scenario, season, overrides)` builds parameters and
  `run_wood_waste_sim(params, seed)` returns `events`, `costs`, `totals`,
  `distance_matrix` and `params`.
- `simulation_server.R` — reads inputs, calls the engine once per Run, renders.

## Dependencies

Reviving requires installing:
```r
install.packages(c("simmer", "igraph"))
```
These packages were removed from `global.R` in plan 010 to speed up app startup.
The conflict preferences for igraph/simmer also moved into `simulation_server.R`.

## Honesty caveat

As noted in plan 009 step 5: simulation outputs are synthetic `runif` draws, not
real measurements. All numeric results are randomly generated placeholders. Building
a real data-driven model is a separate, deferred effort.
