library(jsonlite)
library(igraph)
library(visNetwork)
library(dplyr)
library(tidyr)

json_path <- file.path(getwd(), "mappings.json")
data <- fromJSON(json_path, flatten = TRUE)

# Build nodes
nodes <- bind_rows(
  data$residues |>   transmute(id = index, label = name, group = "Residues"),
  data$roundwood |>  transmute(id = index, label = name, group = "Roundwood"),
  data$primary_processing_products |>
    transmute(id = index, label = name, group = "Primary Processing"),
  data$intermediate_products |>
    transmute(id = index, label = name, group = "Intermediate Products")
)

edges_pp <- data$primary_processing_products |>
  select(index, applicable_resources) |>
  unnest_longer(applicable_resources) |>
  transmute(from = index, to = applicable_resources)

edges_ip <- data$intermediate_products |>
  select(index, applicable_resources) |>
  unnest_longer(applicable_resources) |>
  transmute(from = index, to = applicable_resources)

edges <- bind_rows(edges_pp, edges_ip)

colors <- c(
  Residues               = "#1f77b4",
  Roundwood              = "#ff7f0e",
  `Primary Processing`   = "#2ca02c",
  `Intermediate Products`= "#d62728"
)

nodes <- nodes |>
  mutate(color = colors[group])

network <- visNetwork(nodes, edges, width = "100%", height = "1000px") |>
  visOptions(
    highlightNearest = list(enabled = TRUE, hover = TRUE),
    nodesIdSelection = TRUE
  ) |>
  visLayout(randomSeed = 123) |>
  visPhysics(
    solver = "forceAtlas2Based",
    forceAtlas2Based = list(gravitationalConstant = -50)
  ) |>
  visEdges(arrows = "to") |>
  visLegend(
    useGroups = FALSE,
    addNodes = data.frame(
      label = c("Residues",
                "Roundwood",
                "Primary Processing",
                "Intermediate Products"),
      shape = "square",
      color = c("#1f77b4",
                "#ff7f0e",
                "#2ca02c",
                "#d62728"),
      stringsAsFactors = FALSE
    )
  )

network |>
  visSave(file = "network.html",
          selfcontained = TRUE,
          background = "white")
