library(here)
library(igraph)

source("graph-functions.R")
sanity_check()

# Create a graph
graph.file <- here("data", "CA-GrQc.txt")
graph.orig <- file_to_graph(graph.file)

# Select the largest component (due to data)
graph <- analyze_components(graph.orig)

# Calculate top 5 most central nodes
closeness_centrality <- closeness(graph, mode = "all")
head(closeness_centrality, n = 5)

# Longest path
longest_path <- get_diameter(graph, directed = FALSE)
longest_path.graph <- induced.subgraph(graph, vids=longest_path)
plot(longest_path.graph, layout = layout.grid)
longest_path.distance <- get.diameter(graph, directed = FALSE)
message(sprintf("The longest path's distance is %d", max(longest_path.distance)))

# Largest cliques
lc <- largest_cliques(graph)
lc.graph <- induced.subgraph(graph, vids=lc[[1]])
plot(lc.graph)

# Egos
plot_egos(graph.orig)
plot_egos(graph)
egos <- ego(graph)
egos.largest <- egos[[which.max(sapply(egos, length))]]
egos.graph <- induced.subgraph(graph, vids=egos.largest)
plot(egos.graph, layout=layout.lgl, vertex.size = 0.25*degree(egos.graph))


# Power centrality
power_cent <- power_centrality(graph)
max(power_cent)
