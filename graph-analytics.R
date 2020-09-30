library(here)
library(igraph)

source("graph-functions.R")
sanity_check()

# Create a graph
graph.file <- here("data", "CA-GrQc.txt")
graph.orig <- file_to_graph(graph.file)

# Select the largest component (due to data considerations)
graph <- analyze_components(graph.orig)

# Calculate top 5 most central nodes
closeness_centrality <- closeness(graph, mode = "all")
head(closeness_centrality, n = 5)

# Degree distribution
avg_degree <- mean(degree(graph))
message(sprintf("Average degree: %f", avg_degree))

# Longest path
longest_path <- get_diameter(graph, directed = FALSE)
longest_path.graph <- induced.subgraph(graph, vids=longest_path)
plot(longest_path.graph, layout = layout.grid)
message(sprintf("The longest path's distance is %d", gsize(longest_path.graph)))

# Largest cliques
lc <- largest_cliques(graph)
lc.graph <- induced.subgraph(graph, vids=lc[[1]])
message(sprintf("Largest clique has %d nodes and %d eges", vcount(lc.graph), gsize(lc.graph)))
plot(lc.graph)

# Egos
plot_communities(graph.orig)
plot_communities(graph)
egos <- ego(graph)
egos.largest <- egos[[which.max(sapply(egos, length))]]
egos.graph <- induced.subgraph(graph, vids=egos.largest)
message(sprintf("Largest clique has %d nodes and %d eges", vcount(egos.graph), gsize(egos.graph)))
plot(egos.graph, layout=layout.lgl, vertex.size = 0.25*degree(egos.graph))

# Power centrality
power_cent <- power_centrality(graph, exponent = 0.9)
message(sprintf("Max power centrality: %f", max(power_cent)))
