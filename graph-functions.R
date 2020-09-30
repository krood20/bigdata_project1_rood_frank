library(igraph)
library(here)

file_to_graph <- function(path, header = TRUE, directed = FALSE, simplify = TRUE){
  stopifnot(file.exists(path))
  graph.data <- read.table(path, header)
  graph <- graph_from_data_frame(graph.data, directed = directed)
  if(simplify){
    graph <- simplify(
      graph,
      remove.multiple = TRUE,
      remove.loops = TRUE
    )
  }
  graph
}

attr(file_to_graph, "comment") <- "Function reads in graph data frame, create igraph object, and optionally simplifies the graph"
attr(file_to_graph, "help") <- "Designed to work with CSC6444 Project 1 datset, calls graph_from_data_frame and simplify"

sanity_check <- function(){
  graph.slim.file <- here("data", "CA-GrQc-slim.txt")
  graph.slim <- file_to_graph(graph.slim.file)
  stopifnot(vcount(graph.slim) == 5)
  stopifnot(gsize(graph.slim) == 4)
  stopifnot(is_simple(graph.slim))
  plot(graph.slim)
}

attr(sanity_check, "comment") <- "Validates that the file_to_graph functions accurately"
attr(sanity_check, "help") <- "Designed to work with slim data set, performs stopifnot"

analyze_components <- function(graph) {
  # Find graphs with highest order
  dec <- decompose(graph)
  message(sprintf("Found %d communities within graph", length(dec)))
  largest <- dec[[which.max(sapply(dec, vcount))]]
  message(sprintf("The largest graph has %d nodes, and %d edges", vcount(largest), gsize(largest)))
  smallest <- dec[[which.min(sapply(dec, vcount))]]
  message(sprintf("The smallest graph has %d nodes, and %d edges", vcount(smallest), gsize(smallest)))
  
  message(smallest)
  
  # Ensure we are not confusing |N| with |V|
  stopifnot(which.max(sapply(dec, vcount)) == which.max(sapply(dec, gsize)))
  
  # Plot sizes
  sizes = sapply(dec, vcount)
  sizes.table = table(sizes)
  sizes.percent <- prop.table(sizes.table)*100
  barplot(
    sizes.percent, 
    ylab = "Percent Distribution", 
    xlab="# Vertices in Graph", 
    col="darkred",
    ylim=range(pretty(c(0, sizes.percent)))
  )
  
  largest
}

attr(analyze_components, "comment") <- "Produces stats and histogram for community size"
attr(analyze_components, "help") <- "Shows vertex distribution as size of |N|"

plot_communities <- function(graph){
  communities = cluster_fast_greedy(graph)
  coords = layout_with_graphopt(graph)
  plot(communities, graph, layout=coords, vertex.size=1, vertex.label=NA)
}

attr(plot_communities, "comment") <- "Color codes dense subgraphs by modularity score and plots"
attr(plot_communities, "help") <- "Leverages the cluster_fast_greedy algorithim to determine communties"


