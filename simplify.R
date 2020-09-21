library(here)
library(igraph)

graph.file <- here("data", "CA-GrQc.txt")
graph.data <- read.table(graph.file, header = TRUE)

graph <- graph_from_data_frame(graph.data, directed = F)

message(gsize(graph))
graph.simple <- simplify(
  graph,
  remove.multiple = TRUE,
  remove.loops = TRUE
)

# 355 disconnected subgraphs
decompose <- decompose(graph.simple)
length(decompose)

# Selects larges graph
largest <- decompose[[which.max(sapply(decompose, vcount))]]
message(gsize(largest))
layout = layout_in_circle(largest)
plot.igraph(largest, layout=layout.kamada.kawai, vertex.size=1, vertex.label=NA)

# Smallest has no vertices?
smallest <- decompose[[which.min(sapply(decompose, vcount))]]
message(gsize(smallest))

# Cool visualization
communities = cluster_fast_greedy(graph.simple)
coords = layout_with_graphopt(graph.simple)
plot(communities, graph.simple, layout=coords, vertex.size=1, vertex.label=NA)

# Only 3 graphs with values > 10, trying to do histogram
dec <- decompose(graph.simple)
sizes = sapply(dec, vcount)
hist(sizes, breaks = 5, xlim=c(0, gsize(largest)))

trim = decompose[lapply(decompose, vcount)>10]
message(length(trim))

# Removes the "not so connected" vertices
message(gsize(graph.simple))
graph.simple <- delete.vertices(graph.simple, which(degree(graph.simple)<=10))
message(gsize(graph.simple))

#layout <- layout.circle(graph)
plot.igraph(graph.simple, layout=layout.kamada.kawai, vertex.size=1, vertex.label=NA)
