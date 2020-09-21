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

# 355 diconnected subgraphs
decompose <- decompose(graph.simple)
length(decompose)

# Selects larges graph
largest <- decompose[[which.max(sapply(decompose, vcount))]]
message(gsize(largest))
plot.igraph(largest, layout=layout.kamada.kawai, vertex.size=1, vertex.label=NA)

# Smallest has no vertices?
smallest <- decompose[[which.min(sapply(decompose, vcount))]]
message(gsize(smallest))

#components(graph.simple)

# Removes the "not so connected" vertices
message(gsize(graph.simple))
graph.simple <- delete.vertices(graph.simple, which(degree(graph.simple)<=10))
message(gsize(graph.simple))

#layout <- layout.circle(graph)
plot.igraph(graph.simple, layout=layout.kamada.kawai, vertex.size=1, vertex.label=NA)
