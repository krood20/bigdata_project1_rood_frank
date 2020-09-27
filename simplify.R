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
dec <- decompose(graph.simple)
message(length(dec))

# Selects largest graph (4158)
largest <- dec[[which.max(sapply(dec, vcount))]]
message(vcount(largest))
layout = layout_in_circle(largest)
plot.igraph(largest, layout=layout.kamada.kawai, vertex.size=1, vertex.label=NA)

# Smallest has 1 node
smallest <- decompose[[which.min(sapply(decompose, vcount))]]
message(vcount(smallest))

# Cool visualization
communities = cluster_fast_greedy(graph.simple)
coords = layout_with_graphopt(graph.simple)
plot(communities, graph.simple, layout=coords, vertex.size=1, vertex.label=NA)

# Over 50% only has 2 authors
sizes = sapply(decompose, vcount)
sizes.table = table(sizes)
sizes.percent <- prop.table(sizes.table)*100
barplot(
  sizes.percent, 
  ylab = "Percent Distribution", 
  xlab="# Vertices in Graph", 
  col="darkred",
  ylim=range(pretty(c(0, percent)))
)

isolated = which(sapply(dec, vcount)==1)
plot(dec[[isolated]])

isolated <- which(degree(graph.simple)==0)
isolated

# Trimming
#trim = decompose[lapply(decompose, vcount)>10]
#message(length(trim))

# Removes the "not so connected" vertices
# message(gsize(graph.simple))
# graph.simple <- delete.vertices(graph.simple, which(degree(graph.simple)<=10))
# message(gsize(graph.simple))
