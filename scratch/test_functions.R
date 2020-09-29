#install.packages("igraph")
library(igraph)

# get data --> add your own path here
dataPath <- "./Documents/grad/bigdata/bigdata_project1_rood_frank"
setwd(dataPath)
getwd()
myData <- read.delim("data/project1_data.txt")

#set up graph
agraph <- graph_from_edgelist(as.matrix(myData))
agraph
plot.igraph(agraph)

#get adj matrix
agraph.adj <- igraph::get.adjacency(agraph)
agraph.adj

#density and other metrics
agraph.density = gden(agraph)
agraph.density

igraph::edge_density(agraph)
igraph::edge_density(agraph, loops=T)

#degree centrality
igraph::degree(agraph)
igraph::centr_betw(agraph)
igraph::centr_clo(agraph)
igraph::get.shortest.paths(agraph, 5)

#historgram
hist(igraph::degree(agraph))




