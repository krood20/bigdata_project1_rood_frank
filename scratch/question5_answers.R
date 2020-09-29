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

### Answers to number 5 ###
#(a) central nodes
#one measure of centrality
degree_centrality <- centr_degree(agraph, mode = "all")
degree_centrality

#better representation
closeness_centrality <- closeness(agraph, mode = "all")
closeness_centrality
#top 5 most central: [1], [9], [17], [25], [33]

#(b) longest path
longest_path <- get_diameter(agraph, 5)
longest_path
#[1]   855 21695  6503 21184  3743  2394 18899  1186  7844 16347  4241 10476  4875 11844 17006 19551  7885 22190

#(c) lergest cliques
largest_cliques_var <- largest_cliques(agraph)
largest_cliques_var
#[1], [19], [37]

#(d) ego(s)
ego_list <- ego(agraph)
ego_list[1:20]
#can print out whole list if you comment above line, and uncomment below line
#ego_list

#(e) power centrality
sum(degree(agraph) < 1) # if value is non-zero you have isolates
gg <- delete_vertices(agraph, which(degree(agraph) < 1))
power_cent <- power_centrality(agraph, exponent = -1.1)
power_cent
