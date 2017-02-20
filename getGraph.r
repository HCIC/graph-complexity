library(igraph)
library(uuid)

getGraph <- function(graphString){
  graphString <- gsub("\\[object Object\\]", "0", graphString)
  #  characters <- as.character(graphString)
  filename <- paste("/tmp/", UUIDgenerate(), "temp.gml", sep = "")
  write(as.character(graphString), file=filename)
  graph <- as.undirected(read_graph(filename, format="gml"), mode="collapse")
  file.remove(filename)


  # https://github.com/igraph/rigraph/issues/154
  try(graph <- delete_vertex_attr(graph, "x"))
  try(graph <- delete_vertex_attr(graph, "y"))

  edges <- which_multiple(graph, eids = E(graph))
  es <- E(graph)[edges]
  graph <- delete_edges(graph, es)
  graph
}
