library(plotly)
source("./graphMetrics.r")

d <- read.csv(file = "/home/felix/downloads/random_graphs (18).csv", header=TRUE)

gml_column <- "graph_gml"
# d <- graphMetrics(d, "graph_gml")

d$graph_vertexCount <- sapply(d[[gml_column]],FUN=function(g){ length(V(getGraph(g))) } )
hist(d$graph_vertexCount)
d$graph_edgeCount <- sapply(d[[gml_column]],FUN=function(g){ length(E(getGraph(g))) } )
hist(d$graph_edgeCount)
d$graph_degreesd <- sapply(d[[gml_column]],FUN=function(g){ g <- getGraph(g); sd(degree_distribution(g))/length(E(g)) } )
hist(d$graph_degreesd)
d$graph_componentCount <- sapply(d[[gml_column]],FUN=function(g){ count_components(getGraph(g), mode = "weak") } )
hist(d$graph_componentCount)


plot_ly(d) %>%
  add_trace( x = ~graph_vertexCount, z = ~graph_edgeCount, y = ~graph_degreesd, color = ~graph_componentCount) %>%
  # add_trace(type = 'scatter', x = ~graph_vertexCount, y = ~graph_topologicalInfoContent) %>%
  add_trace()
