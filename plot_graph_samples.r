library(plotly)

setwd("/home/felix/projects/graph-complexity")
source("graphMetrics.r")

d <- read.csv(file = "/home/felix/downloads/random_graphs (46).csv", header=TRUE)

gml_column <- "graph_gml"
# d <- graphMetrics(d, "graph_gml")

hist(d$vertexCount)
hist(d$edgeCount)
hist(d$avgDeg)
hist(d$lambda)
d$degreesd <- sapply(d[[gml_column]],FUN=function(g){ g <- getGraph(g); sd(degree_distribution(g)) })
hist(d$degreesd)
# d$graph_componentCount <- sapply(d[[gml_column]],FUN=function(g){ count_components(getGraph(g), mode = "weak") } )
# hist(d$graph_componentCount)


plot_ly(d) %>%
  # add_trace( x = ~vertexCount, z = ~edgeCount, y = ~degreesd, color = ~lambda) %>%
  # add_trace( x = ~lambda, z = ~edgeCount, y = ~degreesd, color = ~degreesd) %>%
  add_trace( x = ~lambda, y = ~degreesd, color = ~degreesd) %>%
  # add_trace(type = 'scatter', x = ~graph_vertexCount, y = ~graph_topologicalInfoContent) %>%
  add_trace()
