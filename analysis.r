
if(FALSE){
  install.packages("tidyr")
  install.packages("dplyr")
  install.packages("uuid")

  install.packages("ggplot2")
  install.packages("plotly")

  install.packages("igraph")
  source("https://bioconductor.org/biocLite.R")
  biocLite("RBGL")
  install.packages("QuACN")
}

loadData <- function() {
  library(tidyr)
  library(dplyr)
  data <- merge(
    read.csv(file = "~/projects/graph-complexity/batch1.csv", header=TRUE),
    read.csv(file = "~/projects/graph-complexity/batch2.csv", header=TRUE),
    all=TRUE
  )


  subdata <- data[,c(16,24,28,29,34,35,36:174)]

  subdata <- separate(data=subdata, col=Answer.graph.experience, into = c("exp1", "exp2"), sep = "\\|")

  subdata$exp1 <- as.numeric(subdata$exp1)
  subdata$exp2 <- as.numeric(subdata$exp2)
  subdata$Answer.distracted <- as.numeric(subdata$Answer.distracted)

  newdata <- data.frame()
  for(i in 0:4){
    temp <- subdata %>% select(c(1:7,contains(paste("Answer.graph",i,sep=""))))
    tNames <- names(temp)
    tNames <- gsub("Answer\\.graph\\d_", "graph_", tNames, perl=T)
    names(temp)<-tNames
    newdata <- rbind(newdata,temp)
  }

  newdata$pred = abs( 7 - newdata$exp2 - newdata$exp1 ) + newdata$Answer.distracted
  d <- na.omit(newdata)
  d
}


# Daten laden
d <- loadData()


getGraph <- function(graphString){
  library(igraph)
  library(uuid)

  #  characters <- as.character(graphString)
  filename <- paste("/tmp/", UUIDgenerate(), "temp.gml", sep = "")
  write(as.character(graphString), file=filename)
  graph <- as.undirected(read_graph(filename, format="gml"), mode="collapse")
  file.remove(filename)


  # https://github.com/igraph/rigraph/issues/154
  graph <- delete_vertex_attr(graph, "x")
  graph <- delete_vertex_attr(graph, "y")

  edges <- which_multiple(graph, eids = E(graph))
  es <- E(graph)[edges]
  graph <- delete_edges(graph, es)
  graph
}


graphMetrics <- function (d) {
  library(igraph)
  library(QuACN)
  # https://stackoverflow.com/questions/11308754/add-multiple-columns-to-r-data-table-in-one-function-call



  # d[, c("vertexCount","edgeCount"
  # metrics <-

  # d <- within(d, graph_edgeCount <- sapply(graph_graph,FUN=function(g){ length(E(getGraph(g))) } ))
  d$graph_edgeCount <- sapply(d$graph_graph,FUN=function(g){ length(E(getGraph(g))) } )
  d$graph_adhesion <- sapply(d$graph_graph,FUN=function(g){ a <- adhesion(getGraph(g), checks=TRUE); if(a < 0) 0 else a } )
  d$graph_cohesion <- sapply(d$graph_graph,FUN=function(g){ cohesion(getGraph(g), checks=TRUE) } )
  d$graph_componentCount <- sapply(d$graph_graph,FUN=function(g){ count_components(getGraph(g), mode = "weak") } )
  d$graph_triangleCount <- sapply(d$graph_graph,FUN=function(g){ sum(count_triangles(getGraph(g)))/3 } )
  d$graph_globalClusteringCoeff <- sapply(d$graph_graph,FUN=function(g){ globalClusteringCoeff(as_graphnel(getGraph(g)))  } )
  d$graph_symmetryIndex <- sapply(d$graph_graph,FUN=function(g){ symmetryIndex(as_graphnel(getGraph(g)))  } )
  # d$graph_complexityIndexB <- sapply(d$graph_graph,FUN=function(g){ complexityIndexB(as_graphnel(getGraph(g)))  } )
  d$graph_efficiency <- sapply(d$graph_graph,FUN=function(g){ efficiency(as_graphnel(getGraph(g)))  } )
  d$graph_spectralRadius <- sapply(d$graph_graph,FUN=function(g){ spectralRadius(as_graphnel(getGraph(g)))  } )

  # promising
  d$graph_vertexCount <- sapply(d$graph_graph,FUN=function(g){ length(V(getGraph(g))) } )
  d$graph_diameter <- sapply(d$graph_graph,FUN=function(g){ igraph::diameter(getGraph(g)) } )
  d$graph_energy <- sapply(d$graph_graph,FUN=function(g){ energy(as_graphnel(getGraph(g)))  } )
  d$graph_compactness <- sapply(d$graph_graph,FUN=function(g){ compactness(as_graphnel(getGraph(g)))  } )
  d$graph_bertz <- sapply(d$graph_graph,FUN=function(g){ bertz(as_graphnel(getGraph(g)))  } )
  d$graph_topologicalInfoContent <- sapply(d$graph_graph,FUN=function(g){ topologicalInfoContent(as_graphnel(getGraph(g)))$entropy  } )

  # ERROR
  # d$graph_infoTheoreticGCM <- sapply(d$graph_graph,FUN=function(g){ infoTheoreticGCM(as_graphnel(getGraph(g)))$entropy  } )
  # d$graph_laplacianEnergy <- sapply(d$graph_graph,FUN=function(g){ laplacianEnergy(as_graphnel(getGraph(g)))  } )
  # d$graph_balabanJ <- sapply(d$graph_graph,FUN=function(g){ balabanJ(as_graphnel(getGraph(g)))  } )
  # d$graph_bonchev3 <- sapply(d$graph_graph,FUN=function(g){ bonchev3(as_graphnel(getGraph(g)))  } )
  # d$graph_bonchev2 <- sapply(d$graph_graph,FUN=function(g){ bonchev2(as_graphnel(getGraph(g)))  } )

  d
}

d <- graphMetrics(d)


#### EVALUATION
library(ggplot2)
library(plotly)

# plot_ly(d, x= ~graph_diameter, y=~graph_vertexCount, z=~graph_complexity, color=~graph_beauty, size = ~WorkTimeInSeconds)
plot_ly(d) %>%
  add_trace(type = 'scatter', x = ~graph_vertexCount, y = ~graph_edgeCount)

plot_ly(d) %>%
  # add_trace(type = 'scatter', x = ~graph_vertexCount, name = "vertexCount", y = ~graph_complexity) %>%
  #  add_trace(type = 'scatter', x = ~graph_edgeCount, name = "edgeCount", y = ~graph_complexity) %>%
  add_trace(type = 'scatter', x = ~graph_diameter, name = "diameter", y = ~graph_complexity) %>%
  # add_trace(type = 'scatter', x = ~graph_adhesion, name = "adhesion", y = ~graph_complexity) %>%
  # add_trace(type = 'scatter', x = ~graph_cohesion, name = "cohesion", y = ~graph_complexity) %>%
  # add_trace(type = 'scatter', x = ~graph_componentCount, name = "components", y = ~graph_complexity) %>%
  # add_trace(type = 'scatter', x = ~graph_triangleCount, name = "triangles", y = ~graph_complexity) %>%
  add_trace(type = 'scatter', x = ~graph_energy, name = "energy", y = ~graph_complexity) %>%
  add_trace(type = 'scatter', x = ~graph_topologicalInfoContent, name = "topoloInfoContent", y = ~graph_complexity) %>%
  add_trace(type = 'scatter', x = ~graph_bertz, name = "bertz", y = ~graph_complexity) %>%
  # add_trace(type = 'scatter', x = ~graph_complexityIndexB, name = "complexityIndexB", y = ~graph_complexity) %>%
  add_trace(type = 'scatter', x = ~graph_compactness, name = "compactness", y = ~graph_complexity) %>%
  # add_trace(type = 'scatter', x = ~graph_symmetryIndex, name = "symmetryIndex", y = ~graph_complexity) %>%
  # add_trace(type = 'scatter', x = ~graph_efficiency, name = "efficiency", y = ~graph_complexity) %>%
  add_trace(type = 'scatter', x = ~graph_spectralRadius, name = "spectralRadius", y = ~graph_complexity) %>%
  # add_trace(type = 'scatter', x = ~graph_globalClusteringCoeff, name = "globalClusteringCoeff", y = ~graph_complexity) %>%
  #  add_trace(type = 'scatter', x = ~graph_beauty, name = "beauty", y = ~graph_complexity) %>%
  add_trace()

cor(d$graph_vertexCount,d$graph_complexity)
cor(d$graph_edgeCount, d$graph_complexity)


summary(lm(d$graph_complexity ~
             d$graph_vertexCount +
             # d$graph_edgeCount +
             d$graph_diameter +
             # d$graph_adhesion +
             # d$graph_cohesion +
             # d$graph_componentCount +
             # d$graph_triangleCount +
             d$graph_energy +
             d$graph_topologicalInfoContent +
             d$graph_bertz +
             d$graph_spectralRadius
             # d$graph_complexityIndexB
             # d$graph_compactness
             # d$graph_symmetryIndex
))
# plot(model)

#d2 <- filter(d,d$pred<5)

cor(d$graph_complexity,d$graph_topologicalInfoContent)
#hist(data$Answer.distracted)
#
#names(data)
#
#head(subdata)
