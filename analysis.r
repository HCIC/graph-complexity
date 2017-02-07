
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

library(tidyr)
library(dplyr)
library(uuid)
library(ggplot2)
library(plotly)

loadData <- function() {
  data <- read.csv(file = "~/downloads/data1.csv")
  
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

library(igraph)

getGraph <- function(graphString){
#  characters <- as.character(graphString)
  filename <- paste("/tmp/", UUIDgenerate(), "temp.gml", sep = "")
  write(graphString, file=filename)
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

d$graph_graph <- as.character(d$graph_graph)

d$graph_vertexCount <- sapply(d$graph_graph,FUN=function(g){ length(V(getGraph(g))) } )
d$graph_edgeCount <- sapply(d$graph_graph,FUN=function(g){ length(E(getGraph(g))) } )
d$graph_diameter <- sapply(d$graph_graph,FUN=function(g){ diameter(as_graphnel(getGraph(g))) } )
d$graph_adhesion <- sapply(d$graph_graph,FUN=function(g){ a <- adhesion(getGraph(g), checks=TRUE); if(a < 0) 0 else a } )
d$graph_cohesion <- sapply(d$graph_graph,FUN=function(g){ cohesion(getGraph(g), checks=TRUE) } )
d$graph_componentCount <- sapply(d$graph_graph,FUN=function(g){ count_components(getGraph(g), mode = "weak") } )
d$graph_triangleCount <- sapply(d$graph_graph,FUN=function(g){ sum(count_triangles(getGraph(g)))/3 } )

library(QuACN)
d$graph_wienerIndex <- sapply(d$graph_graph,FUN=function(g){ wiener(as_graphnel(getGraph(g)))  } )
d$graph_energy <- sapply(d$graph_graph,FUN=function(g){ energy(as_graphnel(getGraph(g)))  } )
#d$graph_infoTheoreticGCM <- sapply(d$graph_graph,FUN=function(g){ infoTheoreticGCM(as_graphnel(getGraph(g)))$entropy  } )




#### EVALUATION

plot_ly(d, x= ~graph_diameter, y=~graph_VertexCount, z=~graph_complexity, color=~graph_beauty, size = ~WorkTimeInSeconds)

plot_ly(d) %>%
  add_trace(type = 'scatter', x = ~graph_vertexCount, name = "vertexCount", y = ~graph_complexity) %>%
#  add_trace(type = 'scatter', x = ~graph_edgeCount, name = "edgeCount", y = ~graph_complexity) %>%
  add_trace(type = 'scatter', x = ~graph_diameter, name = "diameter", y = ~graph_complexity) %>%
#  add_trace(type = 'scatter', x = ~graph_adhesion, name = "adhesion", y = ~graph_complexity) %>%
#  add_trace(type = 'scatter', x = ~graph_cohesion, name = "cohesion", y = ~graph_complexity) %>%
#  add_trace(type = 'scatter', x = ~graph_componentCount, name = "components", y = ~graph_complexity) %>%
#  add_trace(type = 'scatter', x = ~graph_triangleCount, name = "triangles", y = ~graph_complexity) %>%
  add_trace(type = 'scatter', x = ~graph_energy, name = "energy", y = ~graph_complexity) %>%
#  add_trace(type = 'scatter', x = ~graph_wienerIndex, name = "wienerIndex", y = ~graph_complexity) %>%
#  add_trace(type = 'scatter', x = ~graph_beauty, name = "beauty", y = ~graph_complexity) %>%
  add_trace()

cor(d$graph_vertexCount,d$graph_complexity)
cor(d$graph_edgeCount, d$graph_complexity)

model <- lm(d$graph_complexity ~ d$graph_vertexCount + d$graph_energy)
model
plot(model)

#d2 <- filter(d,d$pred<5)

#cor(d$pred,d$Answer.distracted)
#hist(data$Answer.distracted)
#
#names(data)
#
#head(subdata)
