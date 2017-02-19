
if(FALSE){
  install.packages("tidyr")
  install.packages("dplyr")
  install.packages("uuid")
  install.packages("lme4")

  install.packages("ggplot2")
  install.packages("plotly")

  install.packages("igraph")
  source("https://bioconductor.org/biocLite.R")
  biocLite("RBGL")
  install.packages("QuACN")
}

setwd("/home/felix/projects/graph-complexity")
source("getGraph.r")
source("graphMetrics.r")

loadData <- function() {
  library(tidyr)
  library(dplyr)
  library(scales)
  data <- merge(merge(
    read.csv(file = "~/projects/graph-complexity/batch1.csv", header=TRUE),
    read.csv(file = "~/projects/graph-complexity/batch2.csv", header=TRUE),
    all=TRUE
  ),
    read.csv(file = "~/projects/graph-complexity/batch3.csv", header=TRUE),
    all=TRUE)


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

  onlyOneComponent <- Vectorize(function(g) {count_components(getGraph(g), mode = "weak") == 1})
  d <- filter(d, onlyOneComponent(d$graph_graph))

  d
}



d <- loadData()
plot_ly(d, x=~pred)
gml_column <- "graph_graph"
d <- graphMetrics(d, gml_column)


correlations <- c()
for(i in 1:1000) {

  dtemp <- sample_n(d, 100)
  r <- cor(dtemp$graph_complexity, dtemp$graph_topologicalInfoContent)
  correlations <- c(correlations,r)
}
r <- data.frame(r = correlations)
plot_ly(r, x = ~r)
s <- filter(d, d$graph_vertexCount == 4)
plot_ly(s, x = ~graph_complexity)
plot_ly(d, x = ~graph_topologicalInfoContent)



# for( a in 1:length(d$graph_graph) ) {
#   ag <- getGraph(d$graph_graph[a])
#   for( b in 1:length(d$graph_graph)) {
#     if(a < b) {
#     bg <- getGraph(d$graph_graph[b])
#       if(length(V(ag)) > 1 && isomorphic(ag,bg)) {
#           print("wooooooo")
#           print(a)
#           print(b)
#           print(length(V(ag)))
#           print(length(E(ag)))
#           print(d$graph_complexity[a])
#           print(d$graph_complexity[b])
#           plot(ag)
#         }
#     }
#   }
# }


#### EVALUATION
library(ggplot2)
library(plotly)
library(lme4)

# plot_ly(d, x= ~graph_diameter, y=~graph_vertexCount, z=~graph_complexity, color=~graph_beauty, size = ~WorkTimeInSeconds)
plot_ly(d) %>%
  add_trace( x = ~graph_vertexCount, z = ~graph_edgeCount, y = ~graph_degreesd, color = ~graph_complexity) %>%
  # add_trace(type = 'scatter', x = ~graph_vertexCount, y = ~graph_topologicalInfoContent) %>%
  add_trace()

plot_ly(d) %>%
  add_trace( x = ~graph_beauty, y = ~graph_complexity, color = ~graph_vertexCount)

hist(d$graph_complexity)
hist(d$graph_topologicalInfoContent)
plot_ly(d) %>% add_trace(type = 'scatter', x = ~graph_topologicalInfoContent, name = "topoloInfoContent", y = ~graph_complexity, color = ~graph_vertexCount)
plot_ly(d) %>% add_trace(type = 'scatter', x = ~graph_topologicalInfoContent, name = "topoloInfoContent", y = ~graph_diameter, color = ~graph_vertexCount)

plot_ly(d) %>% add_trace(z = ~graph_complexity, x = ~graph_vertexCount, y = ~graph_edgeCount, color = ~graph_topologicalInfoContentLog)
plot_ly(d) %>% add_trace(z = ~graph_complexity, x = ~graph_vertexCount, y = ~graph_edgeCount, color = ~graph_infoTheoreticGCM)
plot_ly(d) %>% add_trace(z = ~graph_complexity, x = ~graph_vertexCount, y = ~graph_edgeCount, color = ~graph_spectralRadius)


layout(p = plot_ly(d, type = 'scatter', mode = 'markers', color = ~graph_complexity, y = ~graph_topologicalInfoContent, x = ~graph_vertexCount), xaxis = list(type = "log"))
layout(p = plot_ly(d, type = 'scatter', mode = 'markers', color = ~graph_complexity, y = ~graph_edgeCount, x = ~graph_vertexCount))
# layout(p = plot_ly(d, type = 'scatter', mode = 'markers', color = ~graph_beauty, y = ~Answer.age, x = ~graph_vertexCount))

plot_ly(d) %>%
  add_trace(type = 'scatter', x = ~graph_vertexCount, name = "vertexCount", y = ~graph_complexity) %>%
   add_trace(type = 'scatter', x = ~graph_edgeCount, name = "edgeCount", y = ~graph_complexity) %>%
  # add_trace(type = 'scatter', x = ~graph_diameter, name = "diameter", y = ~graph_complexity) %>%
  # add_trace(type = 'scatter', x = ~graph_adhesion, name = "adhesion", y = ~graph_complexity) %>%
  # add_trace(type = 'scatter', x = ~graph_cohesion, name = "cohesion", y = ~graph_complexity) %>%
  # add_trace(type = 'scatter', x = ~graph_componentCount, name = "components", y = ~graph_complexity) %>%
  # add_trace(type = 'scatter', x = ~graph_triangleCount, name = "triangles", y = ~graph_complexity) %>%
  # add_trace(type = 'scatter', x = ~graph_energy, name = "energy", y = ~graph_complexity) %>%
  add_trace(type = 'scatter', x = ~graph_topologicalInfoContent, name = "topoloInfoContent", y = ~graph_complexity) %>%
  # add_trace(type = 'scatter', x = ~graph_infoTheoreticGCM, name = "infoTheoreticGCM", y = ~graph_complexity) %>%
  # add_trace(type = 'scatter', x = ~graph_infoTheoreticGCMDistance, name = "infoTheoreticGCMDistance", y = ~graph_complexity) %>%
  # add_trace(type = 'scatter', x = ~graph_bertz, name = "bertz", y = ~graph_complexity) %>%
  # add_trace(type = 'scatter', x = ~graph_complexityIndexB, name = "complexityIndexB", y = ~graph_complexity) %>%
  # add_trace(type = 'scatter', x = ~graph_compactness, name = "compactness", y = ~graph_complexity) %>%
  # add_trace(type = 'scatter', x = ~graph_symmetryIndex, name = "symmetryIndex", y = ~graph_complexity) %>%
  # add_trace(type = 'scatter', x = ~graph_efficiency, name = "efficiency", y = ~graph_complexity) %>%
  # add_trace(type = 'scatter', x = ~graph_spectralRadius, name = "spectralRadius", y = ~graph_complexity) %>%
  # add_trace(type = 'scatter', x = ~graph_globalClusteringCoeff, name = "globalClusteringCoeff", y = ~graph_complexity) %>%
  #  add_trace(type = 'scatter', x = ~graph_beauty, name = "beauty", y = ~graph_complexity) %>%
  add_trace()

cor(d$graph_vertexCount,d$graph_complexity)
cor(d$graph_edgeCount, d$graph_complexity)


model <- lm(d$graph_beauty ~
             d$graph_vertexCount +
             d$graph_edgeCount +
             d$graph_degreesd +
             d$graph_diameter +
             d$graph_adhesion +
             d$graph_cohesion +
             d$graph_componentCount +
             d$graph_triangleCount +
             d$graph_energy +
             d$graph_topologicalInfoContent +
             d$graph_infoTheoreticGCM  +
             d$graph_bertz +
             d$graph_spectralRadius +
             d$Answer.Gender +
             d$Answer.age +
             d$exp1 +
             # d$exp2 +
             d$Answer.distracted +
             # d$graph_complexityIndexB +
             d$graph_compactness +
             d$graph_symmetryIndex
)
model <- step(model, direction = "backward")
summary(model)
model$anova
confint(model)
# plot(model)



#d2 <- filter(d,d$pred<5)

cor(d$graph_complexity,d$graph_vertexCount)
cor(d$graph_complexity,d$graph_edgeCount)
cor(d$graph_complexity,d$graph_topologicalInfoContent)
cor(d$graph_complexity,d$graph_infoTheoreticGCM)
cor(d$graph_complexity,d$graph_spectralRadius)
cor(d$graph_complexity,d$graph_compactness)
cor(d$graph_complexity,d$graph_energy)
#hist(data$Answer.distracted)
#
#names(data)
#
#head(subdata)
