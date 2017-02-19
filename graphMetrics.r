library(igraph)
library(QuACN)

source("./getGraph.r")

graphMetrics <- function (d, gml_column) {
  # https://stackoverflow.com/questions/11308754/add-multiple-columns-to-r-data-table-in-one-function-call


  # d[, c("vertexCount","edgeCount") :=
  # metrics <- c(
  #     c("vertexCount", function(graph){length(V(graph))} ),
  #     c("edgeCount", function(graph){length(E(graph))} )
  #   )

  # d <- within(d, graph_edgeCount <- sapply(graph_graph,FUN=function(g){ length(E(getGraph(g))) } ))

  # TODO: planarity

  d$graph_degreesd <- sapply(d[[gml_column]],FUN=function(g){ sd(degree_distribution(getGraph(g))) } )
  #
  d$graph_adhesion <- sapply(d[[gml_column]],FUN=function(g){ a <- adhesion(getGraph(g), checks=TRUE); if(a < 0) 0 else a } )
  d$graph_cohesion <- sapply(d[[gml_column]],FUN=function(g){ cohesion(getGraph(g), checks=TRUE) } )
  d$graph_componentCount <- sapply(d[[gml_column]],FUN=function(g){ count_components(getGraph(g), mode = "weak") } )
  d$graph_triangleCount <- sapply(d[[gml_column]],FUN=function(g){ sum(count_triangles(getGraph(g)))/3 } )
  # d$graph_globalClusteringCoeff <- sapply(d[[gml_column]],FUN=function(g){ globalClusteringCoeff(as_graphnel(getGraph(g)))  } )
  d$graph_symmetryIndex <- sapply(d[[gml_column]],FUN=function(g){ symmetryIndex(as_graphnel(getGraph(g)))  } )
  d$graph_complexityIndexB <- sapply(d[gml_column],FUN=function(g){ complexityIndexB(as_graphnel(getGraph(g)))  } )
  d$graph_efficiency <- sapply(d[[gml_column]],FUN=function(g){ efficiency(as_graphnel(getGraph(g)))  } )
  d$graph_spectralRadius <- sapply(d[[gml_column]],FUN=function(g){ spectralRadius(as_graphnel(getGraph(g)))  } )

  # promising
  d$graph_edgeCount <- sapply(d[[gml_column]],FUN=function(g){ length(E(getGraph(g))) } )
  d$graph_vertexCount <- sapply(d[[gml_column]],FUN=function(g){ length(V(getGraph(g))) } )
  d$graph_diameter <- sapply(d[[gml_column]],FUN=function(g){ igraph::diameter(getGraph(g)) } )
  d$graph_energy <- sapply(d[[gml_column]],FUN=function(g){ energy(as_graphnel(getGraph(g)))  } )
  d$graph_compactness <- sapply(d[[gml_column]],FUN=function(g){ compactness(as_graphnel(getGraph(g)))  } )
  d$graph_bertz <- sapply(d[[gml_column]],FUN=function(g){ bertz(as_graphnel(getGraph(g)))  } )
  d$graph_topologicalInfoContent <- sapply(d[[gml_column]],FUN=function(g){ topologicalInfoContent(as_graphnel(getGraph(g)))$entropy  } )
  d$graph_infoTheoreticGCM <- sapply(d[[gml_column]],FUN=function(g){
    g <- getGraph(g)
    if(count_components(g) == 1)
    infoTheoreticGCM(as_graphnel(g), dist = NULL, coeff = "lin", infofunct = "sphere", lambda = 1000, custCoeff=NULL, alpha=0.5, prec=53, flag.alpha=FALSE )$entropy
    else
      -1
    } )
  # d$graph_infoTheoreticGCMDistance <- sapply(d[[gml_column]],FUN=function(g){
  #   g <- getGraph(g)
  #   if(count_components(g) == 1)
  #   infoTheoreticGCM(as_graphnel(g), dist = NULL, coeff = "lin", infofunct = "sphere", lambda = 1000, custCoeff=NULL, alpha=0.5, prec=53, flag.alpha=FALSE )$distance
  #   else
  #     -1
  #   } )

  # ERROR
  # d$graph_laplacianEnergy <- sapply(d[gml_column],FUN=function(g){ laplacianEnergy(as_graphnel(getGraph(g)))  } )
  # d$graph_balabanJ <- sapply(d[gml_column],FUN=function(g){ balabanJ(as_graphnel(getGraph(g)))  } )
  # d$graph_bonchev3 <- sapply(d[gml_column],FUN=function(g){ bonchev3(as_graphnel(getGraph(g)))  } )
  # d$graph_bonchev2 <- sapply(d[gml_column],FUN=function(g){ bonchev2(as_graphnel(getGraph(g)))  } )

  d
}
