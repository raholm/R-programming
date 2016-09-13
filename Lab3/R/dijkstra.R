dijkstra <- function(graph, node.init)
{
  ## This function assumes the nodes start from 1 to |V|.
  dijkstra.check.input(graph, node.init)
  
  init <- dijkstra.initialize(graph, node.init)
  
  return(dijkstra.distances(graph,
                            init$unvisited,
                            init$distances,
                            init$previous))
}

dijkstra.check.input <- function(graph, node.init)
{
  stopifnot(is.data.frame(graph))
  stopifnot(is.numeric(node.init))
  
  required.variables <- c("v1", "v2", "w")
  for (variable in required.variables)
  {
    stopifnot(variable %in% names(graph))
  }
  
  ## Make sure the node.init actually exists.
  stopifnot(node.init %in% graph[, "v1"] || node.init %in% graph[, "v2"])
}

dijkstra.initialize <- function(graph, node.init)
{
  nodes <- unique(c(graph[, "v1"], graph[, "v2"]))
  node.unvisited <- nodes
  node.distances <- rep(Inf, length(nodes))
  node.distances[node.init] <- 0
  node.previous <- rep(NA, length(nodes))
  
  return(list(unvisited=node.unvisited,
              distances=node.distances,
              previous=node.previous))
}

dijkstra.distances <- function(graph, node.unvisited, node.distances, node.previous)
{
  while (!dijkstra.vector.empty(node.unvisited))
  {
    node.current <- dijkstra.node.next(node.unvisited, node.distances)
    
    for (node.neighbor in dijkstra.node.neighbors(node.current, graph))
    {
      node.distance <- node.distances[node.current] + dijkstra.node.distance(node.current,
                                                                             node.neighbor,
                                                                             graph)
      
      if (node.distance < node.distances[node.neighbor])
      {
        node.distances[node.neighbor] <- node.distance
        node.previous[node.neighbor] <- node.current
      }
    }
    
    node.unvisited <- dijkstra.vector.remove(node.unvisited, node.current)
  }
  
  return(node.distances)
}


dijkstra.vector.empty <- function(v)
{
  return(sum(v, na.rm=TRUE) == 0)
}

dijkstra.vector.remove <- function(v, index)
{
  v[index] <- NA
  return(v)
}

dijkstra.node.next <- function(nodes, distances)
{
  distance.min <- Inf
  node.next <- NULL
  
  for (node in nodes)
  {
    if (!is.na(node) && distances[node] < distance.min)
    {
      distance.min <- distances[node]
      node.next <- node
    }
  }
  
  ## Could not find the next node.
  stopifnot(!is.null(node.next))
  
  return(node.next)
}

dijkstra.node.neighbors <- function(node, graph)
{
  return(graph$v2[which(graph$v1 == node)])
}

dijkstra.node.distance <- function(from, to, graph)
{
  distance.index <- NA
  
  for (i in 1:length(graph$v1))
  {
    if (graph$v1[i] == from && graph$v2[i] == to)
    {
      distance.index <- i
      break
    }
  }
  
  ## Could not find an edge.
  stopifnot(!is.na(distance.index))
  
  return(graph$w[distance.index])
}

wiki.graph <- data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
                         v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
                         w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))

dijkstra(wiki.graph, 1)
dijkstra(wiki.graph, 3)
