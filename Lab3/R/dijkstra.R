#' Computes the least path costs to all nodes in a directed graph from the initial node
#' 
#' The nodes have to labeled from 1 to |V| (number of nodes in total)
#' 
#' @param graph A directed graph which is supposed to compute the least path of different nodes.
#' @param node_init A node which is chosen for computing the shortest path.
#' @return Vector of path costs to every node from \code{node_init}.
#' 
#' @format graph
#' \describe{
#'    \item{v1}{Vector of nodes for edges start positions.}
#'    \item{v2}{Vector of nodes for edges end positions.}
#'    \item{w}{Vector of edge weights.}
#' }
#' 
#' @examples 
#' dijkstra(wiki_graph, 1)
#' dijkstra(wiki_graph, 3)
#' 
#' @export
#' @source \url{https://en.wikipedia.org/wiki/Dijkstra's_algorithm}
dijkstra <- function(graph, node_init) {
    dijkstra_check_input(graph, node_init)

    init <- dijkstra_initialize(graph, node_init)

    return(dijkstra_distances(graph,
                              init$unvisited,
                              init$distances,
                              init$previous))
}

dijkstra_check_input <- function(graph, node_init) {
    stopifnot(is.data.frame(graph))
    stopifnot(is.numeric(node_init))

    required_variables <- c("v1", "v2", "w")
    for (variable in required_variables) {
        stopifnot(variable %in% names(graph))
    }
    
    ## Ensure that the nodes are labeled 1, 2, ..., |V|
    nodes <- unique(c(graph$v1, graph$v2))
    stopifnot(length(nodes) == length(1:max(nodes)))
    stopifnot(nodes == 1:max(nodes))

    ## Make sure the node_init actually exists.
    stopifnot(node_init %in% graph[, "v1"] || node_init %in% graph[, "v2"])
}

dijkstra_initialize <- function(graph, node_init) {
    nodes <- unique(c(graph[, "v1"], graph[, "v2"]))
    node_unvisited <- nodes
    node_distances <- rep(Inf, length(nodes))
    node_distances[node_init] <- 0
    node_previous <- rep(NA, length(nodes))

    return(list(unvisited=node_unvisited,
                distances=node_distances,
                previous=node_previous))
}

dijkstra_distances <- function(graph, node_unvisited, node_distances, node_previous) {
    while (!dijkstra_vector_empty(node_unvisited)) {
        node_current <- dijkstra_node_next(node_unvisited, node_distances)

        for (node_neighbor in dijkstra_node_neighbors(node_current, graph)) {
            node_distance <- node_distances[node_current] + dijkstra_node_distance(node_current,
                                                                                   node_neighbor,
                                                                                   graph)

            if (node_distance < node_distances[node_neighbor]) {
                node_distances[node_neighbor] <- node_distance
                node_previous[node_neighbor] <- node_current
            }
        }

        node_unvisited <- dijkstra_vector_remove(node_unvisited, node_current)
    }

    return(node_distances)
}


dijkstra_vector_empty <- function(v) {
    return(sum(v, na.rm=TRUE) == 0)
}

dijkstra_vector_remove <- function(v, index) {
    v[index] <- NA
    return(v)
}

dijkstra_node_next <- function(nodes, distances) {
    distance_min <- Inf
    node_next <- NULL

    for (node in nodes) {
        if (!is.na(node) && distances[node] < distance_min) {
            distance_min <- distances[node]
            node_next <- node
        }
    }

    ## Could not find the next node
    stopifnot(!is.null(node_next))

    return(node_next)
}

dijkstra_node_neighbors <- function(node, graph) {
    return(graph$v2[which(graph$v1 == node)])
}

dijkstra_node_distance <- function(from, to, graph) {
    distance_index <- NA

    for (i in 1:length(graph$v1)) {
        if (graph$v1[i] == from && graph$v2[i] == to) {
            distance_index <- i
            break
        }
    }

    ## Could not find an edge.
    stopifnot(!is.na(distance_index))

    return(graph$w[distance_index])
}
