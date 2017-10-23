

################################################################################
##
## Function: GouldBrokerage
##
## Purpose: calculates Gould's brokerage score for each node in a given network.
##
##
## Inputs:
##  - in_matrix: adjacency matrix used for network calculations. The input matrix
##    must be symmetric and binary.
##
## Process:
##  1. Intake adjacency matrix and calculate all indirect length-2 paths.
##  2. Drop isolates from data set - they are not useful in identifying
##      communities, and for some algorithms (spinglass) they cause errors.
##  3. Convert adjacency matrix to graph structure.
##  4. Identify all communities in the resulting graph.
##  5. Implement Gould's algorithm for identifying, for each node j, the
##      proportion of all length-2 paths connecting all other nodes (i,k)
##      that pass through node j.
##
## Returns:
##  A length-N vector where element i corresponds to the brokerage centrality
##  of node i in the network.
##
################################################################################

GouldBrokerage <- function(in_matrix){
  
  ## Drop isolated nodes - spinglass won't run with isolates
  in_matrix <- in_matrix[rowSums(in_matrix) > 0, colSums(in_matrix) > 0]
  new_n <- nrow(in_matrix)
  diag(in_matrix) <- 1
  
  ## Invert the original matrix (1 -> 0, 0 -> 1). Each 0-element now represents
  ##  the presence of a *direct* tie between two nodes. Because we are only
  ##  interested in *indirect* length-2 paths between nodes, any dyad with a
  ##  direct connection is not considered when looking at length-2 paths.
  flip_in_matrix <- 1- in_matrix
  
  
  ## Calculate all length-2 path between nodes. For now, only look at 2-length 
  ##  paths; later consider using exponential distance: expm(in_matrix)
  paths2matrix <- in_matrix %^% 2
  
  ## Remove all (i,j) dyads that share a direct connection
  indirect_paths <- paths2matrix * flip_in_matrix
  
  ## Create graph object
  broker_graph <- igraph::graph_from_adjacency_matrix(
    in_matrix
    , mode = 'undirected'
    , diag = F
    )
  
  ## Calculate communities - currently defaulting to 'walktrap' method
  communities <- igraph::cluster_walktrap(broker_graph)
  
  ## Retrieve each node's community membership
  node_membership <- communities$membership
  
  ## Create a cross-community matrix: 1 if dyad (i,j) are in *different*
  ##  communities, and 0 otherwise
  dif_com_mat <-  matrix(1, nrow = new_n, ncol = new_n)
  for(i in 1:new_n){
    dif_com_mat[i, ] <- 1 - (node_membership %in% node_membership[i])
  }
  
  ## Create a 'valid-ties' matrix: each element contains the count of indirect
  ##  length-2 paths between nodes i and j that *do not* have a direct tie.
  ##  This corresponds to the P_ik term in Gould's algorithm.
  num_valid_ties <- indirect_paths * dif_com_mat
  num_valid_ties[lower.tri(num_valid_ties)] <- 0
  
  ## Create dummy matrix: 1 if dyad (i,j) has >0 valid paths, 0 otherwise.
  ##  This corresponds to the B_ik term in Gould's algorithm.
  valid_ties <- matrix(as.integer(as.logical(num_valid_ties)), nrow = new_n)
  
  ##########
  ##
  ## Sub-function: brokerage
  ##
  ## Purpose: calculate brokerage score for a single node in the network.
  ##
  ## Inputs:
  ##  - node_idx: index of node for which brokerage score will be calculated.
  ##
  ## Process:
  ##  1. Get length-1 egonet for node i (all length-1 ties).
  ##  2. Calculate all 'valid' length-2 paths between all nodes in i's egonet.
  ##  3. Calculate the share of all paths between node i's egonet partners
  ##      that *do not* share a direct tie.
  ##
  ##
  ##########
  
  brokerage <- function(node_idx){
    
    ## Get all 1-path neighbors of the focal node
    paths2 <- ego(broker_graph, 1, node_idx)[[1]][-1]
    
    ## Create an origin/destination matrix of all node pairs sharing 
    ##  a length-2 connection. Because these are undirected networks, only
    ##  keep A-B pairs where index(A) > index(B)
    paths_idx <- as.matrix(expand.grid(paths2, paths2))
    paths_idx <- paths_idx[which(dif_com_mat[paths_idx] == 1), ]
    paths_idx <- paths_idx[paths_idx[,1] > paths_idx[,2], ]
    paths_idx <- matrix(paths_idx, ncol = 2)
    
    ##########
    ##
    ## Sub-function: brokerage_ik
    ##
    ## Purpose: calculate brokerage score for a single pair (i,k) that are both
    ##  direct neighbors of node i, and not direct neighbors of one another.
    ##
    ## Inputs:
    ##  - pair_idx: index of dyad (i,k) for which brokerage score
    ##    will be calculated.
    ##
    ## Process:
    ##  1. Calculate all shortest paths between nodes i and k.
    ##  2. Keep all paths of length 2.
    ##  3. Calculate the share of i-k paths that pass through node j.
    ##
    ##
    ##########
    brokerage_ik <- function(pair_idx){
      
      ## Initialize output object
      brokerage <- 0
      
      ## Calculate all shortest i-k paths
      ties_dist <- igraph::all_shortest_paths(
        broker_graph
        , from = paths_idx[pair_idx, 1]
        , to = paths_idx[pair_idx, 2]
        )$res
    
      ## Keep all paths of length 3 (i-j-k)
      all2paths <- ties_dist[lapply(ties_dist, 'length') == 3]
      
      ## If there exists one or more length-2 paths and no direct path between
      ##  nodes i and k, calculate the share of paths crossing node j.
      if(length(all2paths) > 0){
        n_all2paths <- length(all2paths)
        bikpik <- 1 / n_all2paths
        brokerage <- brokerage + bikpik
      }
      
      return(brokerage)
    }

    ## Calculate brokerage score for a given i-k dyad.
    if(nrow(paths_idx) > 0){
      brokerage_out <- sum(sapply(1:nrow(paths_idx), brokerage_ik))
      return(brokerage_out)
    } else{
      return(0)
    }
  }
  
  ## Calculate brokerage scores for all i-k dyads that are direct partners
  ##  of node i.
  brokerage_scores <- sapply(1:new_n, brokerage)
  
  ## Assign some node characteristics based on network values
  V(broker_graph)$color <- node_membership
  V(broker_graph)$size <- brokerage_scores + 2
  V(broker_graph)$communityid <- node_membership
  V(broker_graph)$between <- betweenness(broker_graph, normalized = T) * 100
  V(broker_graph)$brokerage <- brokerage_scores
  
  ## Plot network with communities
  plot(communities
       , broker_graph
       , vertex.label = NA
       # , layout = layout.kamada.kawai
       )
  
  ## Return network object
  return(broker_graph)
}
