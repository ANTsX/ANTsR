filterfMRIforNetworkAnalysis <- function(aslmat, tr, freqLo = 0.01, freqHi = 0.1, cbfnetwork = "ASLCBF", mask = NA, 
  labels = NA, graphdensity = 0.5, seg = NA, useglasso = NA ) {
  pixtype <- "float"
  myusage <- "usage: filterfMRIforNetworkAnalysis( timeSeriesMatrix, tr, freqLo=0.01, freqHi = 0.1, cbfnetwork=c(\"BOLD,ASLCBF,ASLBOLD\") , mask = NA,  graphdensity = 0.5 )"
  if (nargs() == 0) {
    print(myusage)
    return(NULL)
  }
  if (!is.numeric(tr) | missing(tr)) {
    print("TR parameter is missing or not numeric type - is typically between 2 and 4 , depending on your fMRI acquisition")
    print(myusage)
    return(NULL)
  }
  if (!is.numeric(freqLo) | !is.numeric(freqHi)) {
    print("freqLo/Hi is not numeric type")
    print(myusage)
    return(NULL)
  }
  if (missing(aslmat)) {
    print("Missing first (image) parameter")
    print(myusage)
    return(NULL)
  }
  freqLo <- freqLo * tr
  freqHi <- freqHi * tr
  # network analysis
  wb <- (mask > 0)  # whole brain
  leftinds <- shift(c(1:nrow(aslmat)), 1)
  rightinds <- shift(c(1:nrow(aslmat)), -1)
  oaslmat <- aslmat
  if (cbfnetwork == "ASLCBF") {
    # surround subtraction for cbf networks
    aslmat <- oaslmat - 0.5 * (oaslmat[leftinds, ] + oaslmat[rightinds, ])
    taginds <- c(1:(nrow(aslmat)/2)) * 2
    controlinds <- taginds - 1
    aslmat[controlinds, ] <- aslmat[controlinds, ] * (-1)  # ok! done w/asl specific stuff
    # plot( apply( aslmat[controlinds,],1, mean) , type='l') # should be (+) everywhere
  }
  if (cbfnetwork == "ASLBOLD") {
    # surround addition for bold networks
    aslmat <- oaslmat + 0.5 * (oaslmat[leftinds, ] + oaslmat[rightinds, ])
    # plot( apply( aslmat[controlinds,],1, mean) , type='l')
  }
  voxLo <- round((1/freqLo))  # remove anything below this (high-pass)
  voxHi <- round((1/freqHi))  # keep anything above this
  myTimeSeries <- ts(aslmat, frequency = 1/tr)
  if (nrow(myTimeSeries)%%2 > 0) {
    firsttime <- myTimeSeries[1, ]
    myTimeSeries <- myTimeSeries[2:nrow(myTimeSeries), ]
    filteredTimeSeries <- residuals(cffilter(myTimeSeries, pl = voxHi, pu = voxLo, drift = FALSE, root = FALSE, 
      type = c("trigonometric")))
    filteredTimeSeries <- rbind(firsttime, filteredTimeSeries)
    filteredTimeSeries <- ts(filteredTimeSeries, frequency = 1/tr)
  } else {
    filteredTimeSeries <- residuals(cffilter(myTimeSeries, pl = voxHi, pu = voxLo, drift = FALSE, root = FALSE, 
      type = c("trigonometric")))
  }
  vox <- round(ncol(filteredTimeSeries) * 0.5)  #  a test voxel
  spec.pgram(filteredTimeSeries[, vox], taper = 0, fast = FALSE, detrend = F, demean = F, log = "n")
  temporalvar <- apply(filteredTimeSeries, 2, var)
  wh <- which(temporalvar == 0)
  for (x in wh) {
    filteredTimeSeries[, x] <- sample(filteredTimeSeries, nrow(filteredTimeSeries))
  }
  if (!is.na(labels)) {
    # do some network thing here
    oulabels <- sort(unique(labels[labels > 0]))
    whvec <- (mask == 1)
    ulabels <- sort(unique(labels[whvec]))
    if (ulabels[1] == 0) 
      ulabels <- ulabels[2:length(ulabels)]
    labelvec <- labels[whvec]
    if (!is.na(seg)) 
      segvec <- seg[whvec] else segvec <- NA
    labmat <- matrix(data = rep(NA, length(oulabels) * nrow(filteredTimeSeries)), nrow = length(oulabels))
    nrowts <- nrow(filteredTimeSeries)
    for (mylab in ulabels) {
      if (!is.na(seg)) {
        dd <- (labelvec == mylab & segvec == 2)
      } else {
        dd <- labelvec == mylab
      }
      
      submat <- filteredTimeSeries[, dd]
      # if ( length( c( submat ) ) > nrowts ) myavg<-svd( submat )$u[,1] else myavg<-submat
      
      if (length(c(submat)) > nrowts) {
        myavg <- apply(submat, MARGIN = 1, FUN = mean)
      } else {
        myavg <- submat
      }
      
      if (length(myavg) > 0) {
        labmat[which(ulabels == mylab), ] <- myavg
      } else {
        labmat[which(ulabels == mylab), ] <- NA
      }
      
      # if ( length(myavg) > 0 ) labmat[ mylab, ]<-myavg else labmat[ mylab, ]<-NA
    }
    cormat <- cor(t(labmat), t(labmat))
    if ( !is.na(useglasso) )
    if ( useglasso > 0 ) # treat useglasso as rho
      {
#      graphdensity<-1
      if ( TRUE ) { # go with inv cov mat 
        cormat<-glasso( cormat, useglasso )$wi
        myinds<-( abs( cormat ) < 1.e-4 ) 
        cormat[  myinds ]<-cormat[ myinds ]*(-1)
      } else {
        glassomat<-glasso( cormat, useglasso )
        cormat<-glassomat$w
        cormat[ glassomat$wi == 0 ]<-0
      }
      rgdens<-( 0.5*sum(cormat>0&cormat<1)/ (0.5*ncol(cormat)*(ncol(cormat)-1)) )
      print(paste("Use Glasso",useglasso,"density",rgdens))
      diag(cormat)<-rep(1,ncol(cormat))
      }
    gmet <- makeGraph(cormat, graphdensity = graphdensity)
    return(list(filteredTimeSeries = filteredTimeSeries, mask = mask, temporalvar = temporalvar, network = labmat, 
      graph = gmet, corrmat = cormat))
  } else {
    return(list(filteredTimeSeries = filteredTimeSeries, mask = mask, temporalvar = temporalvar))
  }
  
}


makeGraph <- function(myrsfnetworkcorrs, graphdensity = 1) {
  pckg <- try(require(igraph))
  if (!pckg) {
    getPckg("igraph")
  }
  library(igraph)
  
  correlationThreshold <- 1e-06
  
  numberOfNeighbors <- nrow(myrsfnetworkcorrs)
  if (numberOfNeighbors == 0) {
    return(0)
  }
  
  myrsfnetworkcorrs[myrsfnetworkcorrs >= (1 - correlationThreshold)] <- 0
  myrsfnetworkcorrs[myrsfnetworkcorrs < correlationThreshold] <- 0
  adjmat <- 1/myrsfnetworkcorrs
  npossibleedges <- nrow(adjmat) * (nrow(adjmat) - 1)
  ndesirededges <- npossibleedges * graphdensity
  if (graphdensity < 1) {
    myord <- rev(order(adjmat))
    whichnodestoZero <- round((1 - graphdensity) * length(adjmat))
    adjmat[myord[1:whichnodestoZero]] <- 0
    # print( paste( ' 0'd ',whichnodestoZero,' of ', length( adjmat ) ) )
  }
  adjmat[adjmat == Inf] <- 0
  adjmat[adjmat > 0] <- adjmat[adjmat > 0] - 1
  adjacencyMatrix <- as.matrix(adjmat, nrow = numberOfNeighbors, ncol = nnumberOfNeighbors)
  g1 <- graph.adjacency(adjacencyMatrix, mode = c("undirected"), weighted = TRUE)
  # 
  edgeWeights <- E(g1)$weight
  # print( paste( 'Graph-Density:',graph.density( g1 ) ) )
  gmetric0 <- evcent(g1)$vector
  gmetric1 <- closeness(g1, normalized = T, weights = edgeWeights)
  gmetric2 <- page.rank(g1, weights = edgeWeights)$vector  #  
  gmetric3 <- degree(g1)
  gmetric4 <- betweenness(g1, normalized = F, weights = edgeWeights)  # 
  gmetric5 <- transitivity(g1, isolates = c("zero"), type = c("local"))  #, weights = 1/edgeWeights )
  mycommunity <- fastgreedy.community(g1)
  walktrapcomm <- walktrap.community(g1)
  return(list(mygraph = g1, centrality = gmetric0, closeness = gmetric1, pagerank = gmetric2, degree = gmetric3, 
    betweeness = gmetric4, localtransitivity = gmetric5, community = mycommunity, walktrapcomm = walktrapcomm, 
    adjacencyMatrix = adjacencyMatrix))
}



clique.community <- function(graph, k) {
  clq <- cliques(graph, min = k, max = k)
  edges <- c()
  for (i in seq_along(clq)) {
    for (j in seq_along(clq)) {
      if (length(unique(c(clq[[i]], clq[[j]]))) == k + 1) {
        edges <- c(edges, c(i, j) - 1)
      }
    }
  }
  clq.graph <- simplify(graph(edges))
  V(clq.graph)$name <- seq_len(vcount(clq.graph))
  comps <- decompose.graph(clq.graph)
  
  lapply(comps, function(x) {
    unique(unlist(clq[V(x)$name]))
  })
}


largeScaleCommunity <- function(g, mode = "all") {
  cat("Assigning initial communities...\n")
  V(g)$group <- V(g)$name
  ## random order in which vertices will be processed
  cat("Generating random order...\n")
  order <- sample(vcount(g), vcount(g))
  t <- 0
  done <- FALSE
  
  while (!done) {
    t <- t + 1
    cat("round: ", t, "\n")
    ## change to FALSE whenever a node changes groups
    done <- TRUE
    
    for (i in order) {
      ## get the neighbor group frequencies:
      group.freq <- table(V(g)[neighbors(g, i, mode = mode)]$group)
      ## pick one of the most frequent:
      new.group <- sample(names(group.freq)[group.freq == max(group.freq)], 1)
      if (done) {
        ## we are only done if new group is the same as old group
        done <- (new.group == V(g)[i]$group)
      }
      V(g)[i]$group <- new.group
    }
  }
  
  cat("Creating community-object...\n")
  comms <- list(membership = as.numeric(V(g)$group), vcount = vcount(g), algorithm = "LPA", names = V(g)$name)
  class(comms) <- "communities"
  return(comms)
} 
