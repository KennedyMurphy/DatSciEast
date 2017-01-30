library(igraph)
library(data.table)
library(network)
library(sna)
library(ggplot2)
library(ggnet)

#' Set seed so you can reproduce the graphs
set.seed(10312016)

#' Read in the data.
wine <- read.csv(file = 'Data/Data Smart Ch2.csv', stringsAsFactors = FALSE)
deals <- wine[, colnames(wine)[1:7]]

#' Create the wine matrix.
wineMatrix <- as.matrix(wine[, 8:ncol(wine)])
rownames(wineMatrix) <- wine$Offer
wineMatrix[is.na(wineMatrix)] <- 0

dimnames(wineMatrix) <- list(Offer = paste("Offer", rownames(wineMatrix)),
                             Customer = colnames(wineMatrix))

#' Create an offer network
offerNetwork <- data.frame(wineMatrix,
                           row.names = rownames(wineMatrix))
offerNetwork <- network(offerNetwork,
                        matrix.type = "bipartite",
                        ignore.eval = FALSE,
                        names.eval  = "weights")

#' Create the network graph
col <- c("actor" = "gold", "event" = "grey")
ggnet2(offerNetwork, color = "mode", palette = col, label = TRUE) +
  theme(legend.position = "none")

#' Create the Varietal matrix
varietalMatrix <- as.matrix(wine[, 8:ncol(wine)])
rownames(varietalMatrix) <- wine$Varietal
varietalMatrix[is.na(varietalMatrix)] <- 0

#' Loop over each varietal and aggregate the data
sample <- colSums(varietalMatrix[rownames(varietalMatrix) == "Merlot", ,drop = FALSE])
varietalMatrix <- vapply(unique(deals$Varietal), FUN = function(v) {
  return(colSums(varietalMatrix[rownames(varietalMatrix) == v, ,drop = FALSE]))
}, FUN.VALUE = sample)

varietalMatrix <- t(varietalMatrix)

varietalNetwork <- data.frame(varietalMatrix,
                          row.names = rownames(varietalMatrix))
varietalNetwork <- network(varietalNetwork,
                       matrix.type = "bipartite",
                       ignore.eval = FALSE,
                       names.eval  = "weights")

#' Create the varietal network graph
ggnet2(varietalNetwork, color = "mode", palette = col, label = TRUE) +
  theme(legend.position = "none")

#' Create the origin matrix
originMatrix <- as.matrix(wine[, 8:ncol(wine)])
rownames(originMatrix) <- wine$Origin
originMatrix[is.na(originMatrix)] <- 0

#' Loop over each varietal and aggregate the data
sample <- colSums(originMatrix[rownames(originMatrix) == "France", ,drop = FALSE])
originMatrix <- vapply(unique(deals$Origin), FUN = function(o) {
  return(colSums(originMatrix[rownames(originMatrix) == o, ,drop = FALSE]))
}, FUN.VALUE = sample)

originMatrix <- t(originMatrix)

originNetwork <- data.frame(originMatrix,
                              row.names = rownames(originMatrix))
originNetwork <- network(originNetwork,
                           matrix.type = "bipartite",
                           ignore.eval = FALSE,
                           names.eval  = "weights")

#' Create the varietal network graph
ggnet2(originNetwork, color = "mode", palette = col, label = TRUE) +
  theme(legend.position = "none")


#' Plot the K-means clusters
k <- kmeans(t(wineMatrix), centers = 4)

#' similarity matrix will show how many deals the customers in each
#' row have in common with customers in each column
similarity <- t(wineMatrix) %*% wineMatrix
dimnames(similarity) <- list(Customer1 = rownames(similarity),
                             Customer2 = colnames(similarity))

clusterNetwork <- data.table(melt(similarity))

clusterNetwork <- clusterNetwork[Customer1 != Customer2 & value > 0] 

#' Run network on the clusters. This takes a while because there are so 
#' many edges.
clusterNetwork <- network(clusterNetwork[, c("Customer1", "Customer2")], directed = TRUE)

#' Create cluster types
clusterNetwork %v% "cluster" <- k$cluster[network.vertex.names(clusterNetwork)]

ggnet2(clusterNetwork, color = "cluster",
       size = 4, palette = "Set1",
       edge.alpha = 0.25, mode = "fruchtermanreingold",
       edge.color = c("color", "grey50"),
       color.legend = "Cluster ID", label = TRUE) +
  theme(legend.position = "bottom")

#' Plot the K-means clusters
k5 <- kmeans(t(wineMatrix), centers = 5)

clusterNetwork <- data.table(melt(similarity))

clusterNetwork <- clusterNetwork[Customer1 != Customer2 & value > 0] 

#' Run network on the clusters. This takes a while because there are so 
#' many edges.
clusterNetwork <- network(clusterNetwork[, c("Customer1", "Customer2")], directed = TRUE)

#' Create cluster types
clusterNetwork %v% "cluster" <- k5$cluster[network.vertex.names(clusterNetwork)]

ggnet2(clusterNetwork, color = "cluster",
       size = 4, palette = "Set1",
       edge.alpha = 0.25, mode = "fruchtermanreingold",
       edge.color = c("color", "grey50"),
       color.legend = "Cluster ID", label = TRUE) +
  theme(legend.position = "bottom")

