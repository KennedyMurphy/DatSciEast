require(ggplot2)
require(data.table)

#' Read in the data.
wine <- read.csv(file = 'Data/Data Smart Ch2.csv', stringsAsFactors = FALSE)
deals <- wine[, colnames(wine)[1:7]]


#' Create the wine matrix.
wine.matrix <- as.matrix(wine[, 8:ncol(wine)])
rownames(wine.matrix) <- wine$Offer
wine.matrix[is.na(wine.matrix)] <- 0

#' Calculate the Euclidean distance between every customer
euclidean <- as.matrix(dist(t(wine.matrix), method = "euclidean"))

max.clusters <- round((ncol(wine.matrix) - 1) / 2)

#' Run through 100 * (max.clusters - 1) K-Means simulation for  the
#' elbow method
var.explained <- rbindlist(lapply(1:100, FUN = function(y) {
  writeLines(paste0("Elbow analysis number ", y))
  
  #' Run through each cluster
  result <- rbindlist(lapply(2:max.clusters, FUN = function(x) {
    km <- kmeans(t(wine.matrix), centers = x, iter.max=50, nstart=10)
    
    return(data.frame(clusters = x, explained = km$betweenss / km$totss))
  }))
  
  return(result)
}))

#' Run K-Means
k <- kmeans(t(wine.matrix), centers = 4)

center.dist <- as.matrix(dist(rbind(t(wine.matrix), k$centers), 
                              method = "euclidean"))
center.dist <- melt(center.dist[colnames(wine.matrix),
                                c("1", "2", "3", "4")])
center.dist$range <- factor(floor(center.dist$value))

#' Combine the cluster centers with the deals data
centers <- cbind(deals, t(k$centers))
centers <- melt(centers, 
                id.vars = colnames(centers)[1:7], 
                measure.vars = as.character(1:4), 
                variable.name = "Cluster",
                value.name = "Taken")
centers$Taken <- ceiling(centers$Taken)