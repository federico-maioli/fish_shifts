#install.packages("FNN")
library(FNN)

# g is grid 
# d is data

exclude_too_far <- function(g1, g2, d1, d2, dist) {
  # Combine grid points and data points into matrices
  grid_points <- cbind(g1, g2)
  data_points <- cbind(d1, d2)
  
  # Find the nearest neighbor distance for each grid point
  nn_distances <- get.knnx(data_points, grid_points, k = 1)$nn.dist
  
  # Return a logical vector indicating if the grid node is too far
  nn_distances > dist
}

# Example usage:
# g1 <- c(0.1, 0.5, 0.9)
# g2 <- c(0.1, 0.5, 0.9)
# d1 <- c(0.2, 0.6)
# d2 <- c(0.2, 0.6)
# dist <- 0.2
# 
# exclude_too_far(g1, g2, d1, d2, dist)
