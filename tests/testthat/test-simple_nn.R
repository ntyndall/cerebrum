

# Set up a simple neural network with the following nodes in each layer
# c(2, 3, 4, 2)
testNN <- list(
  layers = 4,
  bias = list(
    matrix(c(0.4, 0.8, 0.1)),
    matrix(c(0.2, 0.4, 0.1, 0.8)),
    matrix(c(0.1, 0.6))
  ),
  weights = list(
    matrix(c(0.2, 0.1, 0.3, 0.4, 0.8, 0.1), nrow = 3),
    matrix(c(0.3, 0.1, 0.9, 0.7, 0.6, 0.1, 0.2, 0.3, 0.5, 0.4, 0.2, 0.8), nrow = 4),
    matrix(c(0.3, 0.6, 0.1, 0.9, 0.4, 0.7, 0.2, 0.7), nrow = 2)
  )
)
