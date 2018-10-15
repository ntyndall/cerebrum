library(magrittr)
library(cerebrum)

# First step is to load up the image datasets
data.set <- cerebrum::load_mnist()

# Convert labels to matrix
allLabs <- data.set$labels %>%
  unique %>%
  sort

# Create empty output matrices
empt <- matrix(0 %>% rep(allLabs %>% length))

# Update the actual labels
newLabels <- lapply(
  X = data.set$labels,
  FUN = function(x) {
    current <- empt
    current[x + 1, ] <- 1
    current
  }
)

# Generate random samples
reorder <- data.set$labels %>%
  length %>%
  sample

# Now re-order data AND labels
newLabels %<>% `[`(reorder)
data.set$dat <- data.set$dat[reorder, ] / 256

# Split up training + testing
train.data <- data.set$dat[1:50000, ]
train.labels <- newLabels[1:50000]
test.data <- data.set$dat[50001:60000, ]
test.labels <- newLabels[50001:60000]

# Initialise a neural network with a single hidden layer (with 5 neurons)!
res <- c(data.set$dat %>% ncol, 30, allLabs %>% length) %>%
  cerebrum::nnetwork()

# Define standard variables...
batchsize <- 10
epochs <- 10
eta <- 3.0

# Optimize weights + biases
for (i in 1:epochs) {

  # Sample the training data
  splitup <- train.data %>%
    nrow %>%
    sample

  batchContrib <- 1:(train.data %>% nrow %>% `/`(batchsize))

  new.train <- lapply(
    X = batchContrib,
    FUN = function(x) {
      train.data[(((x - 1) * batchsize) + 1):(x * batchsize), ]
    }
  )

  new.labels <- lapply(
    X = batchContrib,
    FUN = function(x) {
      train.labels[(((x - 1) * batchsize) + 1):(x * batchsize)]
    }
  )

  # Set up progress bar
  pb <- utils::txtProgressBar(
    min = 0,
    max = train.data %>% nrow %>% `/`(batchsize),
    style = 3
  )

  # Gradient descent for all batches
  for (k in 1:(train.data %>% nrow %>% `/`(batchsize))) {
    res %<>% cerebrum::s_grad_desc(
      xs = new.train[[k]],
      ys = new.labels[[k]],
      eta = eta
    )
    utils::setTxtProgressBar(pb, value = k)
  }

  # Now check out the test data (it doesnt need shuffled!)
  totsum <- 0
  for (k in 1:(test.data %>% nrow)) {

  # Calculate the highest feed forward activation
  my_res <- test.data[k, ] %>%
    cerebrum::feed_forward(
      res = res
    ) %>%
    `[[`("activations") %>%
    `[[`(3) %>%
    as.double %>%
    which.max

   # If it matches then increment the count!
   if (test.labels[[k]][my_res, 1] == 1) totsum %<>% `+`(1)
  }

  cat(paste0("Epoch ", i, " : ", totsum / (test.data %>% nrow)), "\n")

}
