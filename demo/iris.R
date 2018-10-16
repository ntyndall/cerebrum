#
# Test out the iris data set on my
# neural network implementation
#

full.data <- datasets::iris

# Coerce the data frame to a matrix
d.set <- full.data %>%
  dplyr::select(-Species) %>%
  unlist %>%
  as.numeric %>%
  matrix(nrow = full.data %>% nrow)

# Create a list useful for updating the dataset
data.set <- list(
  num = full.data %>% nrow,
  dat = d.set,
  labels = full.data$Species
)

# Create a scaled + named data set for optimizing neural network
my.data <- data.set %>%
  cerebrum::update_data()


# Initialise a neural network with a single hidden layer (with 5 neurons)!
res <- c(my.data$train %>% ncol, 10, my.data$labels.tr[[1]] %>% nrow) %>%
  cerebrum::nnetwork()

# Optimize weights + biases
res %>% cerebrum::optimize_and_test(
  train.data = my.data$train,
  train.labels = my.data$labels.tr,
  test.data = my.data$test,
  test.labels = my.data$labels.te,
  batchsize = 10,
  eta = 3.0,
  epochs = 40
)
