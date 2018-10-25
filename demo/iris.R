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
irisnn <- c(my.data$train %>% ncol, 10, my.data$labels.tr[[1]] %>% nrow) %>%
  cerebrum::nnetwork()

# Optimize weights + biases
result.frame <- irisnn %>%
  cerebrum::optimize_and_test(
    my.data = my.data,
    batchsize = 10,
    eta = 3.0,
    epochs = 20
  )
