library(magrittr)
library(cerebrum)


# First step is to load up the image datasets
data.set <- cerebrum::load_mnist()

# Get the right structure for the data
my.data <- data.set %>%
  cerebrum::update_data()

# Initialise a neural network with a single hidden layer (with 5 neurons)!
res <- c(my.data$train %>% ncol, 30, my.data$labels.tr[[1]] %>% nrow) %>%
  cerebrum::nnetwork()

# Test data
result.frame <- res %>%
  cerebrum::optimize_and_test(
    my.data = my.data
  )
