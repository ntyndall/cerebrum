#' @title Optimize And Test
#'
#' @export


optimize_and_test <- function(res, train.data, train.labels, test.data, test.labels,
                              batchsize = 10, eta = 3.0, epochs = 10) {

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
        `[[`(res$layers) %>%
        as.double %>%
        which.max

      # If it matches then increment the count!
      if (test.labels[[k]][my_res, 1] == 1) totsum %<>% `+`(1)
    }

    cat(paste0(" \n ## Epoch ", i, " : ", totsum / (test.data %>% nrow)), "\n")
  }
}
