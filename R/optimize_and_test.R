#' @title Optimize And Test
#'
#' @export


optimize_and_test <- function(res, my.data, batchsize = 10, eta = 3.0, epochs = 10) {

  # Set up function for splitting up the data sets
  batches <- cerebrum::calc_batches(
    datRow = my.data$train %>% nrow,
    batchsize = batchsize
  )

  # Initialise result frame
  result.frame <- data.frame(stringsAsFactors = F)

  # Loop over all epochs while performing optimization
  for (i in 1:epochs) {

    # Sample the training data
    splitup <- train.data %>%
      nrow %>%
      sample

    # Subset data by batch
    new.train <- batches %>% lapply(FUN = function(x) my.data$train[x, ])
    new.labels <- batches %>% lapply(FUN = function(x) my.data$labels.tr[x])

    # Iterations of batches
    batchIter <- my.data$train %>%
      nrow %>%
      `/`(batchsize) %>%
      floor

    # Set up progress bar
    pb <- utils::txtProgressBar(
      min = 0,
      max = batchIter,
      style = 3
    )

    # Gradient descent for all batches
    for (k in 1:batchIter) {
      res %<>% cerebrum::s_grad_desc(
        xs = new.train[[k]],
        ys = new.labels[[k]],
        eta = eta
      )

      # Update progress bar
      pb %>% utils::setTxtProgressBar(k)
    }

    # Now check out the test data (it doesnt need shuffled!)
    totsum <- 0
    for (k in 1:(my.data$test %>% nrow)) {

      # Calculate the highest feed forward activation
      my_res <- my.data$test[k, ] %>%
        cerebrum::feed_forward(
          res = res
        ) %>%
        `[[`("activations") %>%
        `[[`(res$layers) %>%
        as.double %>%
        which.max

      # If it matches then increment the count!
      if (my.data$labels.te[[k]][my_res, 1] == 1) totsum %<>% `+`(1)
    }

    # Percentage
    totsum %<>%
      `/`(my.data$test %>% nrow) %>%
      round(digits = 4) %>%
      `*`(100)

    # Print to screen
    cat(paste0(" \n  |= Epoch ", i, " : ", totsum, "\n\n"))

    # Append to result frame
    result.frame %<>% rbind(
      data.frame(
        epoch = i,
        percentage = totsum,
        stringsAsFactors = F
      )
    )
  }

  # Return the results frame
  return(result.frame)
}
