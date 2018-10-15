#' @title Stochastic Gradient Descent
#'
#' @export


s_grad_desc <- function(res, xs, ys, size, eta = 0.05) {

  # Set up empty matrices
  nabW <- res$weights %>% cerebrum::zero_mat()
  nabB <- res$bias %>% cerebrum::zero_mat()
  nabLen <- 1:(nabW %>% length)

  # Try to loop over the training data - and use back propagation somewhere!
  for (i in 1:(xs %>% nrow)) {
    backwards <- res %>%
      cerebrum::backprop(
        x = xs,
        y = ys
      )

    # Update nablaW
    nabW <- lapply(
      X = nabLen,
      FUN = function(x) nabW[[x]] %>% `+`(backward$nablaW[[x]])
    )

    # Update nablaB
    nabB <- lapply(
      X = nabLen,
      FUN = function(x) nabB[[x]] %>% `+`(backward$nablaB[[x]])
    )
  }

  fact <- eta %>% `/`(xs %>% nrow)

  # Finally, update the weights + biases...
  res$weights <- lapply(
    X = nabLen,
    FUN = function(x) res$weights[[x]] %>% `-`(fact * nabW[[x]])
  )

  res$bias <- lapply(
    X = nabLen,
    FUN = function(x) res$bias[[x]] %>% `-`(fact * nabB[[x]])
  )

  # Return res back with updated weights + biases
  return(res)
}
