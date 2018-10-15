#' @title Backpropagation
#'
#' @export


backprop <- function(res, x, y) {

  # Get feed forward information
  forward <- x %>%
    cerebrum::feed_forward(
      res = res
    )

  # Now the delta's (weights + biases)
  backward <- forward %>%
    cerebrum::feed_backward(
      res = res,
      outputs = y
    )

  # Return backwards information with delta nabla's
  return(backward)
}
