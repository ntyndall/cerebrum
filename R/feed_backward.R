#' @title Feed Backward
#'
#' @export


feed_backward <- function(forward, res, outputs = matrix(1)) {

  # Calculate delta in the final layer first
  # i.e. del C . sigma'

  nablaC <- forward$activations %>%
    `[[`(forward$activations %>% length) %>%
    `-`(outs)

  sigPrime <- forward$wInputs %>%
    `[[`(forward$wInputs %>% length) %>%
    cerebrum::sig(prime = T)

  deltaL <- nablaC %>%
    matrixcalc::hadamard.prod(sigPrime)

  # Set up zero'd matrix
  nablaW <- res$weights %>% cerebrum::zero_mat()
  nablaB <- res$bias %>% cerebrum::zero_mat()

  # Put in the very last nablas
  nablaB[[nablaB %>% length]] <- deltaL
  nablaW[[nablaW %>% length]] <- forward$activations[[forward$activations %>% length %>% `-`(1)]] %*% deltaL %>% t()

  # Loop over the remaining layers.. (backwards)!
  # ...

}