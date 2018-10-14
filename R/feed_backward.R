#' @title Feed Backward
#'
#' @export


feed_backward <- function(forward, res, outputs = matrix(1)) {

  # Calculate delta in the final layer first
  # i.e. del C . sigma'
  nablaC <- forward$activations %>%
    `[[`(forward$activations %>% length) %>%
    `-`(outputs)

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
  nablaW[[nablaW %>% length]] <- deltaL %*% (forward$activations[[forward$activations %>% length %>% `-`(1)]] %>% t)

  # Loop over the remaining layers.. (backwards)!
  # We know that the errors are related by adjacent layers
  for (i in (res$layers - 2):1) {
    zval <- forward$wInputs[[i]]
    sprime <- zval %>% cerebrum::sig(prime = T)
    nudel <- res$weights[[i + 1]] %>% t %*% deltaL %>% matrixcalc::hadamard.prod(sprime)
    nablaB[[i]] <- nudel
    nablaW[[i]] <- nudel %*% (forward$activations[[i]] %>% t)
    deltaL <- nudel
  }

  # Return the nabla's back
  return(
    list(
      nablaB = nablaB,
      nablaW = nablaW
    )
  )
}
