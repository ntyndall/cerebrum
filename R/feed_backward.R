#' @title Feed Backward
#'
#' @export


feed_backward <- function(forward, res, outputs = matrix(1)) {

  # Function for retrieving the last element of a list
  last_element <- function(x) x %>% utils::tail(1) %>% `[[`(1)

  # Calculate delta in the final layer first
  # i.e. del C . sigma'
  nablaC <- forward$activations %>%
    last_element() %>%
    `-`(outputs)

  sigPrime <- forward$wInputs %>%
    last_element() %>%
    cerebrum::sig(prime = T)

  deltaL <- nablaC %>%
    matrixcalc::hadamard.prod(sigPrime)

  # Set up zero'd matrix
  nablaW <- res$weights %>% cerebrum::zero_mat()
  nablaB <- res$bias %>% cerebrum::zero_mat()

  # Useful function for calculating
  nab_w <- function(del, act, val) del %*% (act %>% `[[`(val) %>% t)

  # Put in the very last nablas
  nablaB[[nablaB %>% length]] <- deltaL
  nablaW[[nablaW %>% length]] <- nab_w(
    del = deltaL,
    act = forward$activations,
    val = forward$activations %>% length %>% `-`(1)
  )

  # Loop over the remaining layers.. (backwards)!
  # We know that the errors are related by adjacent layers
  for (i in (res$layers - 2):1) {
    # Calculate sigma prime
    sprime <- forward$wInputs[[i]] %>%
      sig(prime = T)

    # Calculate the new delta value
    nudel <- res$weights[[i + 1]] %>%
      t %*%
      deltaL %>%
      matrixcalc::hadamard.prod(sprime)

    # Update the b and w tilde values
    nablaB[[i]] <- nudel
    nablaW[[i]] <- nab_w(
      del = nudel,
      act = forward$activations,
      val = i
    )

    # Use this delta in the next iteration
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
