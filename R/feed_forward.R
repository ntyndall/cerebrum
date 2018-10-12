#' @title Feed Forward
#'
#' @export


feed_forward <- function(x = matrix(c(1, 2)), res) {

  # Calculate feed forward activation layer values sigma(w . a + b)
  wInputs <- list()
  activations <- list(x)
  for (i in 1:(res$layers - 1)) {
    weightedInput <- res$weights[[i]] %*% x %>% `+`(res$bias[[i]])
    wInputs %<>% c(weightedInput %>% list)
    x <- weightedInput %>% cerebrum::sig()
    activations %<>% c(x %>% list)
  }

  return(
    list(
      activations = activations,
      wInputs = wInputs
    )
  )
}
