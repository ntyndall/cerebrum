#' @title Zero Mat
#'
#' @export


zero_mat <- function(x) {
  return(
    lapply(
      X = x,
      FUN = function(y) {
        y[] <- 0
        return(y)
      }
    )
  )
}
