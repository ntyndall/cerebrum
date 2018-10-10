#' @title Sig
#'
#' @export


sig <- function(z, prime = F) {

  # Calculate the exponential part
  expon <- z %>% `*`(-1) %>% exp

  # Return sigmoid (or the derivative)
  return(if (prime) expon %>% `/`((1 + expon)^2) else 1 %>% `/`(1 + expon))
}
