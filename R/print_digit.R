#' @title Print Digit
#'
#' @export


print_digit <- function(single.row) {

  #
  maxVal <- single.row %>% max

  for (i in 1:(single.row %>% length)) {
    cval <- single.row[i]
    cat(
      if (cval < 10) {
        crayon::cyan(paste0("   ", cval %>% as.character))
      } else if (cval > 9 && cval < 100) {
        crayon::yellow(paste0("  ", cval %>% as.character))
      } else {
        crayon::red(paste0(" ", cval %>% as.character))
      }
    )

    # New line!
    if (i %% 28 == 0) cat("\n")
  }
}
