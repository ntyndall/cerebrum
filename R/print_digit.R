#' @title Print Digit
#'
#' @export


print_digit <- function() {

  # Load in MNIST data
  data.set <- cerebrum::load_mnist()

  # Ask for a value in the data set
  active <- TRUE
  while (active) {
    cat(crayon::green(paste0("\nPick a number between 1 and ", data.set$dat %>% nrow, ": (or type `q` to quit).\n")))
    input <- readLines(con = "stdin", n = 1)

    # Check input
    if (input %>% `!=`("q")) {
      single.row <- data.set$dat[input %>% as.integer, ]
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
    } else {
      active <- FALSE
    }
  }
}
