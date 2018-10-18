#' @title Update Data
#'
#' @export


update_data <- function(data.set, splitPer = 0.75) {

  # Convert labels to numeric
  data.set$labels %<>% as.numeric

  # Convert labels to matrix
  allLabs <- data.set$labels %>%
    unique %>%
    sort

  # Make sure that labels start at 1 (for indexing purposes)
  data.set$labels %<>% `+`(1 %>% `-`(allLabs[1]))

  # Create empty output matrices
  empt <- matrix(0 %>% rep(allLabs %>% length))

  # Update the actual labels
  newLabels <- lapply(
    X = data.set$labels,
    FUN = function(x) {
      current <- empt
      current[x, ] <- 1
      current
    }
  )

  # Generate random samples
  reorder <- data.set$labels %>%
    length %>%
    sample

  # Reorder them
  newLabels %<>% `[`(reorder)
  data.set$dat <- data.set$dat[reorder, ]

  # Normalise every column..
  maxvals <- data.set$dat %>% apply(MARGIN = 2, max)
  for (i in 1:(data.set$dat %>% ncol)) {
    if (maxvals[i] %>% `!=`(0)) data.set$dat[ , i] %<>% `/`(maxvals[i])
  }

  # Get values for splitting the data set
  trNum <- data.set$num %>%
    `*`(splitPer) %>%
    floor

  # Return the data sets
  return(
    list(
      train = data.set$dat[1:trNum, ],
      labels.tr = newLabels[1:trNum],
      test = data.set$dat[(trNum + 1):data.set$num, ],
      labels.te = newLabels[(trNum + 1):data.set$num]
    )
  )
}
