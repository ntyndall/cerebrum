#' @title Calculate Batches
#'
#' @export


calc_batches <- function(datRows, batchsize) {
  # Calculate counts + remainders
  batchLen <- datRows %>% `/`(batchsize) %>% floor
  remain <- datRows %% batchsize

  if (remain > 0) remain <- (batchLen - remain + 1):batchLen
  accumulator <- 0
  batches <- list()
  for (x in 1:batchLen) {

    trainsize <- if (x %in% remain) 1:(batchsize + 1) else 1:batchsize

    batches %<>% c(trainsize %>% `+`(accumulator) %>% list)
    if (x %in% remain) accumulator %<>% `+`(1)

    accumulator %<>% `+`(batchsize)
  }

  # Return the list of indexes
  return(batches)
}
