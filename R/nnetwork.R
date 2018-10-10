#' @title nnetwork
#'
#' @importFrom magrittr %>% %<>%
#'
#' @export

nnetwork <- function(layerInfo) {

  # Get the number of layers
  layers <- layerInfo %>% length

  return(
    list(
      layers = layers,
      bias = lapply(
        X = layerInfo[2:layers],
        FUN= function(x) x %>% stats::rnorm() %>% matrix
      ),
      weights = lapply(
        X = 1:(layers - 1),
        FUN = function(x) layerInfo[x] %>% `*`(layerInfo[x + 1]) %>% stats::rnorm() %>% matrix(ncol = layerInfo[x])
      )
    )
  )
}
