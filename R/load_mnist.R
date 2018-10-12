#' @title Load MNIST
#'
#' @details Original data source can be found at \code{http://yann.lecun.com/exdb/mnist/}
#'
#' @export


load_mnist <- function(dirName = "image-data", dat = "train-images.idx3-ubyte", labels = "train-labels.idx1-ubyte") {

  # Set up function for checking for slashes in input
  split_up <- function(x) x %>% strsplit(split = "") %>% purrr::flatten_chr()

  # Fix dirName
  if (dirName %>% split_up() %>% utils::tail(1) %>% `==`("/")) {
    dirName %<>% split_up() %>% utils::head(-1) %>% paste(collapse = "")
  }

  # Fix dataset names
  # ...

  # Initialise empty data set
  data.set <- list()

  # Open up the data file
  f1 <- getwd() %>%
    paste0("/", dirName, "/", dat) %>%
    file(open = "rb")

  # Read header information
  read_item <- function(conn) conn %>% readBin(what = 'integer', n = 1, size = 4, endian = "big")

  # Get header details
  headInfo <- lapply(
    X = 1:4,
    FUN = function(x) f1 %>% read_item()
  ) %>%
    `[`(2:4)
  names(headInfo) <- c("num", "rows", "cols")

  # Append number to data set
  data.set$num <- headInfo$num

  cat(crayon::green("Loading image data set \n"))
  data.set$dat <- f1 %>%
    readBin(
      what = "integer",
      n = headInfo %>% as.double %>% prod,
      size = 1,
      signed = FALSE
    ) %>%
    matrix(
      ncol = headInfo$rows * headInfo$cols,
      byrow = TRUE
    )

  # Close the main file
  close(f1)

  # Open up the labels file
  f2 <- getwd() %>%
    paste0("/", dirName, "/", labels) %>%
    file(open = "rb")

  cat(crayon::green("Loading label data set \n"))

  # Read header information
  labelNum <- lapply(
    X = 1:2,
    FUN = function(x) f2 %>% read_item()
  ) %>%
    `[[`(2)

  # Read and append actual labels to data set
  data.set$labels <- f2 %>%
    readBin(
      what = "integer",
      n = labelNum,
      size = 1,
      signed = FALSE
    )

  # Close the labels file
  close(f2)

  # Return data set back
  return(data.set)
}
