# TODO: Add comment
#
# Author: FWang9
###############################################################################
#### preprocess data
## count expression cell or gene
#' @title genecount
#'
#' @param k 
#' @param data 
#' @param index 
#'
#' @return
#' @export
#'
#' @examples
genecount <- function(k, data, index) {
  if (index == "row") {
    x <- data[k, ]
  } else {
    x <- data[, k]
  }
  return(sum(x != 0))
}
