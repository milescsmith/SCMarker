###### Inial cluster
#' @title Bina
#'
#' @param data 
#' @param cutoff 
#'
#' @return
#' @export
#'
#' @examples
Bina <- function(data, cutoff = 2) {
  data[data < cutoff] <- 0
  data[data >= cutoff] <- 1
  return(data)
}
