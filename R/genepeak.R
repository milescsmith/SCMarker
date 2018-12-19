### number of peak
#' @genepeak
#'
#' @param x 
#' @param width 
#'
#' @return
#' @export
#'
#' @examples
genepeak <- function(x, width = 1) {
  if (length(x[x != 0]) > 1) {
    pp <- density(x[x != 0], width = width)
    Hpeak <- peak(pp)
    return(dim(Hpeak)[1])
  } else {
    return(0)
  }
}
