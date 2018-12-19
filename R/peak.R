#### peak of density
#' @title peak
#'
#' @param pp 
#'
#' @return
#' @export
#'
#' @examples
peak <- function(pp) {
  y <- pp$y
  x <- pp$x
  Hpeak <- c()
  for (i in 2:(length(y) - 1)) {
    if (y[i] >= y[i - 1] & y[i] > y[i + 1]) {
      Hpeak <- rbind(Hpeak, c(x[i], y[i]))
    }
  }
  if (length(Hpeak) == 0) {
    Hpeak <- rbind(Hpeak, c(0, 0))
  }
  Hpeak <- as.data.frame(Hpeak)
  names(Hpeak) <- c("H", "Density")
  return(Hpeak)
}
