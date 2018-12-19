#' @title getMarker
#'
#' @param obj 
#' @param k 
#' @param n 
#'
#' @return
#' @export
#'
#' @examples
getMarker <- function(obj, k = 300, n = 30) {
  data <- obj$newdata
  binadata <- obj$binadata
  genename <- rownames(binadata)
  geneindex <- rowSums(binadata)
  HamD <- tcrossprod(binadata)
  diag(HamD) <- 0
  HamDD <- tcrossprod((1 - binadata), binadata)
  MNNmarker <- getMNN(HamD = HamD, 
                      genename = genename, 
                      k = k, 
                      n = n)
  MENmarker <- getMEN(HamD = HamDD, 
                      genename = genename, 
                      k = k, 
                      n = n)
  obj$marker <- union(MNNmarker, 
                      MENmarker)
  return(obj)
}
