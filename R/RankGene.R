#' @title RankGene
#'
#' @param kk 
#' @param k 
#' @param HamD 
#' @param geneName 
#' @param n 
#'
#' @return
#' @export
#'
#' @examples
RankGene <- function(kk, k, HamD, geneName, n) {
  x <- HamD[kk, ]
  xrank <- rank(-x)
  MNNgene <- geneName[xrank <= k & x > n]
  return(MNNgene)
}
