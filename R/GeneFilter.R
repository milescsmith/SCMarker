##### Intial marker
#' @title GeneFilter
#'
#' @param obj 
#'
#' @return
#' @export
#'
#' @examples
GeneFilter <- function(obj) {
  maxexp <- dim(obj$newdata)[2] * 0.8
  data(excludeGene)
  excludeGene <- as.character(excludeGene[, 1])
  geneSumm <- obj$geneSumm
  index <- match(geneSumm$gene, excludeGene)
  geneSumm <- geneSumm[is.na(index), ]
  data <- obj$newdata
  index <- match(geneSumm$gene, rownames(data))
  data <- data[index, ]
  binadata <- obj$binadata
  binadata <- binadata[index, ]
  geneindex <- rowSums(binadata)
  binadata <- binadata[geneindex < maxexp, ]
  data <- data[geneindex < maxexp, ]
  geneSumm <- geneSumm[geneindex < maxexp, ]
  obj$newdata <- data
  obj$geneSumm <- geneSumm
  obj$binadata <- as.matrix(binadata)
  return(obj)
}
