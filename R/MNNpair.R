#' @title MNNpair
#'
#' @param k 
#' @param MNNgene 
#' @param geneName 
#'
#' @return
#' @export
#'
#' @examples
MNNpair <- function(k, MNNgene, geneName) {
  subgene <- MNNgene[[k]]
  index <- match(subgene, geneName)

  if (length(index[!is.na(index)]) > 0) {
    a <- do.call(cbind, 
                 lapply(1:length(index), 
                        PP, 
                        index = index, 
                        MNNgene = MNNgene, 
                        k = k, 
                        geneName = geneName))
    if (!is.null(a)) {
      return(a)
    }
  }
}
