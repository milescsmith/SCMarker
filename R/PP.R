#' @title PP
#'
#' @param i 
#' @param index 
#' @param MNNgene 
#' @param k 
#' @param geneName 
#'
#' @return
#' @export
#'
#' @examples
PP <- function(i, index, MNNgene, k, geneName) {
  if (geneName[k] %in% MNNgene[[index[i]]]) {
    return(geneName[index[i]])
  }
}
