#' @title getMEN
#'
#' @param HamDD 
#' @param genename 
#' @param k 
#' @param n 
#'
#' @return
#' @export
#'
#' @examples
getMEN <- function(HamDD, genename, k, n) {
  MNNgene <- lapply(1:nrow(HamDD), 
                    RankGene, 
                    k = k, 
                    HamD = HamDD, 
                    geneName = genename, 
                    n = n)
  genePair <- unique(as.character(do.call(cbind, 
                                          lapply(1:length(MNNgene), 
                                                 MNNpair, 
                                                 MNNgene = MNNgene, 
                                                 geneName = genename)
                                          )
                                  )
                     )
  return(genePair)
}
