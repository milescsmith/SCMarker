#' @title getMNN
#'
#' @param HamD 
#' @param genename 
#' @param k 
#' @param n 
#'
#' @return
#' @export
#'
#' @examples
getMNN <- function(HamD, genename, k, n) {
  MNNgene <- lapply(1:nrow(HamD), 
                    RankGene, 
                    k = k, 
                    HamD = HamD, 
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
