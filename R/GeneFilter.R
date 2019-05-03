##### Intial marker
#' @title GeneFilter
#'
#' @param obj 
#'
#' @importFrom dpylr filter
#' @importFrom tibble rownames_to_column column_to_rownames
#' @importFrom Hmisc "%nin%
#'
#' @return
#' @export
#'
#' @examples
GeneFilter <- function(obj) {
  maxexp <- dim(obj$newdata)[2] * 0.8
  data(excludeGene)
  binadata <- obj$binadata %>% 
    as.data.frame() %>% 
    rownames_to_column('name') %>% 
    filter(name %nin% excludeGene$Gene) %>% 
    column_to_rownames('name') %>% 
    as.matrix()
  geneindex <- rowSums(binadata)
  
  obj$binadata <- binadata[names(geneindex < maxexp), ]
  obj$data <- obj$data[names(geneindex < maxexp), ]
  obj$geneSumm <- obj$geneSumm[names(geneindex < maxexp), ]
  return(obj)
}
