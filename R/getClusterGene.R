##### cluster-specific marker
#' @title getClusterGene
#'
#' @param obj 
#' @param method 
#'
#' @importFrom SC3 get_marker_genes
#' 
#' @return
#' @export
#'
#' @examples
getClusterGene <- function(obj, method) {
  marker <- obj$marker
  if (method == "Seurat") {
    seuratCluster <- obj$seuratCluster
    gene <- get_marker_genes(obj$rawdata[marker, names(seuratCluster)], seuratCluster)
    gene$gene <- marker
    obj$clustergene <- gene
    obj$method <- method
  } else if (method == "dbscan") {
    dbscanCluster <- obj$dbscanCluster
    gene <- get_marker_genes(obj$rawdata[marker, names(dbscanCluster)], dbscanCluster)
    gene$gene <- marker
    obj$clustergene <- gene
    obj$method <- method
  }
  return(obj)
}
