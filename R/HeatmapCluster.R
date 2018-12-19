#' @title HeatmapCluster
#'
#' @param obj 
#' @param top 
#' @param scale 
#'
#' @importFrom pheatmap pheatmap
#' @importFrom grDevices rainbow
#' 
#' @return
#' @export
#'
#' @examples
HeatmapCluster <- function(obj, 
                           top, 
                           scale = "none",
                           ...) {
  feature <- obj$clustergene
  topmarker <- as.data.frame(feature %>% group_by(clusts) %>% top_n(top, auroc))
  topmarker <- topmarker[order(topmarker$clusts), ]
  method <- obj$method
  if (method == "Seurat") {
    cluster <- obj$seuratCluster
  } else if (method == "dbscan") {
    cluster <- obj$dbscanCluster
  }
  clustertype <- levels(factor(cluster))
  genemean <- c()
  for (i in clustertype) {
    cell <- names(cluster[cluster == i])
    subdata <- obj$rawdata[topmarker$gene, cell]
    genemean <- cbind(genemean, apply(subdata, 1, mean))
  }
  rownames(genemean) <- topmarker$gene
  colnames(genemean) <- clustertype

  mat_col <- data.frame(cluster = levels(factor(cluster)))
  rownames(mat_col) <- levels(factor(cluster))
  mat_colors <- list(cluster = rainbow(length(clustertype)))
  names(mat_colors$cluster) <- levels(factor(cluster))
  pheatmap(genemean, cluster_cols = F, scale = scale, cluster_rows = F, show_colnames = F, annotation_col = mat_col, annotation_colors = mat_colors, ...)
}
