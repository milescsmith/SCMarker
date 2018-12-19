#' @title HeatmapCell
#'
#' @param obj 
#' @param top 
#'
#' @importFrom pheatmap pheatmap
#'
#' @return
#' @export
#'
#' @examples
HeatmapCell <- function(obj, top) {
  feature <- obj$clustergene
  topmarker <- as.data.frame(feature %>% group_by(clusts) %>% top_n(top, auroc))
  topmarker <- topmarker[order(topmarker$clusts), ]
  method <- obj$method
  if (method == "Seurat") {
    cluster <- obj$seuratCluster
    cluster <- cluster[order(cluster)]
  } else if (method == "dbscan") {
    cluster <- obj$dbscanCluster
    cluster <- cluster[order(cluster)]
  }
  clustertype <- unique(cluster)
  data <- obj$rawdata[topmarker$gene, names(cluster)]
  clustercount <- table(cluster)
  gapIndex <- c()
  for (i in 2:(length(clustertype) - 1)) {
    gapIndex <- c(gapIndex, sum(clustercount[1:i]))
  }
  pheatmap(data, cluster_cols = F, cluster_rows = F, show_colnames = F, gaps_col = gapIndex)
}
