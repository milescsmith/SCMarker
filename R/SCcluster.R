###### cluster
#' @title SCcluster
#'
#' @param obj SCmarker object for which to process
#'
#' @importFrom Seurat Idents Embeddings RunPCA RunTSNE CreateSeuratObject NormalizeData FindVariableFeatures ScaleData FindNeighbors FindClusters
#' @importFrom magrittr "%>%"
#' @importFrom dim.reduction.wrappers DofastTSNE
#' @importFrom dbscan dbscan
#' @importFrom utils installed.packages
#' 
#' @return
#' @export
#'
#' @examples
SCcluster <- function(obj) {
  data <- obj$rawdata
  gene <- rownames(data) %>% unique()
  index <- match(gene, rownames(data))
  data <- data[index, ]
  marker <- obj$marker
  seuratObj <- CreateSeuratObject(counts = data, 
                             min.cells = 3, 
                             min.features = 200,
                             project = "project")
  seuratObj <- NormalizeData(object = seuratObj, 
                             normalization.method = "LogNormalize", 
                             scale.factor = 10000                             )
  seuratObj <- FindVariableFeatures(object = seuratObj, 
                                    selection.method = "vst")
  seuratObj <- ScaleData(object = seuratObj)
  seuratObj <- RunPCA(object = seuratObj, 
                      pc.genes = marker, 
                      do.print = TRUE, 
                      pcs.print = 1:5,
                      genes.print = 5)
  seuratObj <- FindNeighbors(seuratObj)
  seuratObj <- FindClusters(seuratObj)
  
  if ("dim.reduction.wrappers" %in% installed.packages()){
    seuratObj <- DofastTSNE(seuratObj = seuratObj, 
                            reduction.use = "pca",
                            reduction.save = 'tsne')
    
  } else {
    seuratObj <- RunTSNE(object = seuratObj)
  }
    TSNE <- Embeddings(seuratObj, reduction = "tsne")
  seuratCluster <- Idents(seuratObj)
  dbscanCluster <- dbscan(TSNE, eps = 1.2, minPts = 15)$cluster
  names(dbscanCluster) <- rownames(TSNE)
  obj$tsne <- TSNE
  obj$seuratCluster <- seuratCluster
  obj$dbscanCluster <- dbscanCluster
  return(obj)
}
