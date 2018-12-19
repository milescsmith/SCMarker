##### filter data
#' @title ModalFilter
#'
#' @param data 
#' @param geneK 
#' @param cellK 
#' @param width 
#' @param cutoff 
#'
#' @return
#' @export
#'
#' @examples
ModalFilter <- function(data, geneK, cellK, width = 1, cutoff = 2) {
  rawdata <- data
  cellSumm <- data.frame(cell = colnames(data))
  cellSumm$count <- sapply(1:dim(data)[2], 
                           genecount, 
                           data = data, 
                           index = "col")
  data <- data[, cellSumm$count > cellK]
  binadata <- Bina(data, 
                   cutoff = cutoff)
  geneSumm <- data.frame(gene = rownames(data))
  geneSumm$count <- rowSums(binadata)
  data <- data[geneSumm$count > geneK, ]
  binadata <- binadata[geneSumm$count > geneK, ]
  geneSumm <- geneSumm[geneSumm$count > geneK, ]
  cellSumm <- cellSumm[cellSumm$count > cellK, ]
  geneSumm$exppeak <- apply(data, 1, 
                            genepeak, 
                            width = width)
  data <- data[geneSumm$exppeak >= 2, ]
  binadata <- binadata[geneSumm$exppeak >= 2, ]
  geneSumm <- geneSumm[geneSumm$exppeak >= 2, ]
  obj <- list(rawdata = rawdata, 
              newdata = data, 
              geneSumm = geneSumm, 
              cellSumm = cellSumm, 
              binadata = binadata)
  return(obj)
}
