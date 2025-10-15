#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
##' @param outputPivot
##' @param skipCols
##' @param targetVal
plot_detection_results <- function(outputPivot,skipCols=c("index","target"),targetVal="1") {
  response <- as.factor(outputPivot$target)
  predictors <- as.matrix(outputPivot[,-which(colnames(outputPivot) %in% skipCols)])
  DET::detc(response, predictors, positive=targetVal)
}
