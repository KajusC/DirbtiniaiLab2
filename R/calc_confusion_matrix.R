#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
##' @param outputPivot
##' @param modelNames
calc_confusion_matrix <- function(outputPivot, modelNames) {
  res <- NULL
  opt.thr <- PresenceAbsence::optimal.thresholds(DATA = outputPivot, model.names = modelNames, opt.methods = "Sens=Spec")
  for (model in modelNames) {
    tmp <- NULL
    tmp$modelName <- model
    tmp$threshold <- opt.thr[1,model]
    tmp$confusion <- caret::confusionMatrix(as.factor(outputPivot[,model]>=tmp$threshold),
                                              as.factor(as.logical(outputPivot$target)),
                                            positive="TRUE", mode="everything")
    res <- c(res,list(tmp))
  }
  return(res)
}
