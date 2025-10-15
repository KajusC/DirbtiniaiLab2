#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
##' @param cpuThreads
initialize <- function(cpuThreads=max(1,parallel::detectCores()-1)) {
  clustID <- parallel::makeCluster(cpuThreads)
  tmp <- capture.output(parallel::clusterEvalQ(clustID, require(pacman)))
  tmp <- capture.output(parallel::clusterEvalQ(clustID, pacman::p_load(textclean, lexicon)))
  parallel::setDefaultCluster(cl=clustID)
  message(sprintf("Number of CPU threads = %d", cpuThreads))
  pbapply::pboptions(type = "txt", style = 3)
  return(clustID)
}
