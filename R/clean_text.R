#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
##' @param raw_data_frame
##' @param text_field
##' @param cpu_count
clean_text <- function(raw_data_frame, text_field="text", cpu_count=max(1,parallel::detectCores()-1)) {
  cluster_id <- initialize(cpu_count)
  cln_data_frame <- data.frame(doc_id=1:nrow(raw_data_frame), raw_data_frame, stringsAsFactors=F)
  cln_data_frame[,text_field] <- pbapply::pbsapply(cln_data_frame[,text_field],
                                                   clean_text_field, simplify=T, USE.NAMES=F, cl=cluster_id)
  parallel::stopCluster(cl=cluster_id)
  return(cln_data_frame)
}
