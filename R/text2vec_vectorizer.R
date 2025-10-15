#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
##' @param iteratorObj
##' @param termCountMin
##' @param stopWords
text2vec_vectorizer <- function(iteratorObj, termCountMin, stopWords) {
  v <- create_vocabulary(iteratorObj, stopwords = stopWords)
  v <- prune_vocabulary(v, term_count_min = termCountMin)
  vectorizer <- vocab_vectorizer(v)
}
