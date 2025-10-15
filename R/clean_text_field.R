#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
##' @param txt
clean_text_field <- function(txt) {
  txt <- textclean::replace_symbol(txt, pound = FALSE)
  txt <- textclean::replace_white(txt)
  txt <- textclean::add_comma_space(txt)
  txt <- textclean::replace_incomplete(txt, replacement = "... ")
  txt <- textclean::replace_html(txt)
  edt <- lexicon::hash_emoticons
  txt <- textclean::replace_non_ascii(txt, remove.nonconverted = FALSE)
  #edt[, y := lapply(.SD, function(x) paste0('<<',toupper(gsub(' ','-',x)),'>>')), .SDcols = 2]
  txt <- textclean::replace_emoticon(txt, emoticon_dt = edt)
  edt <- lexicon::hash_emojis
  #edt[, y := lapply(.SD, function(x) paste0('<<',toupper(gsub(' ','-',x)),'>>')), .SDcols = 2]
  txt <- textclean::replace_emoji(txt, emoji_dt = edt)
  txt <- gsub('>','',gsub('<','',txt))
  txt <- textclean::replace_non_ascii(txt, remove.nonconverted = TRUE)
  txt <- textclean::replace_money(txt, replacement = 'MONEY')
  txt <- textclean::replace_date(txt, replacement = 'DATE')
  txt <- textclean::replace_email(txt, replacement = 'EMAIL')
  txt <- textclean::replace_url(txt, replacement = 'URL')
  txt <- textclean::replace_hash(txt, replacement = 'HASHTAG')
  txt <- gsub("[[:space:]]|(?=(?![<->])[[:punct:]])", ". ", txt, perl = TRUE)
  txt <- textclean::strip(txt, digit.remove = TRUE, apostrophe.remove = TRUE, lower.case = TRUE)
  txt <- textclean::replace_word_elongation(txt)
  return(txt)
}
