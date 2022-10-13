prop_diff <- function(p1, p2, n1, n2) {
  p <- (p1*n1+p2*n2)/(n1+n2)
  z <- (p1-p2)/(p*(1-p)*(1/n1 + 1/n2))
  return(abs(z))
}

#' Calculates the z score of the test that two proportions are the same
#'
#' Arguments \code{p1} and \code{p2} only are vectorized. This is intended for use when you have a lot of different proportions to test (such as proportion of a word present) and the sample sizes are constant across all tests.
#'
#' @param p1,p2 Proportion in first/second group.
#' @param n1,n2 Sample size in first/second group
#' @export
pdv = Vectorize(prop_diff, c('p1','p2'))

#' Creates a tokenized data set, including single words and pre-specified two-word phrases
#'
#' @param dat_orig The data
#' @param textcol String for the column name with text to tokenize
#' @param keepvars String vector of columns to keep with the data. Note an "ID" column will also be added.
#' @param stopwords String vector of words to remove
#' @param hardcode String vector of phrases to detect (more than one word; all one-word phrases will already be detected). Not case sensitive.
#' @param tolower Convert all text to lowercase?
#' @export
get_thetext <- function(dat_orig, textcol, keepvars = NULL, stopwords = NULL, hardcode = NULL, tolower = TRUE) {
  dat = copy(dat_orig)
  dat = dat[, .SD, .SDcols = c(textcol, keepvars)]
  setnames(dat, textcol, 'text')
  keepvars = c('ID',keepvars)

  dat = dat[text != '' & !is.na(text)]
  dat[, ID := 1:.N]


  if (tolower) {
    dat[, text := tolower(text)]
  }

  thetext = dat |>
    tidytext::unnest_tokens(word, text)

  if (!is.null(hardcode)) {
    lilphrase = function(phrase) {
      got_the_phrase = dat[text %ilike% phrase]
      got_the_phrase[, word := phrase]
      got_the_phrase[, text := NULL]
      setcolorder(got_the_phrase, c(keepvars, 'word'))
      return(got_the_phrase)
    }

    allphrase = hardcode |>
      lapply(lilphrase) |>
      rbindlist()
    setnames(allphrase, c(keepvars, 'word'))
    thetext = rbind(thetext, allphrase)
  }

  thetext = thetext[!(word %in% stopwords)]
  thetext = unique(thetext)

  return(thetext)
}
