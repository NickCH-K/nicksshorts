#' Mean with na.rm
#'
#' This is just a mean function but with na.rm set to TRUE.
#'
#' @param ... To pass to \code{mean()}.
#' @examples
#'
#' # Example data
#' mean_rm(c(1, NA))
#'
#' @export
mean_rm = function(...) {
  mean(..., na.rm = TRUE)
}

#' Sum with na.rm
#'
#' This is just a sum function but with na.rm set to TRUE.
#'
#' @param ... To pass to \code{sum()}.
#' @examples
#'
#' # Example data
#' sum_rm(c(1, NA))
#'
#' @export
sum_rm = function(...) {
  sum(..., na.rm = TRUE)
}

#' Standard deviation with na.rm
#'
#' This is just a sd function but with na.rm set to TRUE.
#'
#' @param ... To pass to \code{sd()}.
#' @examples
#'
#' # Example data
#' sd_rm(c(1, 2, NA))
#'
#' @export
sd_rm = function(...) {
  sd(..., na.rm = TRUE)
}

#' Quantile function with na.rm
#'
#' This is just a quantile function, by default the median, but with na.rm set to TRUE.
#'
#' @param ... To pass to \code{quantile()}.
#' @param prob The quantile to get, by default the median.
#'
#' @examples
#'
#' # Example data
#' quantile_rm(c(1, 2, NA))
#'
#' @export
quantile_rm = function(..., prob = .5) {
  quantile(..., probs = prob, na.rm = TRUE)
}

#' Weighted mean function with na.rm
#'
#' This is just a weighted mean function, but with \code{na.rm = TRUE} (and adjusted so that it actually works).
#'
#' @param x Value to take the mean of
#' @param w Weights
#' @param ... Other arguments to pass along
#'
#' @examples
#'
#' # Example data
#' weighted_mean_rm(c(NA,1,2),c(1,NA,2))
#'
#' @export
weighted_mean_rm = function(x, w, ...) {
  weighted.mean(x[!is.na(x) & !is.na(w)], w[!is.na(x) & !is.na(w)], ...)
}

#' Weighted median function with na.rm
#'
#' This is a weighted median function, which returns the value X for which 50% of the weight is for values X or lower, and less than 50% of the weight is for values lower than X. Missing values are always dropped.
#'
#' Should work with any sortable \code{x} variable, including strings.
#'
#' @param x Value to take the median of
#' @param w Weights
#'
#' @examples
#'
#' # Example data
#' weighted_median_rm(c(NA,1,2,3,4),c(1,NA,2,3,10))
#'
#' @export
weighted_median_rm = function(x, w) {
  dat = data.table(x=x, w=w)
  dat = na.omit(dat)

  if (nrow(dat) == 0) {
    return(NA)
  }
  if (nrow(dat) == 1) {
    return(dat[, x])
  }

  setorder(dat, x)
  dat[, cumw := cumsum(w)/sum(w)]

  if (dat[1, cumw] >= .5) {
    return(dat[1, x])
  }

  return(dat[cumw >= .5 & shift(cumw) < .5, x])
}

#' Fast-to-type recoding function
#'
#' Recodes just some of the values in one variable into another.
#'
#' @param dat Original \code{data.table}. Note this data will be directly modified in-place.
#' @param var String name of the variable you want to recode.
#' @param from Vector of values to recode from. Any value of \code{var} left out here will be maintained as-is, unless \code{difftype = TRUE}.
#' @param to Vector of values of the same length as \code{from} to recode to, in the same order.
#' @param newvar If you want the recoded values in another variable, give the string name of that new variable here.
#' @param checkfrom Check that all values of \code{from} are actually in the data, and give a warning if they're not. This will make the code run slower.
#' @param difftype Instead of starting by copying all the old \code{var} values, start with a \code{NULL} column. This will allow the recoded values to be a different type from the original. This requires that \code{newvar} not be the same as \code{var}.
#' @examples
#'
#' dat = data.table(myvar = c('a','b','c'))
#' qrecode(dat, 'myvar',c('a','c'),c('d','e'))
#' @export
qrecode = function(dat, var, from, to, newvar = var, checkfrom = FALSE, difftype = FALSE) {
  setDT(dat)
  if (length(from) != length(to)) {
    stop('from and to are different lengths.')
  }
  if (difftype & newvar == var) {
    stop('If difftype is specified, newvar and var cannot be the same.')
  }
  if (is.null(dat[[newvar]])) {
    if (!difftype) {
      dat[, eval(parse(text = paste0(newvar, ' := ', var)))]
    }
  }
  for (i in 1:length(from)) {
    if (checkfrom) {
      if (dat[eval(parse(text = paste0(var,' == from[i]'))),.N] == 0) {
        warning(paste0('No observations of ', from[i], ' in the data.\n'))
      }
    }
    dat[eval(parse(text = paste0(var,' == from[i]'))),
        eval(parse(text = paste0(newvar, ' := to[i]')))]
  }
}

#' Convenience function for paste0 and parse
#'
#' This is just \code{parse(text = paste0(...))} to be fed to \code{eval}.
#'
#' @param ... To pass to \code{paste0()}.
#' @examples
#'
#' # Example data
#' eval(pp('2','+','2'))
#'
#' @export
pp = function(...) {
  parse(text = paste0(...))
}


#' Get the linear slope between two variables
#'
#' This performs a linear regression using \code{fixest::feols} and returns only the second coefficient (i.e. the first non-intercept coefficient).
#'
#' @param data Dataset
#' @param formula Formula
#' @param y Instead pass the dependent variable as a vector (only works with a single predictor).
#' @param x Instead pass a single predictor as a vector.
#' @param se Instead of just returning the coefficient, pass a vector that also includes the standard error.
#' @param p Instead of just returning the coefficient, pass a vector that also includes the p-value (will be third element if \code{se = TRUE}).
#' @param names Name the return vector
#' @param ... Other arguments to pass to \code{fixest::feols}
#' @examples
#'
#' # Example data
#' eval(pp('2','+','2'))
#'
#' @export
linear_slope = function(dat = NULL, formula = NULL, y = NULL, x = NULL, se = FALSE, p = FALSE, names = (se | p), ...) {
  if ((is.null(formula) | is.null(dat)) & (is.null(y) | is.null(x))) {
    stop('Must specify data and formula or x and y.')
  }
  if (is.null(dat) & is.null(formula)) {
    formula = y~x
    dat = data.table(y = y, x = x)
  }
  m = fixest::feols(formula, dat, ...)

  res = m$coeftable$Estimate[2]
  namevec = c('Estimate')
  if (se == TRUE) {
    res = c(res, m$coeftable$`Std. Error`[2])
    namevec = c(namevec, 'SE')
  }
  if (p == TRUE) {
    res = c(res, m$coeftable$`Pr(>|t|)`[2])
    namevec = c(namevec, 'p')
  }
  if (names) {
    names(res) = namevec
  }
  return(res)
}

#' Subset a string up to (or after) a marker substring
#'
#' This takes a string (or vector of strings) and looks for the first regex match. Then, it returns the content of that string up to that match (or following the match, as desired).
#'
#' It's called \code{str_eat} because you often use it on messy string data, "eating" it from the left as you chomp it piece by piece into processed pieces.
#'
#' @param string Input vector. Either a character vector, or something coercible to one.
#' @param pattern Pattern to look for. See \code{stringr::str_locate} for more details.
#' @param keep Do you want the text to the 'left' of the match, the 'match' itself, or to the 'right' of the match? Accepts a vector to keep multiple options.
#' @param direction Look for the first match coming from the 'left' or the 'right'?
#' @param trim Apply a \code{stringr::str_trim()} after getting the substring.
#' @param keep_nonmatch Set to 'full' to return the original string if the match isn't found. Set to 'blank' to return a blank string if no match is found. Set to 'NA' to return a missing value if no match is found.
#' @export
str_eat = function(string, pattern, keep = 'left',
                   direction = 'left', trim = FALSE, keep_nonmatch = 'blank') {

  if (sum(c('left','right') %in% keep) == 2) {
    stop('left and right can\'t both be in keep')
  }
  keep = keep[keep %in% c('left','right','match')]
  if (length(keep) == 0) {
    stop('Invalid keep values. Must be some mix of left, right, and match.')
  }

  if (direction == 'left') {
    matches = data.table::data.table(stringr::str_locate(string, pattern))
  } else if (direction == 'right') {
    matches = stringr::str_locate_all(string, pattern)
    matches = data.table::rbindlist(
      lapply(matches, function(x) if(nrow(x) == 0) {
        data.table::data.table(start = NA, end = NA)
        } else { data.table::data.table(tail(x, 1)) })
    )
  } else { stop('direction must be left or right.')}


  if ('left' %in% keep) {
    starts = rep(1, length(string))
  } else if ('match' %in% keep) {
    starts = matches$start
  } else {
    starts = matches$end + 1
  }

  if ('right' %in% keep) {
    ends = rep(-1, length(string))
  } else if ('match' %in% keep) {
    ends = matches$end
  } else {
    ends = matches$start - 1
  }

  string_sub = stringr::str_sub(string, starts, ends)

  if (trim) {
    string_sub = stringr::str_trim(string_sub)
  }

  if (keep_nonmatch == 'full') {
    string_sub = data.table::fifelse(is.na(matches$start),
                                     string,
                                     string_sub)
  } else if (keep_nonmatch == 'blank') {
    string_sub = data.table::fifelse(is.na(matches$start),
                                     '',
                                     string_sub)
  } else if (keep_nonmatch == 'NA') {
    string_sub = data.table::fifelse(is.na(matches$start),
                                     NA_character_,
                                     string_sub)
  } else { stop('Invalid keep_nonmatch value. Must be full, blank, or NA.')}

  return(string_sub)
}


#' Subset a string up from one marker substring to the next
#'
#' This takes a string (or vector of strings) and looks for the first regex match. Then, it returns the content of that string up to the next match.
#'
#' @param string Input vector. Either a character vector, or something coercible to one.
#' @param pattern_first Pattern to look for. See \code{stringr::str_locate} for more details.
#' @param pattern_second Second pattern to look for. See \code{stringr::str_locate} for more details.
#' @param keep Do you want the text to the 'between' the matches, the 'first' match, or the 'second' match? Accepts a vector to keep multiple options.
#' @param trim Apply a \code{stringr::str_trim()} after getting the substring.
#' @param keep_nonmatch Set to 'full' to return the original string if the match isn't found. Set to 'blank' to return a blank string if no match is found. Set to 'NA' to return a missing value if no match is found.
#' @export
str_between = function(string, pattern_first, pattern_second, keep = 'between',
                   trim = FALSE, keep_nonmatch = 'blank') {

  keep = keep[keep %in% c('first','second','between')]
  if (length(keep) == 0) {
    stop('Invalid keep values. Must be some mix of first, second, and between.')
  }

  matches = data.table::data.table(stringr::str_locate(string, pattern_first))

  first_pattern = stringr::str_extract(string, pattern_first)

  string_r = stringr::str_sub(string, matches$end+1)

  matches = data.table::data.table(stringr::str_locate(string_r, pattern_second))

  if ('first' %in% keep) {
    str_match = first_pattern
  } else {
    str_match = rep('',length(string))
  }
  miss_rows = is.na(first_pattern)

  if ('between' %in% keep) {
    between_pattern = stringr::str_sub(string_r, 1, matches$start - 1)
    str_match = paste0(str_match, between_pattern)
    miss_rows = miss_rows | is.na(between_pattern)
  }

  if ('right' %in% keep) {
    second_pattern = stringr::str_extract(string_r, pattern_second)
    str_match = paste0(str_match, second_pattern)
    miss_rows = miss_rows | is.na(second_pattern)
  }

  if (trim) {
    str_match = stringr::str_trim(str_match)
  }

  if (keep_nonmatch == 'full') {
    str_match[miss_rows] = string
  } else if (keep_nonmatch == 'blank') {
    str_match[miss_rows] = ''
  } else if (keep_nonmatch == 'NA') {
    str_match[miss_rows] = NA_character_
  } else { stop('Invalid keep_nonmatch value. Must be full, blank, or NA.')}

  return(str_match)
}
