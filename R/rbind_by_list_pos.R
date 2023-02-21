#' Row-binds data.tables in a list of lists
#'
#' This function takes a list of lists of \code{data.table}s (or anything that \code{data.table::rbind} accepts, like \code{data.frame}s), and then row-binds them by position or name. For example, if passed \code{list(list(first=A,second=B),list(first=C,second=D))}, you would get back \code{list(first=rbind(A,C),second=rbind(B,D))}.
#'
#' @param dtl List of lists of \code{data.table}s.
#' @param ignore_names If the list is named, match objects across lists only by their position in the list and not by their names.
#' @examples
#'
#' list_of_lists <- list(
#'     list(data.frame(a = 1), data.frame(a = 2), data.frame(a = 3)),
#'     list(data.frame(a = 4), data.frame(a = 5), data.frame(a = 6))
#' )
#' rbind_by_list_pos(list_of_lists)
#'
#' list_of_named_lists <- list(
#'     list(A = data.frame(a = 1), B = data.frame(a = 2), C = data.frame(a = 3)),
#'     list(C = data.frame(a = 4), A = data.frame(a = 5), B = data.frame(a = 6))
#'  )
#' rbind_by_list_pos(list_of_named_lists)
#'
#' @export

rbind_by_list_pos <- function(dtl,ignore_names=FALSE) {

  # How many tables are we binding
  ntabs <- length(dtl[[1]])

  retDT <- list()

  # If we go by position
  if (ignore_names | is.null(names(dtl[[1]]))) {
    for (i in 1:ntabs) {
      retDT[[i]] <- dtl %>%
        purrr::map(function(x) x[[i]]) %>%
        data.table::rbindlist(fill = TRUE)
    }
  } else {
    for (n in names(dtl[[1]])) {
      retDT[[n]] <- dtl %>%
        purrr::map(function(x) x[[n]]) %>%
        data.table::rbindlist(fill = TRUE)
    }
  }

  return(retDT)
}
