#' A Theme I Like
#'
#' This combines \code{ggpubr::theme_pubr()} with serif text and a horizontal y-axis title. Don't forget to add a \code{scales::label_wrap()} for long y-axis titles!
#'
#' @param size Text font size
#' @param showtext_font String name of font to be included. Assumes that \code{showtext::showtext_auto()} has been set beforehand. Will set \code{lineheight} to .5 by default to offset \code{showtext} oddness.
#' @param ... To pass to \code{ggpubr::theme_pubr()}.
#' @examples
#'
#' # Example data
#' ggplot(mtcars, aes(x = mpg, y = hp)) + theme_nick()
#'
#' @export
theme_nick <- function(size = 14, showtext_font = NULL, ...) {
  if (is.null(showtext_font)) {
    font_elem = ggplot2::element_text(family = 'serif', size = size)
  } else {
    font_elem = ggplot2::element_text(family = showtext_font, size = size, lineheight = .5)
  }

  ggpubr::theme_pubr(...) +
    ggplot2::theme(text= font_elem,
          axis.title.y = ggplot2::element_text(hjust = 1, angle = 0))
}


#' Bar graph with labeled values on top
#'
#' Given an explicit bar height, constructs a bar graph and puts the value given for the height on top of the bar, formatted, in Serif size 13 font.
#'
#' @param dat Data.
#' @param xvar x-axis variable, as a string.
#' @param yvar y-axis variable, as a string.
#' @param yscale Numerical formatting function, in \code{scales::label_} style.
#' @param fillcolor The color of the bars.
#' @param limits y-axis limits on the graph, usually to make it taller so as to fit the text.
#' @examples
#'
#' # Example data
#' data.frame(x = c('A','B'), y = c(4, 5)) |>
#'     labeled_bar('x','y')
#'
#' @export
labeled_bar = function(dat, xvar, yvar, yscale = scales::label_number(big.mark = ','), fillcolor = 'lightblue',
                       limits = NULL) {
  ggplot2::ggplot(dat, ggplot2::aes_string(x = xvar, y = yvar)) +
    ggplot2::geom_col(fill = fillcolor, size = 1) +
    ggplot2::geom_text(ggplot2::aes(label = yscale(.data[[yvar]])), vjust = -.5,
              family = 'serif', size = 13/ggplot2::.pt) +
    theme_nick() +
    ggplot2::scale_y_continuous(limits = limits, labels = yscale)
}

#' Aggregates to a mean value and plots that value, with 95 percent confidence interval
#'
#' Given a grouping variable and a value to aggregate, gets the mean and 95% CI, and plots it on a labeled bar graph with a confidence interval
#'
#' @param dat Data.
#' @param xvar x-axis variable, as a string.
#' @param yvar y-axis variable to aggregate, as a string.
#' @param yscale Numerical formatting function, in \code{scales::label} style.
#' @param fillcolor The color of the bars.
#' @param highlight Set of categories to highlight
#' @param highlightcolor The color of the highlighted bars
#' @param colormap Skip \code{fillcolor} and the highlight stuff and color the bars according to this color palette. No, there's no default palette.
#' @param limits y-axis limits on the graph, usually to make it taller so as to fit the text.
#' @param cis Show confidence intervals
#' @param hjust Horizontal justification for the mean labels
#' @param savedata Save the graphed data to CSV file by this name.
#' @examples
#'
#' # Not now
#'
#' @export
dynamite_plot = function(dat_orig, xvar, yvar, yscale = scales::label_number(big.mark = ','), fillcolor = 'lightblue',
                       highlight = NULL, highlightcolor = 'firebrick', colormap = NULL,
                       limits = NULL, cis = TRUE, hjust = ifelse(cis == TRUE, 1.5, .5), savedata = NULL) {
  if (!is.null(highlight) & !is.null(colormap)) {
    stop('Can\'t specify both highlight and colormap.')
  }
  dat = copy(dat_orig)
  data.table::setDT(dat)
  data.table::setnames(dat, yvar, 'OUTCOME')
  rdate = dat[, list(Y = mean_rm(OUTCOME),
                     citop = mean_rm(OUTCOME) + 1.96*sd_rm(OUTCOME)/sqrt(.N),
                     cibot = mean_rm(OUTCOME) - 1.96*sd_rm(OUTCOME)/sqrt(.N)), by = xvar]
  rdate = rdate[Y != citop]

  if (!is.null(savedata)) {
    fwrite(rdate, savedata)
  }

  rdate[[xvar]] = factor(rdate[[xvar]])


  if (is.null(highlight) & is.null(colormap)) {
    p = ggplot2::ggplot(rdate, ggplot2::aes_string(x = xvar, y = 'Y', ymin = 'cibot', ymax = 'citop')) +
      ggplot2::geom_col(fill = fillcolor, size = 1)
  } else if (!is.null(colormap)) {
    p = ggplot2::ggplot(rdate, ggplot2::aes_string(x = xvar, y = 'Y', ymin = 'cibot', ymax = 'citop', fill = xvar)) +
      ggplot2::geom_col(size = 1) +
      ggplot2::scale_fill_manual(values = colormap) +
      ggplot2::guides(fill = 'none')
  } else {
    cats = sort(unique(rdate[[xvar]]))
    cats = c(highlight, levels(rdate[[xvar]])[cats[!(cats %in% highlight)]])
    rdate[[xvar]] = factor(rdate[[xvar]], levels = cats)
    p = ggplot2::ggplot(rdate, ggplot2::aes_string(x = xvar, y = 'Y', ymin = 'cibot', ymax = 'citop'))  +
      ggplot2::geom_col(ggplot2::aes_string(fill = xvar)) +
      ggplot2::scale_x_discrete(limits = cats) +
      ggplot2::scale_fill_manual(values = c(rep(highlightcolor, length(highlight)),
                                            rep(fillcolor, length(cats) - length(highlight)))) +
      ggplot2::guides(fill = 'none')
  }

  p = p + ggplot2::geom_errorbar(width = .1) +
    ggplot2::geom_text(ggplot2::aes(label = yscale(Y), y = max(Y)/8), vjust = -.5, hjust = hjust,
                       family = 'serif', size = 13/ggplot2::.pt)

  p = p +
    theme_nick() +
    ggplot2::scale_y_continuous(limits = limits, labels = yscale)
  return(p)
}


#' Grouped dynamite plot with median markers and group labels at the top
#'
#' Given a grouping variable and a value to aggregate, gets the mean and 95% CI, and plots it on a labeled bar graph with a confidence interval
#'
#' To control the ordering of the bars, set \code{help_ordering} to \code{FALSE}, and make sure that both \code{xvar} and \code{catvar} are ordered such that the \code{xvar} values are ordered such that all the same \code{catvar}s go together.
#'
#' @param dat Data.
#' @param xvar x-axis variable, as a string.
#' @param catvar Variable indicating the categories that each of the x-axis values slot into.
#' @param yvar y-axis variable to aggregate, as a string.
#' @param yscale Numerical formatting function, in \code{scales::label} style.
#' @param catcolor A labeled (or properly ordered) vector connecting each value of \code{catvar} to a color.
#' @param limits y-axis limits on the graph, usually to make it taller so as to fit the text.
#' @param cis Show confidence intervals
#' @param medians Show medians
#' @param hjust Horizontal justification for mean text labels
#' @param lcolor Color for mean text labels
#' @param lfamily Font family for mean text labels
#' @param lsize Font size for mean text labels
#' @param lpos Y-value height of the mean text labels
#' @param tfamily Font family for theme (will use \code{showtext_font} in \code{theme_nick()})
#' @param tsize Font size for theme
#' @param help_ordering Reorder the levels of the \code{xvar} variable so that they're ordered within values of \code{catvar}.
#' @param savedata Save the graphed data to CSV file by this name.
#' @examples
#'
#' # Not now
#'
#' @export
grouped_colorlab_bar = function(dat_orig, xvar, catvar, yvar, yscale = scales::label_number(big.mark = ','), catcolor = NULL,
                         limits = NULL, cis = TRUE, medians = TRUE, hjust = ifelse(cis == TRUE, 1.5, .5), lpos = .5,
                         lcolor = 'white', lfamily = 'serif', lsize = 14/ggplot2::.pt, tfamily = NULL, tsize = 14, help_ordering = TRUE, savedata = NULL) {
  dat = copy(dat_orig)
  data.table::setDT(dat)
  data.table::setnames(dat, yvar, 'OUTCOME')

  if (max(dat[, eval(pp('.(NU = uniqueN(', catvar, '))')), by = xvar]$NU) > 1) {
    stop('Each xvar must be associated with no more than one catvar.')
  }

  # Ensure catvar is ordered
  if (!is.ordered(dat[[catvar]])) {
    dat[, eval(pp(catvar, ' := ', ' factor(', catvar, ', ordered = TRUE)'))]
  }
  # And order xvar
  if (help_ordering == TRUE) {
    xlevs = dat[, eval(pp('.(cat = first(', catvar, '))')), by = xvar]
    setorderv(xlevs, c('cat', xvar))
    dat[, eval(pp(xvar, ' := factor(', xvar, ', levels = xlevs$', xvar, ')'))]
  }

  setorderv(dat, c(catvar, xvar))

  rdate = dat[, list(Y = mean_rm(OUTCOME),
                     Median = quantile_rm(OUTCOME),
                     citop = mean_rm(OUTCOME) + 1.96*sd_rm(OUTCOME)/sqrt(.N),
                     cibot = mean_rm(OUTCOME) - 1.96*sd_rm(OUTCOME)/sqrt(.N)), by = c(catvar, xvar)]
  rdate = rdate[Y != citop]
  rdate[, eval(pp('xnum := as.numeric(', xvar, ')'))]

  if (!is.null(savedata)) {
    fwrite(rdate, savedata)
  }

  bardata = dat[, eval(pp('.(cat = first(', catvar, '))')), by = xvar]
  setnames(bardata, c('X', 'CAT'))
  bardata[, X := as.numeric(X)]
  lastX = max(bardata$X)
  bardata = bardata[, .(X = first(X)), by = CAT]
  bardata[, xend := shift(X, -1)]
  bardata[.N, xend := lastX+.5]

  p = ggplot2::ggplot(rdate, ggplot2::aes_string(x = xvar, y = 'Y', ymin = 'cibot', ymax = 'citop', fill = catvar)) +
    ggplot2::geom_col(width = .5)

  if (cis == TRUE) {
    p = p + ggplot2::geom_errorbar(width = .1)
  }

  p = p +
    ggplot2::geom_text(ggplot2::aes(label = yscale(Y), y = lpos),
              family = lfamily, size = lsize, color = lcolor, fontface = 'bold')

  if (medians == TRUE) {
    p = p + ggplot2::geom_segment(ggplot2::aes(x = xnum - .2, xend = xnum + .2, y = Median, yend = Median), size = 1)
  }

  # Add category labels
  lineheight_val = ifelse(cis == TRUE, max(rdate$citop) + .1, max(c(rdate$Y, rdate$Median)) + .1)
  p = p +
    ggplot2::geom_segment(ggplot2::aes(x = X - .5, xend = xend, color = CAT, y = lineheight_val, yend = lineheight_val, ymin = NULL, ymax = NULL, fill = NULL),
                          data = bardata, size = 2) +
    ggplot2::geom_text(ggplot2::aes(x = X - .5, y = lineheight_val, label = CAT, color = CAT, ymin = NULL, ymax = NULL, fill = NULL),
              data = bardata, family = lfamily, size = 20/ggplot2::.pt, hjust = 0, vjust = -.9) +
    ggplot2::guides(fill = 'none', color = 'none') +
    theme_nick(size = tsize, showtext_font = tfamily) +
    ggplot2::scale_x_discrete(labels = scales::label_wrap(10)) +
    ggplot2::scale_y_continuous(limits = limits, labels = yscale)

  if (!is.null(catcolor)) {
    p = p +
      ggplot2::scale_fill_manual(values = catcolor) +
      ggplot2::scale_color_manual(values = catcolor)
  }

  return(p)
}

#' Grouped bar graph with labeled values on top
#'
#' Given an explicit bar height, constructs a bar graph with multiple categories and puts the value given for the height on top of the bars, formatted, in Serif size 13 font.
#'
#' @param dat Data.
#' @param xvar x-axis variable, as a string.
#' @param yvar y-axis variable, as a string.
#' @param fillvar Group variable for the bars
#' @param yscale Numerical formatting function, in \code{scales::label_} style.
#' @param limits y-axis limits on the graph, usually to make it taller so as to fit the text.
#' @examples
#'
#' # Example data
#' data.frame(x = c('A','B', 'A','B'), group = c('C','C','D','D'), y = c(4, 5,6,7)) |>
#'     dodged_labeled_bar('x','y', 'group')
#'
#' @export
dodged_labeled_bar = function(dat, xvar, yvar, fillvar, yscale = scales::label_number(big.mark = ','),
                       limits = NULL) {
  ggplot2::ggplot(dat, ggplot2::aes_string(x = xvar, y = yvar, fill = fillvar)) +
    ggplot2::geom_col(size = 1, position = 'dodge') +
    ggplot2::geom_text(ggplot2::aes(label = yscale(.data[[yvar]])), vjust = -.5,
                       family = 'serif', size = 13/ggplot2::.pt, position = ggplot2::position_dodge(.9)) +
    theme_nick() +
    ggplot2::scale_y_continuous(limits = limits, labels = yscale)
}

#' Sorted, formatted table with formatted values and top-n selection
#'
#' Sorts a table by a value, picks the top N values, and then produces a nice looking table.
#'
#' @param dat_orig Data.
#' @param ntop How many rows to keep
#' @param vars Variables to keep in the table
#' @param sort Which variable to sort by
#' @param order Sort order (-1 = descending)
#' @param vscales List of \code{scales::label_} formatting functions to format the columns of data for presentation, or \code{NULL} to just present raw values.
#' @param rank Add a "Rank" variable showing the row numbers to the left of the table.
#' @param savedata Save the full data (before trimming to top N) to cSV file by this name
#' @examples
#'
#' # Example data
#' data.frame(x = c('A','B', 'A','B'), group = c('C','C','D','D'), y = c(4, 5,6,7)) |>
#'     topn_table(2)
#'
#' @export
topn_table = function(dat_orig, ntop = min(10, nrow(dat)), vars = names(dat), sort = tail(vars,1), order = -1L, vscales = NULL, rank = TRUE, savedata = NULL) {
  dat = copy(dat_orig)
  data.table::setDT(dat)
  dat = data.table:::subset.data.table(dat, select = vars)
  data.table::setcolorder(dat, vars)
  data.table::setorderv(dat, sort, order = order)
  if (!is.null(savedata)) {
    fwrite(dat, savedata)
  }
  dat = dat[1:ntop,]
  data.table::setDT(dat)
  if (!is.null(vscales)) {
    for (i in 1:length(vscales)) {
      if (!is.null(vscales[[i]])) {
        coln = names(dat)[i]
        evaltext = paste0('`',coln, '` := vscales[[', i, ']](`',coln,'`)')
        dat[, eval(parse(text = evaltext))]
      }
    }
  }
  if (rank) {
    dat[, Rank := 1:.N]
    data.table::setcolorder(dat, 'Rank')
  }
  dat |> knitr::kable() |> kableExtra::kable_styling('striped')
}

#' Line Graph with Confidence Interval Ribbon
#'
#' Calculates the average of \code{yvar} as well as a 95% confidence interval for that mean, within each \code{xvar} value. Then produces a line graph with a confidence ribbon. Will drop any observation for which the confidence interval has a range of 0.
#'
#' @param dat_orig Data.
#' @param xvar x-axis variable, as a string.
#' @param yvar y-axis variable, as a string.
#' @param colorvar Color variable to do multiple lines at once
#' @param yscale Numerical formatting function, in \code{scales::label_} style.
#' @param limits y-axis limits on the graph, usually to make it taller so as to fit the text.
#' @param savedata Save the graphed data to a CSV file by this name.
#' @examples
#'
#' # Example data
#' line_w_ribbon(mtcars, 'cyl', 'mpg')
#'
#' @export
line_w_ribbon = function(dat_orig, xvar, yvar, colorvar = NULL, yscale = scales::label_number(), limits = NULL, savedata = NULL) {
  dat = copy(dat_orig)
  data.table::setDT(dat)
  data.table::setnames(dat, yvar, 'OUTCOME')
  rdate = dat[, list(Y = mean_rm(OUTCOME),
                   citop = mean_rm(OUTCOME) + 1.96*sd_rm(OUTCOME)/sqrt(.N),
                   cibot = mean_rm(OUTCOME) - 1.96*sd_rm(OUTCOME)/sqrt(.N)), by = c(xvar, colorvar)]
  rdate = rdate[Y != citop]

  if (!is.null(savedata)) {
    fwrite(rdate, savedata)
  }

  if (is.null(colorvar)) {
    p = ggplot2::ggplot(rdate, ggplot2::aes_string(x = xvar, y = 'Y', ymin = 'cibot', ymax = 'citop')) +
      ggplot2::geom_ribbon(fill = 'blue', alpha = .2) +
      ggplot2::geom_line(size = 1, color = 'blue')
  } else {
    p = ggplot2::ggplot(rdate, ggplot2::aes_string(x = xvar, y = 'Y', ymin = 'cibot', ymax = 'citop', color = colorvar, fill = colorvar)) +
      ggplot2::geom_ribbon(alpha = .2) +
      ggplot2::geom_line(size = 1)
  }

  p = p +
    ggplot2::scale_y_continuous(labels = yscale, limits = limits) +
    theme_nick()

  return(p)
}


#' Hexgrid US State Map
#'
#' Creates a hexgrid map of the United States. CURRENTLY NOT WORKING DUE TO RETIREMENT OF RGEOS AND RGDAL
#'
#' @param dat_orig Data
#' @param stvar Column name with the two-letter state code in it.
#' @param yvar Variable to determine color intensity.
#' @param yscale Function
#' @param na.include Include states not listed in \code{dat_orig} as blanks.
#' @param grad_low,grad_mid,grad_high Colors to use for gradient intensity. \code{grad_mid} is optional.
#'
#' @export
us_hex_map = function(dat_orig, stvar, yvar, yscale = scales::label_number(big.mark = ','), na.include = TRUE,
                      grad_low = '#56B1F7', grad_mid = NULL, grad_high = '#C0C0C0') {
  stop('THIS FUNCTION CURRENTLY NOT WORKING DUE TO RETIREMENT OF RGEOS')
  data("spdf_fortified")
  data("centers")
  dat = copy(dat_orig)
  setDT(dat)
  setnames(dat, stvar, 'id')
  setnames(dat, yvar, 'Y')
  if (na.include) {
    spdf_fortified <- dplyr::full_join(spdf_fortified, dat, by = 'id')
    centers_lab = dplyr::full_join(centers, dat, by = 'id')
  } else {
    spdf_fortified <- dplyr::inner_join(spdf_fortified, dat, by = 'id')
    centers_lab = dplyr::inner_join(centers, dat, by = 'id')
  }

  p = ggplot2::ggplot() +
    ggplot2::geom_polygon(data = spdf_fortified,
                          ggplot2::aes( x = long, y = lat, group = group, fill = Y),  color="white") +
    ggplot2::geom_text(data=centers_lab, ggplot2::aes(x=x, y=y,
                                             label=paste0(id, '\n', ifelse(is.na(Y),'',yscale(Y))))) +
    ggplot2::theme_void() +
    ggplot2::coord_map() +
    ggplot2::guides(fill = 'none')
  if (is.null(grad_mid)) {
    p = p + ggplot2::scale_fill_gradient(trans = 'reverse', high = grad_high, low = grad_low)
  } else {
    p = p + ggplot2::scale_fill_gradient2(trans = 'reverse', high = grad_high, low = grad_low, mid = grad_mid)
  }
  return(p)
}

#' Numeric Labeling Across a Wide Range
#'
#' Applies scales::comma labeling. Chooses appropriate scaling and adds a suffix separately for sub-1000, 1k-1M, 1M-1B, 1B-1T, 1T+ Above 1T keeps reporting trillions.
#'
#' \code{label_rangescale()} returns a version of the \code{rangescale} function with the appropriate options selected, like the other \code{label_} functions in the scales package.
#'
#' @param x Data.
#' @param accuracy For the most part, see \code{scales::comma}. However, this also accepts a vector of options. If a vector is set, it will apply that value to the sub-1000, 1k-1M, 1M-1B, 1B-1T options, in that order. If fewer than five options are set, the last option will be copied forward (so \code{c(.1, 1)} will give an accuracy of .1 to sub-1k and 1 to the rest).
#' @param prefix,big.mark,decimal.mark,trim,... See \code{scales::comma}. Note suffix and scale cannot be set, and accuracy default is now 1.
#' @examples
#'
#' # Example data
#' rangescale(c(400, 4000, 4200000, 4300000000))
#' rangescale(c(400, 4000, 4200000, 4300000000), accuracy = c(1, .1))
#' my_range_fcn = label_rangescale(accuracy = c(1, .1))
#' my_range_fcn(c(400, 4000, 4200000, 4300000000))
#'
#' @rdname rangescale
#' @export
rangescale = function(x, accuracy = 1, prefix = '',
                      big.mark = ',', decimal.mark = '.', trim = TRUE, ...) {
  acc_l = length(accuracy)
  if (acc_l < 5) {
    accuracy = c(accuracy, rep(accuracy[acc_l], 5 - acc_l))
  }
  data.table::fcase(abs(x) < 1000, scales::comma(x, accuracy = accuracy[1], scale = 1,
                                                 prefix = prefix, suffix = '',
                                                 big.mark = big.mark,
                                                 decimal.mark = decimal.mark,
                                                 trim = trim, ...),
                    abs(x) < 1000000, scales::comma(x, accuracy = accuracy[2], scale = 1/1000,
                                                    prefix = prefix, suffix = 'k',
                                                    big.mark = big.mark,
                                                    decimal.mark = decimal.mark,
                                                    trim = trim, ...),
                    abs(x) < 1000000000, scales::comma(x, accuracy = accuracy[3], scale = 1/1000000,
                                                   prefix = prefix, suffix = 'M',
                                                   big.mark = big.mark,
                                                   decimal.mark = decimal.mark,
                                                   trim = trim, ...),
                    abs(x) < 1000000000000, scales::comma(x, accuracy = accuracy[4], scale = 1/1000000000,
                                                   prefix = prefix, suffix = 'B',
                                                   big.mark = big.mark,
                                                   decimal.mark = decimal.mark,
                                                   trim = trim, ...),
                    !is.na(x), scales::comma(x, accuracy = accuracy[5], scale = 1/1000000000000,
                                             prefix = prefix, suffix = 'T',
                                             big.mark = big.mark,
                                             decimal.mark = decimal.mark,
                                             trim = trim, ...))
}

#' @rdname rangescale
#' @export
label_rangescale = function(accuracy = 1, prefix = '',
                            big.mark = ',', decimal.mark = '.', trim = TRUE, ...) {
  function(x) {
    rangescale(x, accuracy = accuracy, prefix = prefix,
               big.mark = big.mark, decimal.mark = decimal.mark,
               trim = trim, ...)
  }
}
