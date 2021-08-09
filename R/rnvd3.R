#' @title xxx
#'
#' @description xxx
#'
#' @param data dataframe used for the chart
#' @param formula a two-sided formula like \code{y ~ x}, where \code{"x"} and
#'   \code{"y"} are two column names of \code{data}
#' @param by string, the "by" variable; must be a column name of \code{data}
#' @param palette the name of a viridis color palette, e.g. \cod{"viridis"},
#'   \code{"cividis"} or \code{"turbo"};
#'   see \code{\link[viridisLite]{viridis}}
#' @param xAxisTitle a title for the x-axis; if \code{NULL}, the title is
#'   taken from the \code{formula} argument
#' @param yAxisTitle a title for the y-axis; if \code{NULL}, the title is
#'   taken from the \code{formula} argument
#' @param margins a named list defining the margins, with names \code{"t"},
#'   \code{"r"}, \code{"b"} and \code{"l"}, for "top", "right", "bottom"
#'   and "left" respectively; you can specify only certain margins in the list
#'   to change just those parts
#' @param duration duration of the transition, a number of milliseconds
#' @param rotateLabels a number, the angle of rotation of the labels of the
#'   x-axis (in degrees)
#' @param groupSpacing a number, controls the distance between groups of bars
#' @param xAxisLabelDistance
#' @param yAxisLabelDistance
#' @param yAxisShowMaxMin Boolean, whether to show the min and the max on
#'   the y-axis
#' @param yAxisTickFormat
#' @param rightAlignYaxis Boolean, whether to put the y-axis on the right side
#'   instead of the left
#' @param staggerLabels Boolean, whether to make the x-labels stagger at
#'   different distances from the axis so they're less likely to overlap
#' @param wrapLabels Boolean, whether to split long x-labels by new lines in
#'   order to prevent overlapping
#' @param width
#' @param height
#' @param elementId
#'
#' @return A htmlwidget displaying a grouped/stacked bar chart.
#'
#' @import htmlwidgets
#' @export
#'
#' @examples library(Rnvd3)
#' dat <- reshape2::melt(
#'   apply(HairEyeColor, c(1, 2), sum), value.name = "Count"
#' )
#' multiBarChart(dat, Count ~ Eye, by = "Hair")
multiBarChart <- function(
  data,
  formula,
  by,
  palette = "viridis",
  xAxisTitle = NULL,
  yAxisTitle = NULL,
  margins = list(b = 100, l = 70),
  duration = 1300,
  rotateLabels = 0,
  groupSpacing = 0.1,
  xAxisLabelDistance = 35,
  yAxisLabelDistance = -5,
  yAxisShowMaxMin = FALSE,
  yAxisTickFormat = "d",
  rightAlignYaxis = FALSE,
  staggerLabels = FALSE,
  wrapLabels = FALSE,

  width = NULL, height = NULL, elementId = NULL
) {
  stopifnot(is.null(xAxisTitle) || isString(xAxisTitle))
  stopifnot(is.null(yAxisTitle) || isString(yAxisTitle))
  stopifnot(isNamedList(margins))
  stopifnot(all(names(margins) %in% c("t", "r", "b", "l")))
  stopifnot(isNumber(duration))
  stopifnot(isNumber(rotateLabels))
  stopifnot(isNumber(groupSpacing))
  stopifnot(isNumber(xAxisLabelDistance))
  stopifnot(isNumber(yAxisLabelDistance))
  stopifnot(isBoolean(yAxisShowMaxMin))
  stopifnot(isString(yAxisTickFormat))
  stopifnot(isBoolean(rightAlignYaxis))
  stopifnot(isBoolean(staggerLabels))
  stopifnot(isBoolean(wrapLabels))

  mbcData <- multiBarChartData(data, formula, by, palette)
  axisTitles <- attr(mbcData, "axisTitles")
  margins <- dropNulls(
    list(
      "top"    = margins[["t"]],
      "right"  = margins[["r"]],
      "bottom" = margins[["b"]],
      "left"   = margins[["l"]]
    )
  )

  # forward options using x
  x = list(
    "mbcData"            = mbcData,
    "xAxisTitle"         = xAxisTitle %or% axisTitles[["x"]],
    "yAxisTitle"         = yAxisTitle %or% axisTitles[["y"]],
    "margins"            = margins,
    "duration"           = duration,
    "rotateLabels"       = rotateLabels,
    "groupSpacing"       = groupSpacing,
    "xAxisLabelDistance" = xAxisLabelDistance,
    "yAxisLabelDistance" = yAxisLabelDistance,
    "yAxisShowMaxMin"    = yAxisShowMaxMin,
    "yAxisTickFormat"    = yAxisTickFormat,
    "rightAlignYaxis"    = rightAlignYaxis,
    "staggerLabels"      = staggerLabels,
    "wrapLabels"         = wrapLabels
  )

  # create widget
  htmlwidgets::createWidget(
    name = "rnvd3",
    x,
    width = width,
    height = height,
    package = "Rnvd3",
    elementId = elementId
  )
}

#' @importFrom htmltools tags
#' @noRd
widget_html.rnvd3 <- function(id, style, class, ...){
  tags$div(
    id = id, class = class, style = style,
    tags$svg(style = "height: inherit; width: inherit")
  )
}
