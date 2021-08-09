#' @title Multibar chart
#'
#' @description HTMLwidget displaying a multibar chart.
#'
#' @param data dataframe used for the chart
#' @param formula a two-sided formula like \code{y ~ x}, where \code{"x"} and
#'   \code{"y"} are two column names of \code{data}
#' @param by string, the "by" variable; must be a column name of \code{data}
#' @param palette this can be either the name of a viridis color palette, e.g.
#'   \code{"viridis"}, \code{"cividis"} or \code{"turbo"}
#'   (see \code{\link[viridisLite]{viridis}}), or a vector of colors, or a
#'   function that takes an integer argument (the required number of colors)
#'   and returns a character vector of colors (e.g. you can use
#'   \code{\link[grDevices]{colorRampPalette}})
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
#' @param xAxisTitleDistance a number, controls the distance between the
#'   x-axis and its title
#' @param yAxisTitleDistance a number, controls the distance between the
#'   y-axis and its title
#' @param yAxisShowMaxMin Boolean, whether to show the min and the max on
#'   the y-axis
#' @param yAxisTickFormat a d3 formatting string XXXXXXXXXXXXXXXX
#' @param xLabelsFontSize a CSS measure, the font size of the labels on the
#'   x-axis
#' @param yLabelsFontSize a CSS measure, the font size of the labels on the
#'   y-axis
#' @param rightAlignYaxis Boolean, whether to put the y-axis on the right side
#'   instead of the left
#' @param staggerLabels Boolean, whether to make the x-labels stagger at
#'   different distances from the axis so they're less likely to overlap
#' @param wrapLabels Boolean, whether to split long x-labels by new lines in
#'   order to prevent overlapping
#' @param useInteractiveGuideline Boolean, other kind of tooltips: sets the
#'   chart to use a guideline and floating tooltip instead of requiring the
#'   user to hover over specific hotspots
#' @param width width of the chart container, must be a valid CSS measure
#' @param height height of the chart container, must be a valid CSS measure
#' @param elementId an id for the chart container; commonly useless
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
  xAxisTitleDistance = 35,
  yAxisTitleDistance = -5,
  yAxisShowMaxMin = FALSE,
  yAxisTickFormat = "d",
  xLabelsFontSize = "1rem",
  yLabelsFontSize = "1rem",
  rightAlignYaxis = FALSE,
  staggerLabels = FALSE,
  wrapLabels = FALSE,
  useInteractiveGuideline = FALSE,

  width = NULL, height = NULL, elementId = NULL
) {
  stopifnot(is.null(xAxisTitle) || isString(xAxisTitle))
  stopifnot(is.null(yAxisTitle) || isString(yAxisTitle))
  stopifnot(isNamedList(margins))
  stopifnot(all(names(margins) %in% c("t", "r", "b", "l")))
  stopifnot(isNumber(duration))
  stopifnot(isNumber(rotateLabels))
  stopifnot(isNumber(groupSpacing))
  stopifnot(isNumber(xAxisTitleDistance))
  stopifnot(isNumber(yAxisTitleDistance))
  stopifnot(isBoolean(yAxisShowMaxMin))
  stopifnot(isString(yAxisTickFormat))
  stopifnot(isBoolean(rightAlignYaxis))
  stopifnot(isBoolean(staggerLabels))
  stopifnot(isBoolean(wrapLabels))
  stopifnot(isBoolean(useInteractiveGuideline))

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
    "mbcData"                 = mbcData,
    "xAxisTitle"              = xAxisTitle %or% axisTitles[["x"]],
    "yAxisTitle"              = yAxisTitle %or% axisTitles[["y"]],
    "margins"                 = margins,
    "duration"                = duration,
    "rotateLabels"            = rotateLabels,
    "groupSpacing"            = groupSpacing,
    "xAxisTitleDistance"      = xAxisTitleDistance,
    "yAxisTitleDistance"      = yAxisTitleDistance,
    "yAxisShowMaxMin"         = yAxisShowMaxMin,
    "yAxisTickFormat"         = yAxisTickFormat,
    "xLabelsFontSize"         = xLabelsFontSize,
    "yLabelsFontSize"         = yLabelsFontSize,
    "rightAlignYaxis"         = rightAlignYaxis,
    "staggerLabels"           = staggerLabels,
    "wrapLabels"              = wrapLabels,
    "useInteractiveGuideline" = useInteractiveGuideline
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
