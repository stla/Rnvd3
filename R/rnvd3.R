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
#' @param yAxisTickFormat a d3 formatting string for the y-axis; see
#'   \href{https://d3-wiki.readthedocs.io/zh_CN/master/Formatting/#d3_format}{d3.format}
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
#' @note In Shiny, you can style the axis titles with the help of CSS; see the
#'   \link[=rnvd3Output]{shiny example}.
#'
#' @import htmlwidgets
#' @export
#'
#' @examples library(Rnvd3)
#' dat <- reshape2::melt(
#'   apply(HairEyeColor, c(1, 2), sum), value.name = "Count"
#' )
#' multiBarChart(dat, Count ~ Eye, by = "Hair")
#'
#' # style with CSS
#' library(htmlwidgets)
#' library(htmltools)
#'
#' CSS <- HTML(
#'   ".nvd3 .nv-axis.nv-x text.nv-axislabel,
#'    .nvd3 .nv-axis.nv-y text.nv-axislabel {
#'      font-size: 2rem;
#'      fill: red;
#'   }"
#' )
#'
#' widget <- multiBarChart(
#'   dat, Count ~ Eye, "Hair", palette = "turbo"
#' )
#' prependContent(
#'   widget,
#'   tags$style(CSS)
#' )
multiBarChart <- function(
  data,
  formula,
  by,
  palette = "viridis",
  # title = NULL,
  # titleOffset = 0,
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
  # stopifnot(is.null(title) || isString(title))
  # stopifnot(isNumber(titleOffset))
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
  x <- list(
    "chart"                   = "multibarchart",
    "Data"                    = mbcData,
    # "title"                   = title,
    # "titleOffset"             = titleOffset,
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

#' @title Horizontal multibar chart
#'
#' @description HTMLwidget displaying a horizontal multibar chart.
#'
#' @param data dataframe containing the data used for the chart
#' @param formula a two-sided formula like \code{x ~ y}, where \code{"x"} and
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
#' @param groupSpacing a number, controls the distance between groups of bars
#' @param xAxisTitleDistance a number, controls the distance between the
#'   x-axis and its title
#' @param yAxisTitleDistance a number, controls the distance between the
#'   y-axis and its title
#' @param xAxisShowMaxMin Boolean, whether to show the min and the max on
#'   the y-axis
#' @param xAxisTickFormat a d3 formatting string for the y-axis; see
#'   \href{https://d3-wiki.readthedocs.io/zh_CN/master/Formatting/#d3_format}{d3.format}
#' @param xLabelsFontSize a CSS measure, the font size of the labels on the
#'   x-axis
#' @param yLabelsFontSize a CSS measure, the font size of the labels on the
#'   y-axis
#' @param showValues Boolean, whether to show the values next to the bars
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
#' hMultiBarChart(dat, Count ~ Eye, by = "Hair")
hMultiBarChart <- function(
  data,
  formula,
  by,
  palette = "viridis",
  xAxisTitle = NULL,
  yAxisTitle = NULL,
  margins = list(b = 100, l = 100),
  duration = 1300,
  groupSpacing = 0.1,
  xAxisTitleDistance = -5,
  yAxisTitleDistance = 35,
  xAxisShowMaxMin = FALSE,
  xAxisTickFormat = "d",
  xLabelsFontSize = "1rem",
  yLabelsFontSize = "1rem",
  showValues = FALSE,

  width = NULL, height = NULL, elementId = NULL
) {
  stopifnot(is.null(xAxisTitle) || isString(xAxisTitle))
  stopifnot(is.null(yAxisTitle) || isString(yAxisTitle))
  stopifnot(isNamedList(margins))
  stopifnot(all(names(margins) %in% c("t", "r", "b", "l")))
  stopifnot(isNumber(duration))
  stopifnot(isNumber(groupSpacing))
  stopifnot(isNumber(xAxisTitleDistance))
  stopifnot(isNumber(yAxisTitleDistance))
  stopifnot(isBoolean(xAxisShowMaxMin))
  stopifnot(isString(xAxisTickFormat))
  stopifnot(isBoolean(showValues))

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
  x <- list(
    "chart"                   = "horizontalmultibarchart",
    "Data"                    = mbcData,
    "xAxisTitle"              = yAxisTitle %or% axisTitles[["x"]],
    "yAxisTitle"              = xAxisTitle %or% axisTitles[["y"]],
    "margins"                 = margins,
    "duration"                = duration,
    "groupSpacing"            = groupSpacing,
    "xAxisTitleDistance"      = xAxisTitleDistance,
    "yAxisTitleDistance"      = yAxisTitleDistance,
    "xAxisShowMaxMin"         = xAxisShowMaxMin,
    "xAxisTickFormat"         = xAxisTickFormat,
    "xLabelsFontSize"         = xLabelsFontSize,
    "yLabelsFontSize"         = yLabelsFontSize,
    "showValues"              = showValues
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

#' @title Line chart
#' @description Create a HTML widget displaying a line chart.
#'
#' @param data data used fir the chart; it must be a list created with
#'   \code{\link{lineChartData}}, or a list os such lists (for multiple lines)
#' @param xAxisTitle string, the title of the x-axis
#' @param yAxisTitle string, the title of the y-axis
#' @param margins a named list defining the margins, with names \code{"t"},
#'   \code{"r"}, \code{"b"} and \code{"l"}, for "top", "right", "bottom"
#'   and "left" respectively; you can specify only certain margins in the list
#'   to change just those parts
#' @param duration transition duration in milliseconds
#' @param useInteractiveGuideline Boolean, a guideline and synchronized tooltips
#' @param xAxisTickFormat a d3 formatting string for the ticks on the x-axis
#' @param yAxisTickFormat a d3 formatting string for the ticks on the y-axis
#' @param width chart width
#' @param height chart height
#' @param elementId an id for the chart container, usually useless
#'
#' @return A HTML widget displaying a line chart.
#' @export
#'
#' @examples
lineChart <- function(
  data,
  xAxisTitle = "x",
  yAxisTitle = "y",
  margins = list("l" = 100),
  duration = 500,
  useInteractiveGuideline = TRUE,
  xAxisTickFormat = "d",
  yAxisTickFormat = ".02f",
  width = NULL, height = NULL, elementId = NULL
){
  lcData <- makeLineChartData(data)
  stopifnot(isNamedList(margins))
  stopifnot(all(names(margins) %in% c("t", "r", "b", "l")))
  margins <- dropNulls(
    list(
      "top"    = margins[["t"]],
      "right"  = margins[["r"]],
      "bottom" = margins[["b"]],
      "left"   = margins[["l"]]
    )
  )
  stopifnot(is.null(xAxisTitle) || isString(xAxisTitle))
  stopifnot(is.null(yAxisTitle) || isString(yAxisTitle))
  stopifnot(isNumber(duration))
  stopifnot(isBoolean(useInteractiveGuideline))
  stopifnot(isString(xAxisTickFormat))
  stopifnot(isString(yAxisTickFormat))


  # forward options using x
  x <- list(
    "chart"                   = "linechart",
    "Data"                    = lcData,
    "xAxisTitle"              = xAxisTitle,
    "yAxisTitle"              = yAxisTitle,
    "margins"                 = margins,
    "duration"                = duration,
    "useInteractiveGuideline" = useInteractiveGuideline,
    "xAxisTickFormat"         = xAxisTickFormat,
    "yAxisTickFormat"         = yAxisTickFormat
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
