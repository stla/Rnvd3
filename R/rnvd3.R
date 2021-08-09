#' @title xxx
#'
#' @description xxx
#'
#' @param data
#' @param formula
#' @param by
#' @param palette
#' @param xAxisTitle
#' @param yAxisTitle
#' @param margins
#' @param duration
#' @param rotateLabels
#' @param groupSpacing
#' @param xAxisLabelDistance
#' @param yAxisLabelDistance
#' @param yAxisShowMaxMin
#' @param yAxisTickFormat
#' @param width
#' @param height
#' @param elementId
#'
#' @return
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
    "yAxisTickFormat"    = yAxisTickFormat
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



#' Shiny bindings for rnvd3
#'
#' Output and render functions for using rnvd3 within Shiny
#' applications and interactive Rmd documents.
#'
#' @param outputId output variable to read from
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param expr An expression that generates a rnvd3
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#'
#' @name rnvd3-shiny
#'
#' @export
rnvd3Output <- function(outputId, width = '100%', height = '400px'){
  htmlwidgets::shinyWidgetOutput(outputId, 'rnvd3', width, height, package = 'Rnvd3')
}

#' @rdname rnvd3-shiny
#' @export
renderRnvd3 <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, rnvd3Output, env, quoted = TRUE)
}
