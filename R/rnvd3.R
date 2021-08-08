#' <Add Title>
#'
#' <Add Description>
#'
#' @import htmlwidgets
#'
#' @export
rnvd3 <- function(
  data, formula, by, palette = "viridis",
  width = NULL, height = NULL, elementId = NULL
) {

  # forward options using x
  x = list(
    mbcData = multiBarChartData(data, formula, by, palette)
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
  tags$div(id = id, class = class, style = style, tags$svg())
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
