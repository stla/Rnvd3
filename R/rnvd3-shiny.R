#' @title Shiny bindings for rnvd3
#'
#' @description Output and render functions for using \code{rnvd3} widgets
#'   within Shiny applications and interactive Rmd documents.
#'
#' @param outputId output variable to read from
#' @param width,height must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended
#' @param expr an expression that generates a \code{rnvd3} widget
#' @param env the environment in which to evaluate \code{expr}
#' @param quoted is \code{expr} a quoted expression (with \code{quote()})
#'
#' @return \code{rnvd3Output} returns an output element that can be included
#'   in a Shiny UI definition, and \code{renderRnvd3} returns a
#'   \code{shiny.render.function} object that can be included in a Shiny server
#'   definition.
#'
#' @name rnvd3-shiny
#'
#' @export
#' @examples
#' library(Rnvd3)
#' library(shiny)
#'
#' dat <- reshape2::melt(
#'   apply(HairEyeColor, c(1, 2), sum), value.name = "Count"
#' )
#'
#' CSS <- HTML(
#'   "body {
#'     overflow: overlay;
#'   }
#'   /* style axis titles */
#'   .nvd3 .nv-axis.nv-x text.nv-axislabel,
#'    .nvd3 .nv-axis.nv-y text.nv-axislabel {
#'      font-size: 3rem;
#'      fill: red;
#'   }
#'   /* style the tooltip */
#'   .nvtooltip .value {
#'     color: red;
#'   }
#'   .nvtooltip .x-value {
#'     color: green;
#'   }
#'   .nvtooltip .key {
#'     color: blue;
#'     font-style: italic;
#'   }
#'   "
#' )
#'
#' ui <- fluidPage(
#'   tags$head(tags$style(CSS)),
#'   br(),
#'   fluidRow(
#'     column(
#'       9,
#'       rnvd3Output("mychart", width = "100%", height = "500px")
#'     ),
#'     column(
#'       3,
#'       tags$h3("Chart state:"),
#'       verbatimTextOutput("state")
#'     )
#'   )
#' )
#'
#' server <- function(input, output, session){
#'
#'   output[["mychart"]] <- renderRnvd3({
#'     multiBarChart(
#'       dat, Count ~ Eye, "Hair", palette = "viridis",
#'       xLabelsFontSize = "2rem", yLabelsFontSize = "2rem"
#'     )
#'   })
#'
#'   output[["state"]] <- renderPrint({
#'     input[["mychart_state"]]
#'   })
#'
#' }
#'
#' if(interactive()){
#'   shinyApp(ui, server)
#' }
rnvd3Output <- function(outputId, width = '100%', height = '400px'){
  htmlwidgets::shinyWidgetOutput(outputId, 'rnvd3', width, height, package = 'Rnvd3')
}

#' @rdname rnvd3-shiny
#' @export
renderRnvd3 <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, rnvd3Output, env, quoted = TRUE)
}
