#' library(Rnvd3)
#' library(htmlwidgets)
#' library(htmltools)
#'
#' dat <- reshape2::melt(
#'   apply(HairEyeColor, c(1, 2), sum), value.name = "Count"
#' )
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
#'   dat, Count ~ Eye, "Hair", palette = "viridis"
#' )
#'
#' prependContent(
#'   widget,
#'   tags$style(CSS)
#' )
