#' @param tooltipFormatters formatters for the tooltip; each formatter must
#'   be \code{NULL} for the default formatting, otherwise a JavaScript function
#'   created with \code{\link{JS}}; there are three possible formatters
#'   (see the example):
#'   \describe{
#'     \item{value}{formatter for the y-value displayed in the tooltip}
#'     \item{header}{formatter for the tooltip header (this is the x-value)}
#'     \item{key}{formatter for the value of the 'by' variable}
#'   }
