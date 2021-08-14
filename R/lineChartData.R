#' @title Line chart data
#' @description Make line chart data.
#'
#' @param x values on the x-axis
#' @param y values on the y-axis
#' @param key string, the title of the line chart
#' @param color line chart color
#' @param area Boolean, whether to turn the line chart into a filled area chart
#'
#' @return A list, for usage in \code{\link{lineChart}}.
#' @export
lineChartData <- function(x, y, key, color, area = FALSE){
  stopifnot(is.numeric(x))
  stopifnot(is.numeric(y))
  stopifnot(length(x) == length(y))
  stopifnot(isString(key))
  stopifnot(isString(color))
  color <- validateColor(color)
  stopifnot(isBoolean(area))
  out <- list(
    "values" = data.frame(x = x, y = y),
    "key"    = key,
    "color"  = color,
    "area"   = area
  )
  attr(out, "linechartdata") <- TRUE
  out
}

check_lineChartData <- function(data){
  if(isNamedList(data)){
    return(isTRUE(attr(data, "linechartdata")))
  }else if(isUnnamedList(data)){
    attrs <- unlist(lapply(data, attr, which = "linechartdata"))
    n <- length(data)
    return(isTRUE(all.equal(attrs, rep(TRUE, n))))
  }else{
    return(FALSE)
  }
}

#' @importFrom jsonlite toJSON
#' @noRd
makeLineChartData <- function(data){
  if(!check_lineChartData(data)){
    stop("Invalid line chart data.", call. = FALSE)
  }
  if(isNamedList(data)){
    data <- list(data)
  }
  as.character(toJSON(data, auto_unbox = TRUE, digits = NA))
}
