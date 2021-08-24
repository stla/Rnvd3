#' @title Line chart data
#' @description Make line chart data.
#'
#' @param x values on the x-axis, numeric or date type
#' @param y values on the y-axis
#' @param key string, the title of the line chart
#' @param color line chart color
#' @param area Boolean, whether to turn the line chart into a filled area chart
#'
#' @return A list, for usage in \code{\link{lineChart}}.
#' @importFrom lubridate is.Date is.POSIXct year month day hour minute second
#' @export
lineChartData <- function(x, y, key, color, area = FALSE){
  stopifnot(is.numeric(x) || is.Date(x) || is.POSIXct(x))
  stopifnot(is.numeric(y))
  stopifnot(length(x) == length(y))
  stopifnot(isString(key))
  stopifnot(isString(color))
  color <- validateColor(color)
  stopifnot(isBoolean(area))
  isDate <- is.Date(x)
  if(isDate){
    out <- list(
      "values" = data.frame(
        "year"  = year(x),
        "month" = month(x),
        "day"   = day(x),
        "y"     = y
      ),
      "key"    = key,
      "color"  = color,
      "area"   = area
    )
  }else{
    out <- list(
      "values" = data.frame(x = x, y = y),
      "key"    = key,
      "color"  = color,
      "area"   = area
    )
  }
  attr(out, "linechartdata") <- TRUE
  attr(out, "isDate") <- isDate
  out
}

check_lineChartData <- function(data){
  if(isNamedList(data)){
    good <- isTRUE(attr(data, "linechartdata"))
    if(!good){
      return(FALSE)
    }
    out <- TRUE
    attr(out, "isDate") <- attr(data, "isDate")
    return(out)
  }
  if(isUnnamedList(data)){
    attrs <- unlist(lapply(data, attr, which = "linechartdata"))
    n <- length(data)
    good <- isTRUE(all.equal(attrs, rep(TRUE, n)))
    if(!good){
      return(FALSE)
    }
    attrs <- unlist(lapply(data, attr, which = "isDate"))
    good <- isTRUE(all.equal(attrs, rep(TRUE, n))) ||
      isTRUE(all.equal(attrs, rep(FALSE, n)))
    if(!good){
      return(FALSE)
    }
    out <- TRUE
    attr(out, "isDate") <- attr(data[[1L]], "isDate")
    return(out)
  }
  return(FALSE)
}

#' @importFrom jsonlite toJSON
#' @noRd
makeLineChartData <- function(data){
  check <- check_lineChartData(data)
  if(!check){
    stop("Invalid line chart data.", call. = FALSE)
  }
  if(isNamedList(data)){
    data <- list(data)
  }
  out <- as.character(toJSON(data, auto_unbox = TRUE, digits = NA))
  attr(out, "isDate") <- attr(check, "isDate")
  out
}
