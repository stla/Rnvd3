#' @title Line chart data
#' @description Make line chart data.
#'
#' @param x a right-sided formula giving the variable on the x-axis, numeric or
#'   date type
#' @param y a right-sided formula giving the variable on the x-axis, numeric
#'   type
#' @param data dataframe containing the data for the chart; if not \code{NULL},
#'   the variables passed to \code{x} and \code{y} are searched among the
#'   columns of \code{data}
#' @param key string, the title of the line chart
#' @param color string, the color of the line chart
#' @param area Boolean, whether to turn the line chart into a filled area chart
#'
#' @return A list, for usage in \code{\link{lineChart}}.
#'
#' @note The color can be given by the name of a R color, the name of a CSS
#'   color, e.g. \code{"lime"} or \code{"fuchsia"}, an HEX code like
#'   \code{"#ff009a"}, a RGB code like \code{"rgb(255,100,39)"}, or a HSL code
#'   like \code{"hsl(360,11,255)"}.
#' @importFrom lubridate is.Date is.POSIXct year month day hour minute second
#' @importFrom lazyeval f_eval_rhs is_formula
#' @export
lineChartData <- function(x, y, data = NULL, key, color, area = FALSE){
  stopifnot(is_formula(x))
  stopifnot(is_formula(y))
  x <- f_eval_rhs(x, data = data)
  y <- f_eval_rhs(y, data = data)
  stopifnot(is.numeric(x) || is.Date(x) || is.POSIXct(x))
  stopifnot(is.numeric(y))
  stopifnot(length(x) == length(y))
  stopifnot(isString(key))
  stopifnot(isString(color))
  color <- validateColor(color)
  stopifnot(isBoolean(area))
  isDate <- is.Date(x)
  isPOSIXct <- is.POSIXct(x)
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
  }else if(isPOSIXct){
    out <- list(
      "values" = data.frame(
        "year"   = year(x),
        "month"  = month(x),
        "day"    = day(x),
        "hour"   = hour(x),
        "minute" = minute(x),
        "second" = second(x),
        "y"      = y
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
  attr(out, "isPOSIXct") <- isPOSIXct
  out
}

check_lineChartData <- function(data){
  if(isNamedList(data)){
    good <- isTRUE(attr(data, "linechartdata"))
    if(!good){
      return(FALSE)
    }
    out <- TRUE
    attr(out, "isDate")    <- attr(data, "isDate")
    attr(out, "isPOSIXct") <- attr(data, "isPOSIXct")
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
    attrs <- unlist(lapply(data, attr, which = "isPOSIXct"))
    good <- isTRUE(all.equal(attrs, rep(TRUE, n))) ||
      isTRUE(all.equal(attrs, rep(FALSE, n)))
    if(!good){
      return(FALSE)
    }
    out <- TRUE
    attr(out, "isDate")    <- attr(data[[1L]], "isDate")
    attr(out, "isPOSIXct") <- attr(data[[1L]], "isPOSIXct")
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
  attr(out, "isDate")    <- attr(check, "isDate")
  attr(out, "isPOSIXct") <- attr(check, "isPOSIXct")
  out
}
