`%notin%` <- function(x, set){
  !is.element(x, set)
}

dropNulls <- function(x){
  Filter(Negate(is.null), x)
}

`%or%` <- function(x, y){
  if(is.null(x)) y else x
}

isString <- function(x){
  is.character(x) && (length(x) == 1L) && !is.na(x)
}

isBoolean <- function(x){
  is.logical(x) && (length(x) == 1L) && !is.na(x)
}

isNumber <- function(x){
  is.numeric(x) && (length(x) == 1L) && !is.na(x)
}

isNamedList <- function(x){
  is.list(x) && !is.null(names(x)) && all(names(x) != "")
}

#' @importFrom grDevices col2rgb
#' @noRd
color2hex <- function(color){
  RGB <- col2rgb(color)[,1L]
  rgb(RGB["red"], RGB["green"], RGB["blue"], maxColorValue = 255)
}

#' @importFrom htmltools parseCssColors
#' @noRd
validateColor <- function(color){
  stopifnot(isString(color))
  cssColor <- try(parseCssColors(color), silent = TRUE)
  if(!inherits(cssColor, "try-error")){
    return(cssColor)
  }
  Rcolor <- try(color2hex(color), silent = TRUE)
  if(inherits(Rcolor, "try-error")){
    stop(sprintf("Invalid color '%s'.", color), call. = FALSE)
  }else{
    Rcolor
  }
}
