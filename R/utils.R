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
