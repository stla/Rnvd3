#' @importFrom data.table as.data.table
#' @noRd
check_multiBarChartData <- function(dat, category, by){
  dat0 <- dat[, c(category, by)]
  DT0 <- as.data.table(dat0)
  DT1 <- DT0[, list("count" = .N), by = c(category, by)][, c(1L,2L)]
  isTRUE(
    all.equal(
      DT0, DT1, ignore.row.order = TRUE, check.attributes = FALSE
    )
  )
}

#' @importFrom lazyeval is_formula f_lhs f_rhs
#' @importFrom viridisLite viridis
#' @importFrom jsonlite toJSON
#' @importFrom data.table as.data.table `:=` data.table .N
#' @noRd
multiBarChartData <- function(data, formula, by, palette){
  viridisPalette <- FALSE
  if(isString(palette)){
    palette <- match.arg(
      palette,
      c(
        "magma", "inferno", "plasma", "viridis",
        "cividis", "rocket", "mako", "turbo"
      )
    )
    viridisPalette <- TRUE
  }else if(is.character(palette)){
    colors <- vapply(palette, validateColor, character(1L))
  }else if(!is.function(palette)){
    stop("Invalid 'palette' argument.", call. = FALSE)
  }
  stopifnot(is.data.frame(data))
  stopifnot(is_formula(formula))
  stopifnot(isString(by))
  dataColumns <- colnames(data)
  y <- as.character(f_lhs(formula))
  if(!isString(y) || y %notin% dataColumns){
    stop(
      "Invalid formula. ",
      if(isString(y)) sprintf("There is no column '%s' in the data.", y),
      call. = FALSE
    )
  }
  category <- as.character(f_rhs(formula))
  if(!isString(category) || category %notin% dataColumns){
    stop(
      "Invalid formula. ",
      if(isString(category)){
        sprintf("There is no column '%s' in the data.", category)
      },
      call. = FALSE
    )
  }
  check <- check_multiBarChartData(dat = data, category = category, by = by)
  if(!check){
    stop(
      "Invalid data (not aggregated).",
      call. = FALSE
    )
  }
  cols <- c(category, y, by)
  dat <- data[, cols]
  names(dat)[c(1L, 2L)] <- c("...X...", "...Y...")
  DT1 <- as.data.table(dat)
  DT2 <- DT1[
    ,
    list(values = list(data.table(label = `...X...`, value = `...Y...`))),
    by = by
  ]
  names(DT2)[1L] <- "key"
  n <- nrow(DT2)
  if(viridisPalette){
    colors <- viridis(nrow(DT2), option = palette)
  }else if(is.character(palette)){
    if(length(colors) != n){
      stop(
        sprintf(
          "Invalid number of colors: %d given, %d expected.",
          length(colors), n
        ),
        call. = FALSE
      )
    }
  }else{
    colors <- try(palette(n))
    if(
      inherits(colors, "try-error") ||
      !is.character(colors) ||
      length(colors) != n
    ){
      stop(
        "Your 'palette' function is invalid.", call. = FALSE
      )
    }else{
      colors <- vapply(colors, validateColor, character(1L))
    }
  }
  DT2[, `:=`(color = colors)]
  out <- as.character(toJSON(DT2, digits = NA))
  attr(out, "axisTitles") <- c("x" = category, "y" = y)
  out
}
