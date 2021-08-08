#' @import data.table
#' @noRd
check_multiBarChartData <- function(dat, category, by){
  dat0 <- dat[, c(category, by)]
  DT0 <- as.data.table(dat0)
  DT1 <- DT0[, list("count" = .N), by = c(category, by)][, c(1L,2L)]
  isTRUE(
    all.equal(DT0, DT1, ignore.row.order = TRUE, check.attributes = FALSE)
  )
}

#' @importFrom lazyeval is_formula f_lhs f_rhs
#' @importFrom viridisLite viridis
#' @importFrom jsonlite toJSON
#' @import data.table
#' @noRd
multiBarChartData <- function(data, formula, by, palette){
  stopifnot(isString(palette))
  palette <- match.arg(
    palette,
    c("magma", "inferno", "plasma", "viridis", "cividis", "rocket", "mako", "turbo")
  )
  stopifnot(is.data.frame(data))
  stopifnot(is_formula(formula))
  stopifnot(isString(by))
  dataColumns <- colnames(data)
  y <- as.character(f_lhs(formula))
  if(!isString(y) || y %notin% dataColumns){
    stop(
      "Invalid formula.",
      call. = FALSE
    )
  }
  category <- as.character(f_rhs(formula))
  if(!isString(category) || category %notin% dataColumns){
    stop(
      "Invalid formula.",
      call. = FALSE
    )
  }
  check <- check_multiBarChartData(dat = data, category = category, by = by)
  if(!check){
    stop(
      "Invalid data.",
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
  DT2[, `:=`(color = viridis(nrow(DT2), option = palette))]
  out <- as.character(toJSON(DT2))
  attr(out, "axisTitles") <- c("x" = category, "y" = y)
  out
}
