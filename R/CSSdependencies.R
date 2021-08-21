#' @importFrom htmltools htmlDependency
#' @noRd
CSSdependencies <- function(tooltipTransitions, tooltipShadow){
  dependencies <- NULL
  if(tooltipTransitions){
    dependencies <- list(
      htmlDependency(
        name = "withTranstions",
        version = as.character(packageVersion("Rnvd3")),
        src = file.path("htmlwidgets", "css"),
        stylesheet = "withTransitions.css",
        package = "Rnvd3",
        all_files = FALSE
      )
    )
  }
  if(tooltipShadow){
    dependencies <- c(dependencies, list(
      htmlDependency(
        name = "withShadow",
        version = as.character(packageVersion("Rnvd3")),
        src = file.path("htmlwidgets", "css"),
        stylesheet = "with3dShadow.css",
        package = "Rnvd3",
        all_files = FALSE
      )
    ))
  }
  dependencies
}
