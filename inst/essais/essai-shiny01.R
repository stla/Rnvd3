library(Rnvd3)
library(shiny)

dat <- reshape2::melt(
  apply(HairEyeColor, c(1, 2), sum), value.name = "Count"
)

CSS <- HTML(
  ".nvd3 .nv-axis.nv-x text.nv-axislabel,
   .nvd3 .nv-axis.nv-y text.nv-axislabel {
     font-size: 3rem;
     fill: red;
  }"
)

ui <- fluidPage(
  tags$head(tags$style(CSS)),
  br(),
  rnvd3Output("mychart", width = "600px", height = "500px")
)

server <- function(input, output, session){

  output[["mychart"]] <- renderRnvd3({
    multiBarChart(
      dat, Count ~ Eye, "Hair", palette = "viridis",
      xLabelsFontSize = "2rem", yLabelsFontSize = "2rem"
    )
  })

}

shinyApp(ui, server)

