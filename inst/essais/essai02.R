library(Rnvd3)
dat <- reshape2::melt(
  apply(HairEyeColor, c(1, 2), sum), value.name = "Count"
)
multiBarChart(dat, Count ~ Eye, "Hair",
              legendTitle = "Status:", legendHjust = -50)

tt <- list(
  value = JS(
    "function(x){",
    "  return '<span style=\"color:red;\">' + x + '</span>';",
    "});"
  ),
  header = JS(
    "function(x){",
    "  return '<span style=\"color:green;\">' + x + '</span>';",
    "});"
  ),
  key = JS(
    "function(x){",
    "  return '<i style=\"color:blue;\">' + x + '</i>';",
    "});"
  )
)
