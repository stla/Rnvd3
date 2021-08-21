library(Rnvd3)
dat <- reshape2::melt(
  apply(HairEyeColor, c(1, 2), sum), value.name = "Count"
)
multiBarChart(
  dat, Count ~ Eye, "Hair",
  tooltipFormatters = list(
    value = JS( #.value
      "function(x){",
      "  return '<span style=\"color:red;\">' + x + '</span>';",
      "}"
    ),
    header = JS( #.x-value
      "function(x){",
      "  return '<span style=\"color:green;\">' + x + '</span>';",
      "}"
    ),
    key = JS( #.key
      "function(x){",
      "  return '<i style=\"color:blue;\">' + x + '</i>';",
      "}"
    )
  )
)
