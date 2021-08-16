library(Rnvd3)
dat <- reshape2::melt(
  apply(HairEyeColor, c(1, 2), sum), value.name = "Count"
)
multiBarChart(dat, Count ~ Eye, "Hair",
              legendTitle = "Status:", legendHjust = -50)

