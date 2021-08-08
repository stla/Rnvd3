library(Rnvd3)

dat <- reshape2::melt(
  apply(HairEyeColor, c(1, 2), sum), value.name = "Count"
)


rnvd3(dat, Eye ~ Count, "Hair", palette = "viridis")

