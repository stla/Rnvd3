library(Rnvd3)

dat <- structure(list(x = c("Pr1", "Pr1", "Pr2", "Pr2", "Pr3", "Pr3"), bby = c(
  "Complete", "In progress", "Complete", "In progress",
  "Complete", "In progress"
), v = c(2L, 2L, 2L, 1L, 2L, 1L)), row.names = c(
  NA,
  -6L
), class = "data.frame")

rnvd3(dat, v ~ x, "bby", palette = "turbo")
