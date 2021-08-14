library(Rnvd3)

dat1 <-
  lineChartData(x = 1:100, y = sin(1:100/10), key = "Sine wave", color = "blue")
dat2 <-
  lineChartData(x = 1:100, y = sin(1:100/10)*0.25 + 0.5,
                key = "Another sine wave", color = "red")
dat <- list(dat1, dat2)

lineChart(dat)
