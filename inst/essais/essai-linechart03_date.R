library(Rnvd3)

dat1 <-
  lineChartData(
    x = Sys.Date() + 1:100, y = sin(1:100/10), key = "Sine wave", color = "lime"
  )
dat2 <-
  lineChartData(x = Sys.Date() + 1:100, y = sin(1:100/10)*0.25 + 0.5,
                key = "Another sine wave", color = "darkred")
dat <- list(dat1, dat2)

lineChart(
  dat,
  margins = list(t = 100, r = 100, b = 100, l = 100),
  xAxisTickFormat = "%Y-%m-%d"
)
