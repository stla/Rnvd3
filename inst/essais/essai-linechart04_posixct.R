library(Rnvd3)

x <- Sys.time() + (1:300)
xRange <- c(Sys.time() - 100, Sys.time() + 400)

dat1 <-
  lineChartData(
    x = x, y = sin(1:300/10), key = "Sine wave", color = "lime"
  )
dat2 <-
  lineChartData(x = x, y = sin(1:300/10)*0.25 + 0.5,
                key = "Another sine wave", color = "darkred")
dat <- list(dat1, dat2)

lineChart(
  dat,
  margins = list(t = 100, r = 100, b = 100, l = 100),
  xAxisTickFormat = "%H:%M:%S",
  xRange = xRange
)
