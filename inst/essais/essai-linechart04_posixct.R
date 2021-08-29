library(Rnvd3)

dat <- data.frame(
  x  = Sys.time() + (1:300),
  y1 = sin(1:300/10),
  y2 = sin(1:300/10)*0.25 + 0.5
)

xRange <- c(Sys.time() - 100, Sys.time() + 400)

dat1 <-
  lineChartData(x = ~x, y = ~y1, data = dat, key = "Sine wave", color = "lime")
dat2 <-
  lineChartData(x = ~x, y = ~y2, data = dat,
                key = "Another sine wave", color = "darkred")
dat12 <- list(dat1, dat2)

lineChart(
  dat12,
  margins = list(t = 100, r = 100, b = 100, l = 100),
  xAxisTickFormat = "%H:%M:%S",
  xRange = xRange
)
