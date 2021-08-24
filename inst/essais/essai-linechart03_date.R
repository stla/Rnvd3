library(Rnvd3)

dat1 <-
  lineChartData(x = Sys.Date() + 1:10, y = sin(1:10/10), key = "Sine wave", color = "lime")
dat2 <-
  lineChartData(x = Sys.Date() + 1:10, y = sin(1:10/10)*0.25 + 0.5,
                key = "Another sine wave", color = "red")
dat <- list(dat1, dat2)

lineChart(dat, #yLabelsFontSize = "0.75rem",
          interpolate = "bundle",
          margins = list(t = 100, r = 100, b = 100, l = 100),
          xAxisTickFormat = "%Y-%m-%d")
