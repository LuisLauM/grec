rm(list = ls()); gc(reset = TRUE)
# Detect fronts --------------------------------------------------------------------------
# Load example data
data(sst)
exampleChlData <- list(x = chl$longitude,
                       y = chl$latitude,
                       z = chl$chlorophyll[,,1])

# Apply detectFronts function
out <- detectFronts(x = exampleChlData, intermediate = FALSE)
str(out)

# Make plots -----------------------------------------------------------------------------

xlim <- round(range(exampleChlData$x), 0)
ylim <- round(range(exampleChlData$y), 0)

# Define axis values
xAxis <- seq(xlim[1], xlim[2], 5)
yAxis <- seq(ylim[1], ylim[2], 2)

# Set plot parameters
par(xaxs = "i", yaxs = "i", mar = rep(0, 4), oma = c(2, 3, 2, 1), mfrow = c(1, 3))

# Plot original map
image(exampleChlData, axes = FALSE, col = colPalette, xlim = xlim, ylim = ylim); box()
mtext(text = "Original", side = 3, line = -2, adj = 0.99, cex = 1.2, font = 2)
mtext(text = "Chlorophyll-a, Aqua MODIS,\nMonthly Composite\nMarch, 2010\n[85\u00b0 W - 70\u00b0 W] [20\u00b0 S - 0\u00b0 S]",
      side = 3, line = -8, adj = 0.99)
axis(side = 1, at = xAxis, labels = paste(abs(xAxis), "\u00b0W"))
axis(side = 2, at = yAxis, labels = paste(abs(yAxis), "\u00b0S"), las = 2)

# Plot gradient map
image(out, axes = FALSE, col = colPalette, xlim = xlim, ylim = ylim); box()
mtext(text = "Gradient from\nBelkin & O'Reilly (2009)", side = 3, line = -4,
      adj = 0.99, cex = 1.2, font = 2)
axis(side = 3, at = xAxis, labels = paste(abs(xAxis), "\u00b0W"))

# Plot gradient map in logaritmic scale
logOut <- out
logOut$z <- log10(logOut$z)
image(logOut, axes = FALSE, col = colPalette, xlim = xlim, ylim = ylim); box()
mtext(text = "Gradient (log scale) from\nBelkin & O'Reilly (2009)", side = 3,
      line = -4, adj = 0.99, cex = 1.2, font = 2)
axis(side = 1, at = xAxis, labels = paste(abs(xAxis), "\u00b0W"))
