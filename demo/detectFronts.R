
# Detect fronts --------------------------------------------------------------------------

# Load example data
load(system.file("extdata", "exampleSSTData.RData", package = "grec"))

# Apply detectFronts function
out <- detectFronts(x = exampleSSTData, intermediate = TRUE, finalSmooth = TRUE)


# Make plots -----------------------------------------------------------------------------

# Define axis values
xAxis <- seq(min(out$original$x), max(out$original$x), 5)
yAxis <- seq(min(out$original$y), max(out$original$y), 2)

# Set plot parameters
par(xaxs = "i", yaxs = "i", mar = rep(0, 4), oma = c(2, 3, 1, 3), mfrow = c(1, 3))

# Plot original map
image(out$original, axes = FALSE, col = colPalette); box()
mtext(text = "Original", side = 3, line = -2, adj = 0.99, cex = 1.2, font = 2)
mtext(text = "SST, Aqua MODIS, Monthly Composite\nMarch, 2010\n[85\u00b0 W - 70\u00b0 W] [20\u00b0 S - 0\u00b0 S]",
      side = 3, line = -8, adj = 0.99)
axis(side = 1, at = xAxis, labels = paste(abs(xAxis), "\u00b0W"))
axis(side = 2, at = yAxis, labels = paste(abs(yAxis), "\u00b0S"), las = 2)

# Plot gradient map
image(out$gradient, axes = FALSE, col = colPalette); box()
mtext(text = "Gradient", side = 3, line = -2, adj = 0.99, cex = 1.2, font = 2)
axis(side = 1, at = xAxis, labels = paste(abs(xAxis), "\u00b0W"))

# Plot gradient with noise cleaning filter applied
image(out$noise_cleared, axes = FALSE, col = colPalette); box()
mtext(text = "Noise cleared", side = 3, line = -2, adj = 0.99, cex = 1.2, font = 2)
axis(side = 1, at = xAxis, labels = paste(abs(xAxis), "\u00b0W"))
axis(side = 4, at = yAxis, labels = paste(abs(yAxis), "\u00b0S"), las = 2)
