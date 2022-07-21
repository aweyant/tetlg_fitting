# TAKEN COMPLETELY FROM https://www.dataanalytics.org.uk/plot-two-overlapping-histograms-on-one-chart-in-r/

plot_double_hist <- function(A, B, title = "", subtitle ="") {
  c1 <- rgb(173,216,230,max = 255, alpha = 80, names = "lt.blue")
  c2 <- rgb(255,192,203, max = 255, alpha = 80, names = "lt.pink")
  
  hist_min <- min(c(A,B))# Set the minimum for the breakpoints
  hist_max <- max(c(A,B)) # Set the maximum for the breakpoints
  #ax <- pretty(b:e, n = 12) # Make a neat vector for the breakpoints
  ax <-seq(hist_min - 0.5, hist_max + 0.5, by = 0.5)
  ax
  
  hgA <- hist(A, breaks = ax, plot = FALSE, freq = TRUE) # Save first histogram data
  hgB <- hist(B, breaks = ax, plot = FALSE, freq = TRUE) 
  
  plot(hgA, col = c1,
       main = title,
       sub = subtitle) # Plot 1st histogram using a transparent color
  plot(hgB, col = c2, add = TRUE)
}

