#
# Plotting methods for kriging class
#

setMethod("plot", 
  signature(x="kriging", y="missing"),
  function(x, y, col = terrain.colors(20), title = NA, ...) {
    # Set up the color spectrum
    cols <- x@data$pred.pred - min(x@data$pred.pred)
    cols <- cols * 1/max(cols)
    cols <- col[length(col)*cols]    

    # Use the base plot method
    plot(x@coords, col = cols)
  }
)