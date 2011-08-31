#
# Kriging using an object of class SpatialPixels
# Returns gstat object

# Generic function
setGeneric("kriging",
  function(space, response, coordinates, ...)
    standardGeneric("kriging")
)

# New class definition
setClass("kriging",
  representation(title = "character"),
  contains = "SpatialPixelsDataFrame"
)

# Create a gstat object 
setMethod("kriging",
  signature(space = "SpatialPixels", response = "numeric", coordinates = "data.frame"),
  function(space, response, coordinates, variomodel = "Lin", checkvariogram = FALSE) {
    # Create a new data frame
    data <- data.frame(pred = response, long = coordinates[,1], lat = coordinates[,2])

    # Convert from data.frame to SpatialPointsDataFrame object
    # using the predictors from the model
    coordinates(data) <- ~long+lat

    # gstat object for dependent variable
    h <- gstat(id="pred", formula = pred~1, data = data)

    # Fit a variogram
    w <- variogram(h);
    w.fit <- fit.variogram(w, model = vgm(model = variomodel));

    if(checkvariogram) {
      return(plot(w, w.fit))
    }

    # Perform ordinary kriging prediction
    q <- predict.gstat(h, model = w.fit, newdata = space);

    # Return as kriging object
    q <-  new("kriging", q)
    return(q)
  }
)




