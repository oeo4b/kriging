#
# Methods to convert Map object to gridded SpatialPixels object 
#

# Generic function
setGeneric("gridshape",
  function(shape, ...)
    standardGeneric("gridshape")
)

# Grid shape file by zip code
setMethod("gridshape", signature(shape="Map"),
  function(shape, zipcodes = NA, ratio = 0.025) {
    # Get id for each required zip
    zips <- as.numeric(as.character(shape$att.data$NAME))

    # Find the right indexes; default to all
    ids <- {}
    if(!is.na(zipcodes[1])) {
      for(i in 1:length(zips)) {
        for(j in 1:length(zipcodes)) {
          if(!is.na(zips[i])) {
            if(zips[i]==zipcodes[j]) ids <- c(ids, i)
          }
        }
      }
    }
    else {
      ids <- 1:length(zips)
    }

    # Range of coordinates
    longrange <- latrange <- {}
    for(i in ids) {
      longrange<-c(longrange, shape$Shape[[i]]$bbox[1], shape$Shape[[i]]$bbox[3])
      latrange<-c(latrange, shape$Shape[[i]]$bbox[2], shape$Shape[[i]]$bbox[4])
    }
    longrange <- range(longrange)
    latrange <- range(latrange)

    # Grid for the whole range
    detail <- ratio*max(diff(longrange), diff(latrange))
    grd <- expand.grid(
      x=seq(from=longrange[1], to=longrange[2], by=detail),
      y=seq(from=latrange[1], to=latrange[2], by=detail))

    # Point-in-polygon
    x <- y <- {}
    for(i in ids) {
      # This call has O(exp(n)) -- careful with the ratio setting
      p <- pip(grd, shape$Shape[[i]]$verts)
      x <- c(x, p$x)
      y <- c(y, p$y)
    }

    # Re-grid data
    grd <- data.frame(x, y)
    coordinates(grd) <- ~x+y

    # Returned as SpatialPixels class
    gridded(grd) <- TRUE
    return(grd)
  }
)

