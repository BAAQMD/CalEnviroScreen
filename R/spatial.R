extract_filename <- function (x) {
  pattern <- str_c("(.*)", .Platform$file.sep, "(.+)[.][^.]+$")
  str_replace(x, pattern, "\\2")
}

#' To unzip and import the "doc.kml" portion of a KMZ file
#' 
#' @param file filename
#' @param layer layer name
#' @export
read_kmz <- function (file, layer) {
  folder_name <- extract_filename(file)
  dn <- cache_path(folder_name)
  unzip(file, exdir=dn)
  readOGR(normalizePath(file.path(dn, "doc.kml")), layer)
}

#' To extract tables from KML data
#' @param x KML
extract_table <- function (x) {
  readHTMLTable(as.character(x))[[2]]
}

#' To extract attributes from simple KML tables
#' @param x KML
extract_attrs <- function (x) {
  setNames(as.character(x[,2]), x[,1])
}

#' To get the IDs of features in a SpatialPolygons* object
#' 
#' @param spobj SpatialPolygons* object
#' @param \dots ignored
#' @rdname IDs
#' @export
IDs <- function (spobj, ...) UseMethod("IDs")

#' @rdname IDs
#' @method IDs SpatialPolygons
#' @export
IDs.SpatialPolygons <- function (spobj, ...) {
  sapply(spobj@polygons, slot, "ID")
}

#' @rdname IDs
#' @method IDs SpatialPolygonsDataFrame
#' @export
IDs.SpatialPolygonsDataFrame <- function (spobj, ...) {
  IDs(as(spobj, "SpatialPolygons"), ...)
}

#' To calculate the area of each (projected) polygon in a SpatialPolygons* object
#' 
#' @param spobj SpatialPolygons* object
#' @param CRSobj for projection
#' @param \dots ignored
#' @rdname areas
#' @export
areas <- function (spobj, CRSobj, ...) UseMethod("areas")

#' @rdname areas
#' @method areas SpatialPolygons
#' @export
areas.SpatialPolygons <- function (spobj, CRSobj, ...) {
  reprojected <- spTransform(spobj, CRSobj, ...)
  results <- sapply(reprojected@polygons, slot, "area")
  setNames(results, IDs(spobj))
}

#' @rdname areas
#' @method areas SpatialPolygonsDataFrame
#' @export
areas.SpatialPolygonsDataFrame <- function (spobj, CRSobj, ...) {
  areas(as(spobj, "SpatialPolygons"), CRSobj, ...)
}

#' To reindex a data.frame or Spatial*DataFrame (effectively replacing row.names)
#' 
#' @param x data.frame or Spatial*DataFrame
#' @param replace named character vector
#' @rdname reindex
#' @export
reindex <- function (x, replace) UseMethod("reindex")

#' @rdname reindex
#' @method reindex data.frame
#' @export
reindex.data.frame <- function (x, replace) {
  i <- match(row.names(x), names(replace))
  row.names(x) <- replace[i]
  return(x)
}

#' @rdname reindex
#' @method reindex SpatialPolygonsDataFrame
#' @export
reindex.SpatialPolygonsDataFrame <- function (x, replace) {
  f <- function (x) { x@ID <- replace[[x@ID]]; return(x) }
  new_geom <- SpatialPolygons(
    rapply(x@polygons, f, classes="Polygons"), 
    proj4string=CRS(proj4string(x))
  )
  new_data <- reindex(x@data, replace)
  SpatialPolygonsDataFrame(new_geom, new_data)  
}

#' Merge a SpatialPolygonsDataFrame with a data.frame
#' @method merge SpatialPolygonsDataFrame
#' @param x SpatialPolygonsDataFrame
#' @param y data.frame
#' @param by character
#' @param \dots ignored 
#' @export
merge.SpatialPolygonsDataFrame <- function (x, y, by, ...) {
  y <- as.data.frame(y)
  if (missing(by)) {
    if (is.null(row.names(x@data)) || is.null(row.names(y))) {
      warning("[merge.SpatialPolygonsDataFrame] merging by position")
      i <- 1:nrow(x@data)
    } else {
      warning("[merge.SpatialPolygonsDataFrame] merging by row names")
      i <- row.names(x@data)
    }
  } else {
    message("[merge.SpatialPolygonsDataFrame] merging by ", by)
    i <- match(x@data[,by], y[,by])
  }
  new_data <- data.frame(x@data, y[i,])
  row.names(new_data) <- row.names(x@data)
  SpatialPolygonsDataFrame(geometry(x), new_data)
}
.null <- capture.output(
  setMethod("merge", c("SpatialPolygonsDataFrame", "data.frame"), merge.SpatialPolygonsDataFrame),
  file = NULL
)


