cache_path <- function (...) rel_path("cache", ...)

cache_contains <- function (key) {
  file <- cache_path(key)
  return(file.exists(file))
}

cache_put <- function (object, key) {
  file <- cache_path(key)
  dn <- dirname(file)
  if (!file.exists(dn)) dir.create(dn, showWarnings=TRUE, recursive=TRUE)
  saveRDS(object, file, compress="xz")
  return(object)
}

cache_get <- function (key, quiet=FALSE) {
  if (cache_contains(key)) {
    file <- cache_path(key)
    return(readRDS(file))
  }
  return(NULL)
}

cached <- function(key, expr, quiet=FALSE) {
  if (is.null(result <- cache_get(key))) {
    result <- force(expr)
    cache_put(result, key)
  }
  return(result)
}
