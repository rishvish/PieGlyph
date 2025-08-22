
# This file contains helper functions copied as in from grid package
# because they weren't exported in the namescape.
#' @usage NULL
NULL
is.newUnit <- function(x) {
  inherits(x, 'unit_v2')
}

#' @usage NULL
NULL
upgradeUnit <- function(x) {
  if (is.newUnit(x)) return(x)
  UseMethod("upgradeUnit")
}

#' @title Upgrading units for a list
#' @param x A `unit.list` object.
#' @return A combined unit object.
#' @exportS3Method upgradeUnit unit.list
upgradeUnit.unit.list <- function(x) {
  do.call(unit.c, lapply(unclass(x), upgradeUnit))
}
