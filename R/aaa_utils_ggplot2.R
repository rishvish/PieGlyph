#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# This file contains code of helper functions copied as is from ggplot2 v3.5.1
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# MIT License
# Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the “Software”), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# Additional helper functions taken from ggplot2 and grid packages because they
# weren't exported in the namescape by the respective packages
#' @usage NULL
NULL
is.waive <- function (x) {
  inherits(x, "waiver")
}

#' @usage NULL
NULL
is.zero <- function(x) {
  is.null(x) || inherits(x, "zeroGrob")
}


#' @usage NULL
NULL
defaults <- function (x, y) {
  c(x, y[setdiff(names(y), names(x))])
}

#' @usage NULL
NULL
manual_scale <- function(aesthetic, values = NULL, breaks = waiver(),
                         name = waiver(), ...,
                         limits = NULL, call = rlang::caller_call()) {
  call <- call %||% current_call()
  # check for missing `values` parameter, in lieu of providing
  # a default to all the different scale_*_manual() functions
  # if (rlang::is_missing(values)) {
  #   values <- NULL
  # } else {
  #   force(values)
  # }
  force(values)

  if (is.null(limits) && !is.null(names(values))) {
    # Limits as function to access `values` names later on (#4619)
    force(aesthetic)
    limits <- function(x) {
      x <- intersect(x, c(names(values), NA)) %||% character()
      if (length(x) < 1) {
        cli::cli_warn(paste0(
          "No shared levels found between {.code names(values)} of the manual ",
          "scale and the data's {.field {aesthetic}} values."
        ))
      }
      x
    }
  }

  # order values according to breaks
  if (is.vector(values) && is.null(names(values)) && !is.waive(breaks) &&
      !is.null(breaks) && !is.function(breaks)) {
    if (length(breaks) <= length(values)) {
      names(values) <- breaks
    } else {
      names(values) <- breaks[1:length(values)]
    }
  }

  pal <- function(n) {
    if (n > length(values)) {
      cli::cli_abort("Insufficient values in manual scale. {n} needed but only {length(values)} provided.")
    }
    values
  }
  ggplot2::discrete_scale(
    aesthetic, name = name,
    palette = pal, breaks = breaks, limits = limits,
    call = call, ...
  )
}
