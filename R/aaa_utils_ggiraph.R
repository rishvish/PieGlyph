#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# This file contains code of helper functions copied as is from ggiraph v0.8.13
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Additional helper functions taken from ggiraph which weren't exported
layer_interactive <- getFromNamespace("layer_interactive",
                                      "ggiraph")

# do_add_interactive_attrs <- getFromNamespace("do_add_interactive_attrs",
#                                              "ggiraph")

# add_interactive_attrs <- getFromNamespace("add_interactive_attrs",
#                                           "ggiraph")

dsvg_tracer_on <- getFromNamespace("dsvg_tracer_on",
                                   "ggiraph")
dsvg_tracer_off <- getFromNamespace("dsvg_tracer_off",
                                    "ggiraph")

interactive_attr_toxml <- getFromNamespace("interactive_attr_toxml",
                                           "ggiraph")


#' @usage NULL
NULL
IPAR_DEFAULTS <- list(
  data_id = NULL,
  tooltip = NULL,
  onclick = NULL,
  hover_css = NULL,
  selected_css = NULL,
  tooltip_fill = NULL,
  hover_nearest = NULL
)

#' @usage NULL
NULL
IPAR_NAMES <- names(IPAR_DEFAULTS)

#' @usage NULL
NULL
append_aes <- function (mapping, lst) {
  aes_new <- structure(lst, class = "uneval")
  mapping[names(aes_new)] <- aes_new
  mapping
}

#' @usage NULL
NULL
get_interactive_attr_names <- function(x, ipar = IPAR_NAMES) {
  intersect(names(x), ipar)
}

#' @usage NULL
NULL
add_default_interactive_aes <- function(geom = Geom,
                                         defaults = IPAR_DEFAULTS) {
  append_aes(geom$default_aes, defaults)
}

#' @usage NULL
NULL
ggproto_formals <- function (x) {
  formals(environment(x)$f)
}

#' @usage NULL
NULL
interactive_geom_parameters <- function(self, extra = FALSE) {
  parent_params <- self$super()$parameters(extra = extra)
  panel_args <- names(ggproto_formals(self$draw_panel))
  group_args <- names(ggproto_formals(self$draw_group))
  if ((".ipar" %in% panel_args || ".ipar" %in% group_args) &&
      !(".ipar" %in% parent_params)) {
    c(parent_params, ".ipar")
  }
  else {
    parent_params
  }
}

#' @usage NULL
NULL
get_ipar <- function(x, default = IPAR_NAMES) {
  ipar <- (if (!is.atomic(x))
    x$.ipar) %||% attr(x, "ipar")
  if (length(ipar) > 0 && is.character(ipar)) {
    ipar
  }
  else {
    default
  }
}

#' @usage NULL
NULL
interactive_geom_draw_key <- function(self, data, params, size) {
  gr <- self$super()$draw_key(data, params, size)
  add_interactive_attrs(gr, data, data_attr = "key-id",
                        ipar = get_ipar(params))
}

#' @usage NULL
NULL
add_interactive_attrs <- function(gr, data, rows = NULL, cl = NULL, overwrite = TRUE,
                                  data_attr = "data-id", ipar = IPAR_NAMES) {
  anames <- Filter(x = get_interactive_attr_names(data, ipar = ipar),
                   function(a) {
                     !is.null(data[[a]])
                   })
  if (length(anames) == 0) {
    return(gr)
  }
  if (inherits(gr, "gTree") && length(gr$children) > 0) {
    data_len <- nrow(data)
    children_len <- length(gr$children)
    if (is.null(data_len) || data_len == 1) {
      for (i in seq_along(gr$children)) {
        gr$children[[i]] <- do_add_interactive_attrs(gr = gr$children[[i]],
                                                     data = data, rows = rows, cl = cl, overwrite = overwrite,
                                                     data_attr = data_attr, ipar = anames)
      }
    }
    else if (children_len == data_len) {
      for (i in seq_along(gr$children)) {
        gr$children[[i]] <- do_add_interactive_attrs(gr = gr$children[[i]],
                                                     data = data[i, , drop = FALSE], rows = rows,
                                                     cl = cl, overwrite = overwrite, data_attr = data_attr,
                                                     ipar = anames)
      }
    }
    else {
      cli::cli_abort("Can't add interactive attrs to gTree", call = NULL)
    }
    return(gr)
  }
  else {
    do_add_interactive_attrs(gr = gr, data = data, rows = rows,
                             cl = cl, overwrite = overwrite, data_attr = data_attr,
                             ipar = anames)
  }
}

#' @usage NULL
NULL
do_add_interactive_attrs <- function (gr, data, rows = NULL, cl = NULL, overwrite = TRUE,
                                      data_attr = "data-id", ipar = IPAR_NAMES) {
  if (!grid::is.grob(gr) || is.zero(gr)) {
    return(gr)
  }
  isInteractive <- length(grep("interactive_", class(gr))) >
    0
  ip <- get_interactive_data(gr)
  if (length(rows) == 0) {
    for (a in ipar) {
      if (!isInteractive || isTRUE(overwrite) || is.null(ip[[a]])) {
        ip[[a]] <- data[[a]]
      }
    }
  }
  else {
    for (a in ipar) {
      if (!isInteractive || isTRUE(overwrite) || is.null(ip[[a]])) {
        ip[[a]] <- data[[a]][rows]
      }
    }
  }
  gr$.ipar <- ipar
  gr$.interactive <- ip
  gr$.data_attr <- data_attr
  if (is.null(cl) && !isInteractive) {
    cl <- paste("interactive", class(gr)[1], "grob", sep = "_")
    cl <- sub("grob_grob", "_grob", cl, ignore.case = TRUE)
  }
  class(gr) <- c(cl, class(gr))
  gr
}

#' @usage NULL
NULL
get_interactive_data <- function(x, default = list()) {
  (if (!is.atomic(x))
    x$.interactive) %||% attr(x, "interactive") %||% default
}

