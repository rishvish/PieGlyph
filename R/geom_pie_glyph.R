#' Key for Pie Glyphs
#'
#' @inheritParams ggplot2::draw_key
#'
#' @return A grid grob
#' @seealso \code{\link[ggplot2:draw_key]{draw_key}}
#' @export
draw_key_pie <- function (data, params, size) {

  `%||%` <- function (a, b)
  {
    if (is.null(a) || is.na(a)) b else a
  }

  if (is.null(data$size)) {
    data$size <- 0.5
  }
  lwd <- min(data$size, min(size)/4)
  radius <- data$radius*2
  if(names(data)[1] == 'radius'){
    data$shape <- 19
    pointsGrob(0.5, 0.5,
               pch = data$shape,
               gp = gpar(col = alpha(data$colour %||% "black", data$alpha),
                         fill = alpha(data$fill %||% "black", data$alpha),
                         fontsize = (radius %||% 1.5) * .pt + (data$stroke %||% 0.5) * .stroke/2,
                         lwd = (data$stroke %||% 0.5) * .stroke/2),
               vp = viewport(clip = "on"))

  } else {
    rectGrob(width = unit(radius, "npc") - unit(lwd, "mm"),
             height = unit(radius, "npc") - unit(lwd, "mm"),
             gp = gpar(col = data$colour %||%  NA,
                       fill = alpha(data$fill %||% "grey20", data$alpha),
                       lty = data$linetype %||% 1,
                       lwd = (data$size/3 %||% 0.5) * .pt,
                       linejoin = params$linejoin %||% "mitre",
                       lineend = if (identical(params$linejoin, "round")) "round" else "square"),
             vp = viewport(clip = "on"))
  }
}


#' @usage NULL
#' @importFrom grid gpar viewport grobTree unit rectGrob pointsGrob
#' @importFrom tidyr pivot_longer pivot_wider %>%
#' @importFrom dplyr mutate near distinct select
#' @importFrom stats as.formula
#' @importFrom rlang sym syms !! !!!
#' @importFrom ggforce geom_arc_bar
#' @importFrom ggplot2 ggproto Geom draw_key_polygon aes_ aes ggplotGrob ggplot theme_void
#' @export
NULL
GeomPieGlyph <- ggproto('GeomPieGlyph', Geom,
                        required_aes = c('x', 'y'),
                        default_aes = list(
                          colour = NA, radius = 0.5, size = 1, linetype = 1, alpha = 1, categories = NA, values = NA, fill = NA
                        ),
                        draw_key = draw_key_pie,
                        setup_data = function(data, params){
                          if(all(data$group == 1)){
                            nCat <- length(unique(data[, 'categories']))
                            data$group <- rep(1:(nrow(data)/nCat), each = nCat)
                          }
                          data
                        },
                        draw_panel = function(data, panel_scales, coord) {
                          ## Transform the data first
                          coords <- coord$transform(data, panel_scales)
                          #print(head(coords, 12))
                          categories <- unique(data[, 'categories'])
                          nCat <- length(categories)

                          coords <- coords %>%
                            mutate(pie_group = rep(1:(nrow(coords)/nCat), each = nCat)) %>%
                            group_by(pie_group) %>%
                            mutate(ID = factor(paste(values, collapse = '_'))) %>%
                            mutate(ID = (as.numeric(ID))) %>% ungroup()

                          #print(as.data.frame(coords))

                          my_coords <<- coords

                          # Construct pies for the unique communities in the data
                          pies <- get_pies(data = coords)
                          #my_pies <<- pies

                          # List of all pies
                          grobs <- coords %>%
                            group_by(pie_group) %>%
                            group_map(~pie_aes(., pies))

                          # group all the pie grobs into a single grobTree object and plot
                          obj <- do.call(grid::grobTree, grobs)
                        })

#' geom_pie_glyph
#'
#' @param mapping Set of aes mappings created by \code{\link[ggplot2:aes]{aes()}} or \code{\link[ggplot2:aes_]{aes_()}}
#' @param data The data to be displayed in this layer. The default, \code{NULL}, inherits the plot data specified in the \code{\link[ggplot2:ggplot]{ggplot()}} call.
#' @param categories Columns in the data with the categories for the pies (can also be a single column if data is in long format). To be specified either as column indices or column names
#' @param values Column with values for the categories of the pies (if data is in long format). The default is \code{NA} (if data not in long format).
#' @param position Position adjustment to avoid overlapping of pies.
#' @param na.rm If FALSE, the default, missing values are removed with a warning. If TRUE, missing values are silently removed.
#' @param show.legend logical. Should this layer be included in the legends? NA, the default, includes if any aesthetics are mapped. FALSE never includes, and TRUE always includes.
#' @param inherit.aes If FALSE, overrides the default aesthetics, rather than combining with them
#' @param stat The statistical transformation to use on the data for this layer, as a string
#' @param ... Other arguments passed on to layer(). These are often aesthetics, used to set an aesthetic to a fixed value, like colour = "red" or radius = 1. They may also be parameters to the paired geom/stat.
#'
#' @section Aesthetics:
#' geom_pie_glyph understands the following aesthetics (required aesthetics are in bold):
#'
#' - **x** - variable to be shown on X-axis
#' - **y** - variable to be shown on Y-axis
#' - alpha - adjust opacity of the pies
#' - radius - adjust the radius of the pies (in cm)
#' - colour - colour of border of pies
#' - linetype - style of pie borders
#' - size - width of pie borders (in mm)
#'
#' @return layer
#' @export
#'
#' @examples
#'
#' ########### Examples from ecology
#'
#' #install.packages(DImodels)
#' library(PieGlyph)
#' library(DImodels)
#' library(tidyr)
#' library(ggplot2)
#' library(dplyr)
#'
#' ## Load the data
#' data(sim1)
#'
#' ## Convert data into long-format
#' plot_data <- sim1 %>%
#'              mutate(Richness = rowSums(.[, paste0('p',1:4)] != 0),
#'                     Evenness = DI_data_E_AV(prop = 3:6, data = .)$E) %>%
#'              pivot_longer(cols = paste0('p',1:4),
#'                           names_to = 'Species', values_to = 'Prop')
#'
#' ## Create plot
#' ggplot(data = plot_data)+
#'     geom_pie_glyph(aes(x = Richness, y = response, group = Evenness),
#'     categories = 'Species', values = 'Prop', colour = NA,
#'     position = position_dodge(1))+
#'     facet_wrap(~block)+
#'     labs(y = 'Response', x = 'Richness')+
#'     theme_classic()
#'
#'
#' ############# Spatial example
#'
#' library(dplyr)
#' library(ggplot2)
#'
#' ## Get US map data
#' states <- map_data("state")
#'
#' ## Data showing breakdown of the arrests in all US states
#' arrests <- USArrests
#' arrests <- arrests %>% mutate(region = tolower(rownames(USArrests)),
#'                               pie_lat = state.center$y,
#'                               pie_long = state.center$x)
#'
#' ## Merge map data with arrests data to get coordinates to place pies
#' choro <- merge(states, arrests, sort = FALSE, by = "region")
#' pie_data <- choro %>% group_by(region) %>% slice(1) %>%
#'                       select(region, pie_lat, pie_long,
#'                              Murder, Assault, Rape)
#'
#' ## Create plot (Can also create without converting data in long format)
#' ggplot(states, aes(x = long, y = lat)) +
#'    geom_polygon(aes(group = group),
#'                 fill = 'darkseagreen', colour = 'black')+
#'    geom_pie_glyph(aes(y = pie_lat, x = pie_long),
#'                   data = pie_data, categories = 4:6,
#'                   radius = 1, colour = 'black', alpha = 0.7)+
#'    coord_map("albers",  lat0 = 45.5, lat1 = 29.5)+
#'    labs(x = 'Longitude', y ='Latitude')+
#'    theme(panel.background = element_rect(fill = 'lightsteelblue2'))+
#'    scale_fill_brewer(palette = 'Dark2')
#'
#'
#' ############# Compositions example
#'
#' #install.packages('compositions')
#'
#' ## Load data
#' data("SerumProtein", package = 'compositions')
#'
#' ## Fit Logistic regression model
#' disease_data <- as_tibble(SerumProtein) %>%
#'                     mutate(Type = factor(ifelse(Type == 1, 'Yes', 'No')))
#' m1 <- glm(Type ~ a + b + c + d, data = disease_data,
#'           family=binomial(link='logit'))
#'
#' ## Create plot
#' disease_data %>%
#'    mutate('prediction' = predict(m1, type = 'response')) %>%
#'    arrange(desc(prediction)) %>%
#'    mutate('n' = 1:nrow(.)) %>%
#'    pivot_longer(cols = c('a','b','c','d'), names_to = 'Marker',
#'                 values_to = 'Proportion') %>%
#'    ggplot(data = ., aes(x = n, y = prediction, fill = Marker))+
#'      geom_segment(aes(yend = 0, xend = n))+
#'      geom_pie_glyph(categories = 'Marker', values = 'Proportion',
#'                     radius = 0.75)+
#'      labs(y = 'Prob(Having Disease)', x = 'Case')+
#'      theme_minimal()
geom_pie_glyph <- function(mapping = NULL, data = NULL, categories, values = NA,
                           stat = "identity", position = "identity", na.rm = FALSE,
                           show.legend = NA, inherit.aes = TRUE, ...) {
  if (is.null(mapping))
    mapping <- ggplot2::aes_()

  # For situations when the data is wide format instead of long
  if (length(categories)> 1){
    if (is.null(data)){
      stop('Need to specify data in layer if data is not in long format')
    }
    if (is.numeric(categories)){
      categories <- colnames(data)[categories]
    }

    data <- data %>% tidyr::pivot_longer(cols = categories, names_to = 'Categories', values_to = 'Values')
    values <- 'Values'
    categories <- 'Categories'
  }

  if (length(categories) == 1){
    if(is.na(values)){
      stop('Specify column with category values if data is in long format.')
    }
  }

  if(is.null(mapping[['group']])){
    mapping <- utils::modifyList(mapping,
                                 ggplot2::aes_(group = 1))
  }

  mapping <- utils::modifyList(mapping,
                               ggplot2::aes_(fill = as.formula(paste0("~", categories)),
                                             values = as.formula(paste0("~", values)),
                                             categories = as.formula(paste0("~", categories)))
  )

  ggplot2::layer(
    data = data, mapping = mapping, stat = 'identity', geom = GeomPieGlyph,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}




# Function to get pie glyphs for unique communities in data
get_pies <- function(data){
  # Get unique communities in the data
  comms <- data %>%
    dplyr::distinct(categories, values, ID)

  # For mapping the pies to each community in the data
  IDs <- as.character(unique(comms$ID))
  pie_grobs <- comms %>% group_by(ID) %>% group_map(~get_pie(.))
  names(pie_grobs) <- IDs
  return(pie_grobs)
}

# Function to create the individual pies using geom_arc_bar
get_pie <- function(data){
  # Code from geom_arc_bar in ggforce
  # This will accruately create the wedges of the pies
  angles <- cumsum(data$values)
  sep <- 0.000001
  seps <- cumsum(sep * seq_along(angles))
  angles <- angles / max(angles) * (2 * pi - max(seps))
  start = c(0, angles[-length(angles)]) + c(0, seps[-length(seps)]) + sep / 2
  end = angles + seps - sep / 2
  end[start == end] = end[start == end] + sep
  data <- data %>% mutate(start = start, end = end)
  ggplotGrob(
    ggplot(data = data) +
      geom_arc_bar(aes(x0 =1, y0=1, r0 =0, r =1, start = start, end = end, fill = categories), colour = NA, show.legend = F)+
      theme_void()
  ) %>% return()
}


# Function to adjust the aesthetics while plotting the individual pies
#' @usage NULL
pie_aes <- function(point, pies) {
  pie.grob <- grid::grobTree(
    pies[[unique(point$ID)]],
    vp = viewport(), gp = gpar())

  # Radius of pies
  radius <-  point[['radius']]

  # Fill and border colour
  pie.grob$children$layout$grobs[[5]]$children[[3]]$gp$col <- point[['colour']]
  pie.grob$children$layout$grobs[[5]]$children[[3]]$gp$fill <- unlist(point[['fill']])


  # Aesthetics for pies
  pie.grob$children$layout$grobs[[5]]$children[[3]]$gp$alpha  <- point[['alpha']]
  pie.grob$children$layout$grobs[[5]]$children[[3]]$gp$lty <- point[['linetype']]
  pie.grob$children$layout$grobs[[5]]$children[[3]]$gp$lwd <- point[['size']]

  # Position and radius of the pies
  pie.grob$vp$x      <- unit(point[['x']], 'npc')
  pie.grob$vp$y      <- unit(point[['y']], 'npc')
  pie.grob$vp$width  <- unit(radius, 'cm')
  pie.grob$vp$height <- unit(radius, 'cm')

  pie.grob
}

### Scales for the additional aesthetics
#' @rdname scale_radius_continuous
#' @inheritParams ggplot2::scale_size_discrete
#' @export
scale_radius_discrete <-  function (..., range = c(.5, 1.5), unit = 'cm') {
  range <- grid::convertWidth(unit(range, unit), "cm", valueOnly = TRUE)
  ggplot2::discrete_scale(
    aesthetics = "radius",
    scale_name = "radius_d",
    function(n) {
      area <- seq(range[1]^2, range[2]^2, length.out = n)
      sqrt(area)
    },
    ...
  )
}

#' @rdname scale_radius_continuous
#'
#' @inheritParams ggplot2::scale_size_manual
#' @export
scale_radius_manual <- function (..., values, unit = 'cm', breaks = waiver(), na.value = NA) {
  values <- grid::convertWidth(unit(values, unit), "cm", valueOnly = TRUE)
  ggplot2:::manual_scale("radius", values, breaks, ..., na.value = na.value)
}


#' Scales for the pie radius
#'
#' @description \code{scale_radius_*()} is useful for adjusting the radius of the pie glyphs.
#'
#' @inheritParams ggplot2::scale_size
#' @param unit Unit for the radius of the pies. Default is 'cm', but other units like 'in', 'mm', etc. can be used.
#'
#' @export
#' @examples
#' library(PieGlyph)
#' library(DImodels)
#' library(tidyr)
#' library(dplyr)
#' library(ggplot2)
#'
#' ## Load the data
#' data(sim1)
#' sim1$Evenness <- DI_data_E_AV(prop = 3:6, data = sim1)$E
#'
#' ## Convert data into long-format
#' plot_data <- sim1 %>% filter(block == 1) %>%
#'                       pivot_longer(cols = paste0('p',1:4),
#'                                    names_to = 'Species', values_to = 'Prop')
#'
#' ## Create plot
#' p <- ggplot(data = plot_data)+
#'     geom_pie_glyph(aes(x = community, y = response, radius = Evenness),
#'     categories = 'Species', values = 'Prop', colour = NA)+
#'     labs(y = 'Response', x = 'Community')+
#'     theme_classic()
#'
#' p + scale_radius_continuous(range = c(1, 2))
#'
#' q <- ggplot(data = plot_data)+
#'     geom_pie_glyph(aes(x = community, y = response,
#'                        radius = as.factor(Evenness)),
#'                  categories = 'Species', values = 'Prop', colour = 'black')+
#'     labs(y = 'Response', x = 'Community')+
#'     theme_classic()
#'
#' q + scale_radius_discrete(range = c(0.1, 0.2), unit = 'in',
#'                           name = 'Evenness')
#'
#' q + scale_radius_manual(values = c(5, 20, 10, 15), unit = 'mm',
#'                         labels = LETTERS[1:4], name = 'E')
scale_radius_continuous <- function(..., range = c(.5, 1.5), unit = "cm") {
  range <- grid::convertWidth(unit(range, unit), "cm", valueOnly = TRUE)
  ggplot2::continuous_scale(
    aesthetics = "radius",
    scale_name = "radius_c",
    palette = scales::rescale_pal(range),
    ...
  )
}

#' @rdname scale_radius_continuous
#' @export
scale_radius <- scale_radius_continuous
