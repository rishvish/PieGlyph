
<!-- README.md is generated from README.Rmd. Please edit that file -->

# PieGlyph

<!-- badges: start -->
<!-- badges: end -->

The goal of PieGlyph is to create pie-scatter plots with pies invariant
to plot dimensions to avoid any distortions of the pies.

## Installation

You can install the development version of PieGlyph from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("rishvish/PieGlyph")
```

## Examples

### Ecology example

``` r
#install.packages(DImodels)
library(PieGlyph)
library(DImodels)
library(tidyr)
library(ggplot2)

## Load the data
data(sim1)

## Convert data into long-format
plot_data <- pivot_longer(data = sim1, cols = paste0('p',1:4),
                          names_to = 'Species', values_to = 'Prop')

## Create plot
ggplot(data = plot_data)+
    geom_pie_glyph(aes(x = community, y = response), categories = 'Species',
                   values = 'Prop', size = 0.7, colour = NA)+
    facet_wrap(~block)+
    labs(y = 'Response', x = 'Community')+
    theme_classic()
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />

### Spatial data example

``` r
library(dplyr)
library(ggplot2)

## Get US map data
states <- map_data("state")

## Data showing breakdown of the arrests in all US states
arrests <- USArrests
arrests <- arrests %>% mutate(region = tolower(rownames(USArrests)),
                              pie_lat = state.center$y,
                              pie_long = state.center$x)

## Merge map data with arrests data to get coordinates to place pies
choro <- merge(states, arrests, sort = FALSE, by = "region")
pie_data <- choro %>% group_by(region) %>% slice(1) %>%
                      select(region, pie_lat, pie_long, Murder, Assault, Rape)

## Create plot (Can also create without converting data in long format)
ggplot(states, aes(x = long, y = lat)) +
   geom_polygon(aes(group = group), fill = 'darkseagreen', colour = 'black')+
   geom_pie_glyph(aes(y = pie_lat, x = pie_long),
                  data = pie_data, categories = 4:6,
                  size = 0.75, colour = 'black', alpha = 0.7)+
   coord_map("albers",  lat0 = 45.5, lat1 = 29.5)+
   labs(x = 'Longitude', y ='Latitude')+
   theme(panel.background = element_rect(fill = 'lightsteelblue2'))+
   scale_fill_brewer(palette = 'Dark2')
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" />

### Compositions example

``` r
#install.packages('compositions')

## Load data
data("SerumProtein", package = 'compositions')

## Fit Logistic regression model
disease_data <- as_tibble(SerumProtein) %>% mutate(Type = factor(ifelse(Type == 1, 'Yes', 'No')))
m1 <- glm(Type ~ a + b + c + d, data = disease_data, family=binomial(link='logit'))

## Create plot
disease_data %>%
   mutate('prediction' = predict(m1, type = 'response')) %>%
   arrange(desc(prediction)) %>%
   mutate('n' = 1:nrow(.)) %>%
   pivot_longer(cols = c('a','b','c','d'), names_to = 'Marker', values_to = 'Proportion') %>%
   ggplot(data = ., aes(x = n, y = prediction, fill = Marker))+
     geom_segment(aes(yend = 0, xend = n))+
     geom_pie_glyph(categories = 'Marker', values = 'Proportion', size = 0.5)+
     labs(y = 'Prob(Having Disease)', x = 'Case')+
     theme_minimal()
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="100%" />
