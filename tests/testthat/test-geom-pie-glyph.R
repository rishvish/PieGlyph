test_that("geom_pie_glyph throws appropriate errors", {
  # NAs in some slices
  plot_data1 <- data.frame(x = 1:4,
                           y = c(3,  1,  4,  2),
                           A = c(5,  2, NA,  3),
                           B = c(NA, 2,  3, NA),
                           C = c(7, NA, NA,  3))
  p1 <- ggplot()+
    geom_pie_glyph(aes(x = x, y = y),
                   slices = c('A','B','C'),
                   data = plot_data1,
                   radius = 1)
  expect_warning(print(p1),
                 'There were observations with some slices being')

  # NAs in all slices
  plot_data2 <- data.frame(x = 1:4,
                           y = c(3,  1, 4,  2),
                           A = c(5,  2, NA, 3),
                           B = c(0,  2, NA, 3),
                           C = c(7,  5, NA, 3))
  p2 <- ggplot()+
    geom_pie_glyph(aes(x = x, y = y),
                   slices = c('A','B','C'),
                   data = plot_data2,
                   radius = 1)
  expect_warning(print(p2),
                 'There were observations with all slices being')

  # 0 in all slices
  plot_data3 <- data.frame(x = 1:4,
                           y = c(3, 1, 4, 2),
                           A = c(5, 2, 0, 3),
                           B = c(0, 2, 0, 3),
                           C = c(7, 5, 0, 3))
  p3 <- ggplot()+
    geom_pie_glyph(aes(x = x, y = y),
                   slices = c('A','B','C'),
                   data = plot_data3,
                   radius = 1)
  expect_warning(print(p3),
                 'There were observations with all slices being')

  # pie-group warning
  plot_data4 <- data.frame(system = rep(paste0('S', 1:3), each = 3),
                           x = c(1,1,1,1,1,1,2,2,2),
                           y = c(2,2,2,2,2,2,4,4,4),
                           attribute = rep(c('A','B','C'), times = 3),
                           value = c(30,20,10, 20,50,50, 10,10,10))
  p4 <- ggplot(data = plot_data4 %>% dplyr::group_by(system))+
    geom_pie_glyph(aes(x = x, y = y),
                   slices = 'attribute', values = 'value',
                   radius = 1)
  expect_warning(print(p4),
                 "Some pie-glyphs have identical x and y coordinates")

  # Ensure inputs are appropriate
  plot_data5 <- data.frame(x = 1:4,
                           y = c(3, 1, 4,  2),
                           A = c(5, 2, 1, 3),
                           B = c(0, 2, 2, 3),
                           C = c(7, 5, 3, 3))
  # Slices should be specified
  expect_error(ggplot()+
                 geom_pie_glyph(aes(x = x, y = y),
                                data = plot_data5),
               "Specify column/columns")

  # Values not needed if data is in wide-format
  expect_warning(ggplot()+
                   geom_pie_glyph(aes(x = x, y = y),
                                  slices = c("A", "B", "C"),
                                  values = "x",
                                  data = plot_data5),
                 "parameter is not needed if data is in wide-format")

  # Values needed if data is in long-format
  expect_error(ggplot()+
                   geom_pie_glyph(aes(x = x, y = y),
                                  slices = c("A"),
                                  data = plot_data5),
                 "If data is in long-format")

  # Data should be a data.frame
  expect_error(print(ggplot()+
                   geom_pie_glyph(aes(x = x, y = y),
                                  slices = c("A", "B"),
                                  data = pieTree)),
               "Problem while computing layer data.")

  # All columns in slices should be numeric
  plot_data6 <- data.frame(x = 1:4,
                           y = c(3, 1, 4,  2),
                           A = c(5, 2, 1, 3),
                           B = c(0, 2, 2, 3),
                           C = c(7, 5, 3, 3),
                           D = c("A", "B", "C", "D"))

  p6 <- ggplot()+
    geom_pie_glyph(aes(x = x, y = y),
                   slices = c("A", "B", "C", "D"),
                   data = plot_data6)
  expect_error(print(p6),
               "All columns specified in `slices` should be numeric.")

  # Internal keywords aren't present in data
  p7 <- ggplot() +
    geom_pie_glyph(aes(x = x, y = y),
                   slices = 3:5,
                   data = plot_data6 %>% mutate(".Slices" = 0))
  expect_error(print(p7),
               "are special strings reserved for internal computations")

  # If in long format then values should be numeric
  p8 <- ggplot() +
    geom_pie_glyph(aes(x = x, y = y),
                   slices = "attribute", value = "value",
                   data = plot_data4 %>% mutate(value = as.character(value)))
  expect_error(print(p8),
               "The column specified in `values` should be numeric.")

  # Data shouldn't have negative values
  p9 <- ggplot() +
    geom_pie_glyph(aes(x = x, y = y),
                   slices = "attribute", value = "value",
                   data = plot_data4 %>% mutate(value = -value))
  expect_error(print(p9),
               "negative values. Remove them before plotting.")

  # values are specified in scale_radius_manual
  expect_error(ggplot() +
                 geom_pie_glyph(aes(x = x, y = y, radius = system),
                                slices = "attribute", value = "value",
                                data = plot_data4[4:9, ]) +
                 scale_radius_manual(),
               "Specify the values of the radii for each group as a numeric")

  # data is mandatory either in ggplot call or geom_pie_glyph layer
  expect_error(print(ggplot() +
                 geom_pie_glyph(aes(x = x, y = y, radius = system),
                                slices = "attribute", value = "value",
                                data = NULL)),
               "not found")
})

# Snapshot of plots
test_that("geom_pie_glyph snapshots", {
  set.seed(123)
  plot_data <- data.frame(response = rnorm(10, 100, 30),
                          system = as.factor(1:10),
                          group = sample(size = 10,
                                         x = c("G1", "G2", "G3"),
                                         replace = TRUE),
                          A = round(runif(10, 3, 9), 2),
                          B = round(runif(10, 1, 5), 2),
                          C = round(runif(10, 3, 7), 2),
                          D = round(runif(10, 1, 9), 2))


  vdiffr::expect_doppelganger(title = "basic plot",
                              fig = ggplot(data = plot_data,
                                           aes(x = system, y = response))+
                                      geom_pie_glyph(slices = c("A", "B", "C", "D"),
                                                     data = plot_data))

  vdiffr::expect_doppelganger(title = "custom radius",
                              fig = ggplot(mapping = aes(x = system, y = response))+
                                       geom_pie_glyph(slices = 4:7, data = plot_data,
                                                      colour = "black", radius = 0.5))

  p <- ggplot(data = plot_data,
              aes(x = system, y = response))+
           geom_pie_glyph(aes(radius = group),
                          slices = c("A", "B", "C", "D"),
                          data = plot_data, colour = "black")

  vdiffr::expect_doppelganger(title = "map radius to variable",
                              fig = p)

  vdiffr::expect_doppelganger(title = "change slice colour",
                              fig = p +
                                ggplot2::scale_fill_manual(values = c("#56B4E9", "#CC79A7",
                                                                      "#F0E442", "#D55E00")))

  vdiffr::expect_doppelganger(title = "map border colour to variable",
                              fig = ggplot(mapping = aes(x = system, y = response))+
                                geom_pie_glyph(aes(colour = group),
                                               slices = 4:7, data = plot_data,
                                               radius = 0.5))

  vdiffr::expect_doppelganger(title = "map border linetype to variable",
                              fig = ggplot(mapping = aes(x = system, y = response))+
                                geom_pie_glyph(aes(linetype = group),
                                               slices = 4:7, data = plot_data,
                                               colour = "black", radius = 0.5))

  vdiffr::expect_doppelganger(title = "position dodge works",
                              fig = ggplot(mapping = aes(x = group, y = response))+
                                geom_pie_glyph(position = ggplot2::position_dodge(0.5),
                                               slices = 4:7, data = plot_data,
                                               colour = "black", radius = 0.5))

  plot_data_stacked <- plot_data %>%
                            pivot_longer(cols = c("A", "B", "C", "D"),
                                         names_to = "Attributes",
                                         values_to = "values")

  vdiffr::expect_doppelganger(title = "long-form data works",
                              fig = ggplot(data = plot_data_stacked,
                                           aes(x = system, y = response))+
                                      geom_pie_glyph(slices = "Attributes",
                                                     values = "values"))
})

# Snapshot for scale_radius
test_that("scale_radius works", {
  set.seed(737)
  plot_data <- data.frame(response = rnorm(10, 100, 30),
                          system = as.factor(1:10),
                          group = sample(size = 10,
                                         x = c(1:3),
                                         replace = TRUE),
                          A = round(runif(10, 3, 9), 2),
                          B = round(runif(10, 1, 5), 2),
                          C = round(runif(10, 3, 7), 2),
                          D = round(runif(10, 1, 9), 2))

  vdiffr::expect_doppelganger(title = "scale_radius_continuous",
                              fig = ggplot(data = plot_data,
                                           aes(x = system, y = response))+
                                geom_pie_glyph(aes(radius = group),
                                               slices = c("A", "B", "C", "D"),
                                               data = plot_data, colour = "black") +
                                scale_radius_continuous(range = c(0.2, 0.5)))

  q <- ggplot(data = plot_data)+
          geom_pie_glyph(aes(x = system, y = response,
                             radius = as.factor(group)),
                         colour = "black",
                         slices = c('A', 'B', 'C', 'D'))

  vdiffr::expect_doppelganger(title = "scale_radius_discrete",
                            fig = q + scale_radius_discrete(range = c(0.05, 0.2),
                                                            unit = 'in',
                                                            name = 'Group'))

  vdiffr::expect_doppelganger(title = "scale_radius_manual",
                            fig = q + scale_radius_manual(values = c(2, 6, 4),
                                                          unit = 'mm',
                                                          labels = paste0('G', 1:3),
                                                          name = 'G'))
})

# draw_key_pie
test_that("draw_key_pie works as expected", {
  meta_data1 <- data.frame(fill = "#000000",
                           colour = "#ffffff",
                           radius = 0.25,
                           linetype = 1,
                           alpha = 1,
                           slice = NA,
                           values = NA,
                           pie_group = NA,
                           .draw = TRUE)

  key1 <- draw_key_pie(data = meta_data1,
                       params = list(na.rm = TRUE),
                       size = 6)

  meta_data2 <- meta_data1[,c(3, 1, 2, 4:9)]
  key2 <- draw_key_pie(data = meta_data2,
                       params = list(na.rm = TRUE),
                       size = 6)

  # If radius is fixed then return a rectangle legend
  expect_equal(class(key1)[1], "rect")
  # If radius changes across groups then returns a points legend
  expect_equal(class(key2)[1], "points")

})


# tests for scale functions
test_that("test that scale_radius_* works as expected", {
  # Manual scale
  expect_equal(class(scale_radius_manual(values = 1:3)),
               class(scale_radius_discrete()))

  # Discrete scale
  sc <- scale_radius_discrete()
  expect_equal(sc$palette(2),
               c(0.25, 0.6))
})




























