# Snapshots for geom_pie_interactive
test_that("geom_pie_interactive works", {
  set.seed(739)
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
                              fig = girafe(ggobj = ggplot(data = plot_data,
                                                          aes(x = system, y = response))+
                                                      geom_pie_interactive(slices = c("A", "B", "C", "D"),
                                                                           colour = "black")))

  vdiffr::expect_doppelganger(title = "custom tooltip",
                              fig = girafe(ggobj = ggplot(data = plot_data,
                                                          aes(x = system, y = response))+
                                                      geom_pie_interactive(aes(tooltip = paste0("Group: ", group)),
                                                                           slices = c("A", "B", "C", "D"),
                                                                           colour = "black")))

  vdiffr::expect_doppelganger(title = "data_id",
                              fig = girafe(ggobj = ggplot(data = plot_data,
                                                          aes(x = system, y = response))+
                                                      geom_pie_interactive(aes(data_id = group),
                                                                           slices = c("A", "B", "C", "D"),
                                                                           colour = "black")))

  vdiffr::expect_doppelganger(title = "multiple interactive parameters",
                              fig = girafe(ggobj = ggplot(data = plot_data,
                                                          aes(x = system, y = response))+
                                                     geom_pie_interactive(aes(tooltip = paste0("Group: ", group),
                                                                              data_id = group),
                                                                          slices = c("A", "B", "C", "D"),
                                                                          colour = "black")))

  vdiffr::expect_doppelganger(title = "only one attribute",
                              fig = girafe(ggobj = ggplot(data = data.frame(system = 1,
                                                                            response = 1,
                                                                            A = 1, B = 0, C = 0, D = 0))+
                                                      geom_pie_interactive(aes(x = system, y = response),
                                                                           slices = c("A", "B", "C", "D"),
                                                                           colour = "black", inherit.aes = FALSE)))

  plot_data_stacked <- plot_data %>%
    pivot_longer(cols = c("A", "B", "C", "D"),
                 names_to = "Attributes",
                 values_to = "values")

  vdiffr::expect_doppelganger(title = "long-form data works",
                              fig = girafe(ggobj = ggplot(data = plot_data_stacked,
                                                          aes(x = system, y = response))+
                                                      geom_pie_interactive(colour = "black",
                                                                           slices = "Attributes",
                                                                           values = "values")))


})


test_that("ggiraph imports work", {

  set.seed(737)
  plot_data <- data.frame(response = rnorm(10, 100, 30),
                          system = as.factor(1:10),
                          group = sample(size = 10,
                                         x = c(1:3),
                                         replace = TRUE),
                          A = round(runif(10, 3, 9), 2),
                          B = round(runif(10, 1, 5), 2),
                          C = round(runif(10, 3, 7), 2),
                          D = round(runif(10, 1, 9), 2)) %>%
    mutate(x = 1:10,
           y = response,
           radius = 0.25,
           colour = "black",
           linewidth = 1,
           alpha = 1,
           linetype = 1)

  plot_data_stacked <- plot_data %>%
    mutate(pie_group = 1:nrow(plot_data)) %>%
    pivot_longer(cols = c("A", "B", "C", "D"),
                 names_to = "Attributes",
                 values_to = "values") %>%
    mutate(fill = rep(c("tomato", "blue3", "green4", "yellow3"),
                      times = 10))

  meta_data1 <- data.frame(fill = "#000000",
                           colour = "#ffffff",
                           radius = 0.25,
                           linetype = 1,
                           linewdith = 0.5,
                           alpha = 1,
                           slice = NA,
                           values = NA,
                           pie_group = NA,
                           data_id = "1",
                           .draw = TRUE)

  expect_equal(append_aes(aes(x = 1, y = 2),
                          list(new_aes = 3)),
               aes(x = 1, y = 2, new_aes = 3))

  expect_equal(add_default_interactive_aes(GeomPieGlyph),
               list(colour = NA, radius = 0.25, linewidth = 1,
                    linetype = 1, alpha = 1, slices = NA,
                    values = NA, fill = NA, pie_group = NA,
                    data_id = NULL, tooltip = NULL, onclick = NULL,
                    hover_css = NULL, selected_css = NULL,
                    tooltip_fill = NULL, hover_nearest = NULL))

  expect_equal(interactive_geom_parameters(GeomInteractivePieGlyph),
               c("panel_scales", ".ipar"))

  expect_equal(length(interactive_geom_parameters(GeomPieGlyph)), 0)

  expect_equal(get_ipar(""),
               c("data_id", "tooltip", "onclick",
                 "hover_css", "selected_css",
                 "tooltip_fill", "hover_nearest"))

  # No interactive attributes so return basic rect
  key1 <- interactive_geom_draw_key(GeomInteractivePieGlyph,
                                    data = meta_data1[, -10],
                                    params = list(na.rm = TRUE),
                                    size = 1)

  expect_equal(class(key1)[1], "rect")

  # Return interactive rect
  key2 <- interactive_geom_draw_key(GeomInteractivePieGlyph,
                                    data = meta_data1,
                                    params = list(na.rm = TRUE),
                                    size = 1)

  expect_equal(class(key2)[1], "interactive_rect_grob")

  test_grob <- gTree(children = pieTree(plot_data_stacked[1:12, ]))

  # Works with gTree
  expect_equal(class(add_interactive_attrs(test_grob,
                                           data = meta_data1))[1],
               "gTree")

  expect_equal(class(add_interactive_attrs(test_grob,
                                           data = rbind(meta_data1,
                                                        meta_data1,
                                                        meta_data1)))[1],
               "gTree")

  # Error if mismatch between data and grobs count
  expect_error(add_interactive_attrs(test_grob,
                                     data = rbind(meta_data1,
                                                  meta_data1)),
               "Can't add interactive attrs")

  # Do nothing if not grob
  expect_equal(do_add_interactive_attrs(plot_data),
               plot_data)

  # Do nothing if not grob
  expect_equal(
    do_add_interactive_attrs(
        pieGrob(values = 1:4),
        data = meta_data1,
        rows = 1
      ) %>% class() %>% .[1],
    "interactive_polygon_grob"
  )

})
