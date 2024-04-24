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
