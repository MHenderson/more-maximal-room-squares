library(targets)

tar_option_set(
  packages = c("dplyr", "ggplot2", "purrr", "tibble", "wallis")
)

list(
  tar_target(
    name = example_square,
    command = {
      n <- 10
      r <- Room$new(size = n)
      for(e in r$empty_cells) {
        for(p in r$free_pairs) {
          if(r$is_available(e, p)) {
            r$set(e, p)
            break()
          }
        }
      }
      r$cells <- r$cells |>
        mutate(
          n_avail =  map_dbl(r$cells$avail, length)
        )
      return(r)
    }
  ),
  tar_target(
    name = example_square_plot,
    command = {
      ggplot(data = example_square$cells, aes(col, row)) +
        geom_text(data = example_square$cells |> filter(!is.na(first)), aes(label = paste(first, second, sep = ","))) +
        geom_segment(data = wallis:::grid_lines(9, 9), aes(x = x, y = y, xend = xend, yend = yend), linewidth = .1) +
        scale_y_reverse() +
        coord_fixed() +
        theme_void() +
        theme(
          legend.position  = "none"
        )
    }
  ),
  tar_target(
    name = save_example_square_plot,
    command = ggsave(plot = example_square_plot, filename = "plots/example-square-plot.png", bg = "white", width = 1000, height = 1000, units = "px"),
    format = "file"
  ),
  tar_target(
    name = colour_example_plot,
    command = {
      ggplot(data = example_square$cells, aes(col, row)) +
        geom_tile(data = example_square$cells |> filter(is.na(first)), aes(fill = as.factor(n_avail))) +
        geom_text(data = example_square$cells |> filter(!is.na(first)), aes(label = paste(first, second, sep = ","))) +
        geom_text(data = example_square$cells |> filter(is.na(first)) |> filter(n_avail > 0), aes(label = avail)) +
        geom_segment(data = wallis:::grid_lines(9, 9), aes(x = x, y = y, xend = xend, yend = yend), linewidth = .1) +
        scale_y_reverse() +
        coord_fixed() +
        theme_void() +
        theme(
          legend.position  = "none"
        )
    }
  ),
  tar_target(
    name = save_colour_example_plot,
    command = ggsave(plot = colour_example_plot, filename = "plots/colour-example-plot.png", bg = "white", width = 3000, height = 3000, units = "px"),
    format = "file"
  )
)
