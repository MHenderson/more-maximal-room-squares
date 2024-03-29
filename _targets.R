# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
# library(tarchetypes) # Load other packages as needed.

# Set target options:
tar_option_set(
  packages = c("dplyr", "ggplot2", "purrr", "tibble") # packages that your targets need to run
  # format = "qs", # Optionally set the default storage format. qs is fast.
  #
  # For distributed computing in tar_make(), supply a {crew} controller
  # as discussed at https://books.ropensci.org/targets/crew.html.
  # Choose a controller that suits your needs. For example, the following
  # sets a controller with 2 workers which will run as local R processes:
  #
  #   controller = crew::crew_controller_local(workers = 2)
  #
  # Alternatively, if you want workers to run on a high-performance computing
  # cluster, select a controller from the {crew.cluster} package. The following
  # example is a controller for Sun Grid Engine (SGE).
  # 
  #   controller = crew.cluster::crew_controller_sge(
  #     workers = 50,
  #     # Many clusters install R as an environment module, and you can load it
  #     # with the script_lines argument. To select a specific verison of R,
  #     # you may need to include a version string, e.g. "module load R/4.3.0".
  #     # Check with your system administrator if you are unsure.
  #     script_lines = "module load R"
  #   )
  #
  # Set other options as needed.
)

# tar_make_clustermq() is an older (pre-{crew}) way to do distributed computing
# in {targets}, and its configuration for your machine is below.
options(clustermq.scheduler = "multicore")

# tar_make_future() is an older (pre-{crew}) way to do distributed computing
# in {targets}, and its configuration for your machine is below.
# Install packages {{future}}, {{future.callr}}, and {{future.batchtools}} to allow use_targets() to configure tar_make_future() options.

# Run the R scripts in the R/ folder with your custom functions:
tar_source()
# source("other_functions.R") # Source other scripts as needed.

# Replace the target list below with your own:
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
      r
    }
  ),
  tar_target(
    name = example_square_plot,
    command = {
      ggplot(data = example_square$cells, aes(col, row)) +
        geom_text(data = example_square$cells |> filter(!is.na(first)), aes(label = paste(first, second, sep = ","))) +
        geom_segment(data = grid_lines(9, 9), aes(x = x, y = y, xend = xend, yend = yend), linewidth = .1) +
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
    command = ggsave(plot = example_square_plot, filename = "img/example-square-plot.png", bg = "white", width = 1000, height = 1000, units = "px"),
    format = "file"
  )
)
