# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes) # Load other packages as needed. # nolint

# Set target options:
tar_option_set(
  packages = c("dplyr", "here", "purrr", "readr", "tibble"), # packages that your targets need to run
  format = "rds" # default storage format
  # Set other options as needed.
)

# tar_make_clustermq() configuration (okay to leave alone):
options(clustermq.scheduler = "multicore")

# tar_make_future() configuration (okay to leave alone):
# Install packages {{future}}, {{future.callr}}, and {{future.batchtools}} to allow use_targets() to configure tar_make_future() options.

# Run the R scripts in the R/ folder with your custom functions:
tar_source()
# source("other_functions.R") # Source other scripts as needed. # nolint

list(
  tar_target(orders, seq(10, 50, 2)),
  tar_target(seeds, 42:44),
  tar_target(
    results,
    random_room(orders, seeds),
    pattern = cross(orders, seeds)
  ),
  tar_target(
    name = meszka,
    command = read_rds(here("data", "meszka-rosa-table.rds"))
  ),
  tar_target(
    name = final_results,
    command = {
      results %>%
        group_by(n) %>%
        summarise(t3 = max(n_filled)) %>%
        left_join(meszka) %>%
        mutate(
          `t3 - t1` = t3 - t1,
                 r3 = t3 / nc2
        ) %>%
        select(n, nc2, t1, t2, t3, r1, r2, r3)
    }
  ),
  tar_render(
    name = readme,
    "README.Rmd"
  )
)
