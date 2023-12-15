all_pairs <- function(x) {
  
  y <- combn(0:(x - 1), 2)
  
  tibble(
      first = y[1,],
     second = y[2,]
  ) %>%
  mutate(ffs = map2(first, second, c)) %>%
  pull(ffs)

}

all_ordered_pairs <- function(x) {
  
  y <- tidyr::expand_grid(i = 1:x, j = 1:x)
  
  y %>%
    mutate(ffs = map2(i, j, c)) %>%
    pull(ffs)
  
}
