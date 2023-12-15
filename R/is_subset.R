is_subset <- function(x, S) {
  setequal(union(x, S), S)
}