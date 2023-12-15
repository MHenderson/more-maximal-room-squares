library(R6)

Room <- R6Class(
  classname = "Room",
  public = list(

           size = NULL,
          cells = NULL,
        symbols = NULL,
     free_pairs = NULL,
    empty_cells = NULL,
    
    initialize = function(size = NA) {
      self$size <- size
      self$symbols <- 0:(self$size - 1)
      self$cells <- tidyr::expand_grid(row = 1:(self$size - 1), col = 1:(self$size - 1)) %>%
        mutate(first = as.integer(NA), second = as.integer(NA)) %>%
        mutate(avail = list(0:(self$size - 1)))
      self$free_pairs <- all_pairs(self$size)
      self$empty_cells <- all_ordered_pairs(self$size - 1)
    },
    
    set = function(e, p) {
      self$cells[self$cells$row == e[1] & self$cells$col == e[2], "first"] <- p[1]
      self$cells[self$cells$row == e[1] & self$cells$col == e[2], "second"] <- p[2]
      self$cells[self$cells$row == e[1], "avail"]$avail <- lapply(self$cells[self$cells$row == e[1], "avail"]$avail, remove_both, p)
      self$cells[self$cells$col == e[2], "avail"]$avail <- lapply(self$cells[self$cells$col == e[2], "avail"]$avail, remove_both, p)
      self$free_pairs <- self$free_pairs[-match(list(p), self$free_pairs)]
      self$empty_cells <- self$empty_cells[-match(list(e), self$empty_cells)]
    },
    
    is_available = function(e, p) {
      p[1] %in% self$cells[self$cells$row == e[1] & self$cells$col == e[2], "avail"]$avail[[1]] && p[2] %in% self$cells[self$cells$row == e[1] & self$cells$col == e[2], "avail"]$avail[[1]]
    }

  ),
  active = list(
    
    n_filled = function() {
      self$cells %>%
        filter(!is.na(first)) %>% 
        nrow()
    },
    
    volume = function() {
      round(self$n_filled/choose(max(self$cells$col) + 1, 2), 6)
    }

  )
)

