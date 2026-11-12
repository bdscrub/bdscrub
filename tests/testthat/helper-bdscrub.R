# Small helpers for the tests.
phone_valid <- function(x, ...) bd_phone(x, details = TRUE, warn = FALSE, ...)$valid
phone_operator <- function(x) bd_phone(x, details = TRUE, warn = FALSE)$operator
div_place <- function(x, ...) bd_place(x, level = "division", ...)
