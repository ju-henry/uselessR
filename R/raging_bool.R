#' Raging boolean class
#'
#' An object of class 'raging_bool' is logical vector with a 'rage' attribute.
#' Based on this attribute, values can randomly, silently (and mercilessly!) be
#' inverted when working with or printing them.
#'
#' @param x logical vector
#' @return object of class 'raging_bool'
#' @export
#' @examples
#' a <- raging_bool(c(TRUE, FALSE))
#' a
raging_bool <- function(x = logical()) {
  x <- as.logical(x)
  attr(x, "rage") <- 1 / sample(x = 2:10, size = 1, prob = c(5, 8, 12, 15, 20, 15, 12, 8, 5) / 100)
  class(x) <- "raging_bool"
  x
}

#' @rdname raging_bool
#' @export
print.raging_bool <- function(x, ...) {
  if (runif(1) > attr(x, "rage")) {
    cat("<raging_bool> ", paste(ifelse(is.na(x), "NA", ifelse(x, "T", "F")), collapse = " "), "\n", sep = "")
  } else {
    cat("<raging_bool> ", paste(ifelse(is.na(x), "NA", ifelse(x, "F", "T")), collapse = " "), "\n", sep = "")
  }
  invisible(x)
}

#' @rdname raging_bool
#' @export
`!.raging_bool` <- function(x) {
  res <- !as.logical(x)
  attributes(res) <- attributes(x)
  res
}

#' @rdname raging_bool
#' @export
`&.raging_bool` <- function(e1, e2) {
  a <- as.logical(e1)
  b <- as.logical(e2)
  res <- a & b
  class(res) <- "raging_bool"
  attr(res, "rage") <- (attr(e1, "rage") + attr(e2, "rage")) / 2
  res
}

#' @rdname raging_bool
#' @export
`|.raging_bool` <- function(e1, e2) {
  a <- as.logical(e1)
  b <- as.logical(e2)
  res <- a | b
  class(res) <- "raging_bool"
  attr(res, "rage") <- c(attr(e1, "rage"), attr(e2, "rage"))[sample(1:2, 1)]
  res
}

#' @rdname raging_bool
#' @export
as.logical.raging_bool <- function(x, ...) {
  if (runif(1) < attr(x, "rage")) {
    !unclass(x)
  } else {
    unclass(x)
  }
}

#' @rdname raging_bool
#' @export
is.raging_bool <- function(x) {
  inherits(x, "raging_bool")
}

#' @rdname raging_bool
#' @export
`[.raging_bool` <- function(x, i) {
  res <- NextMethod()
  attributes(res) <- attributes(x)
  res
}

#' @rdname raging_bool
#' @export
c.raging_bool <- function(..., recursive = FALSE, use.names = TRUE) {

  dots <- list(...)

  # Use the default function when at least one foreign class is present
  if (!all(sapply(dots, inherits, "raging_bool"))) {
    return(NextMethod("c", recursive = recursive, use.names = use.names))
  }

  # Get all boolean values
  values <- unlist(lapply(dots, as.logical), recursive = FALSE, use.names = FALSE)

  # Compute new rage attribute
  attr_rage <- sapply(dots, attr, "rage")
  length_elements <- sapply(dots, length)
  attr_rage_new <- sum(attr_rage * length_elements) / sum(length_elements)

  # Build result
  res <- structure(values,
                   rage = attr_rage_new,
                   class = "raging_bool")

  res
}
