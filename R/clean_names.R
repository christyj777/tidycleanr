#' @title Clean Column Names
#' A wrapper around janitor::clean_names() that converts messy column
#' names into snake_case.
#'
#' @param df A data.frame or tibble.
#' @return A data.frame/tibble with cleaned column names and an extra
#'   S3 class \code{tidycleanr_cleaned}.
#'
#' @examples
#' x <- data.frame("First Name" = 1:3, "AGE " = 4:6, check.names = FALSE)
#' y <- clean_names(x)
#' names(y)
#' @export
clean_names <- function(df) {
  # defensive checks
  if (!is.data.frame(df)) {
    stop("`df` must be a data.frame or tibble.", call. = FALSE)
  }
  if (is.null(names(df))) {
    stop("`clean_names()` requires that the data frame has column names.", call. = FALSE)
  }
  if (length(df) == 0L) {
    warning("`df` has no columns; nothing to clean.", call. = FALSE)
    return(df)
  }
  old_names <- names(df)
  df <- janitor::clean_names(df)   # ✅ FIXED — pass entire data frame

  attr(df, "old_names") <- old_names
  class(df) <- c("tidycleanr_cleaned", class(df))
  df
}
#' @export
print.tidycleanr_cleaned <- function(x, ...) {
  old <- attr(x, "old_names")
  if (!is.null(old)) {
    cat("tidycleanr_cleaned object. Name changes:\n")
    changed <- data.frame(old = old, new = names(x),
                          stringsAsFactors = FALSE)
    changed <- changed[changed$old != changed$new, , drop = FALSE]
    if (nrow(changed)) {
      print(changed, row.names = FALSE)
    } else {
      cat("  (no changes)\n")
    }
  }
  NextMethod()
}
