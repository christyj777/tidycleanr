#' Remove duplicate rows from a data frame
#'
#' @description
#' Removes duplicate rows from a data frame. The function also
#' records how many rows were removed.
#'
#' @param df A data frame or tibble.
#' @param by Optional character vector of column names. If supplied,
#'   duplicates are defined using only these columns. If `NULL`
#'   (the default), entire rows are compared.
#'
#' @return A data frame of the same class as `df`, with duplicates removed.
#'   An attribute `"duplicates_removed"` stores the number of rows removed.
#'
#' @examples
#' x <- data.frame(a = c(1, 1, 2), b = c("x", "x", "y"))
#' remove_duplicates(x)
#'
#' remove_duplicates(x, by = "a")
#'
#' @export
remove_duplicates <- function(df, by = NULL) {
  if (!is.data.frame(df)) {
    stop("`df` must be a data frame.", call. = FALSE)
  }

  if (!is.null(by)) {
    if (!all(by %in% names(df))) {
      stop("All `by` columns must exist in `df`.", call. = FALSE)
    }
    dup_index <- duplicated(df[by])
  } else {
    dup_index <- duplicated(df)
  }

  out <- df[!dup_index, , drop = FALSE]

  # reset row names so they’re 1, 2, 3, ...
  rownames(out) <- NULL

  attr(out, "duplicates_removed") <- sum(dup_index)

  out
}
