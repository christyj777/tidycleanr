test_that("remove_duplicates removes exact duplicate rows", {
  x <- data.frame(
    a = c(1, 1, 2),
    b = c("x", "x", "y")
  )

  y <- remove_duplicates(x)

  expect_equal(
    y,
    data.frame(a = c(1, 2), b = c("x", "y")),
    ignore_attr = TRUE
  )
  expect_equal(attr(y, "duplicates_removed"), 1)
})

test_that("remove_duplicates works with `by` columns", {
  x <- data.frame(
    id  = c(1, 1, 2),
    val = c("x", "y", "y")
  )

  y <- remove_duplicates(x, by = "id")

  expect_equal(
    y,
    data.frame(
      id  = c(1, 2),
      val = c("x", "y")
    ),
    ignore_attr = TRUE
  )
  expect_equal(attr(y, "duplicates_removed"), 1)
})
