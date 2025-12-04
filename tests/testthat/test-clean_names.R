test_that("clean_names validates input", {
  expect_error(
    clean_names(1:3),
    "`df` must be a data.frame or tibble"
  )
})

test_that("clean_names cleans and records old names", {
  x <- data.frame("First Name" = 1:2,
                  "AGE " = 3:4,
                  check.names = FALSE)

  y <- clean_names(x)

  expect_s3_class(y, "tidycleanr_cleaned")
  expect_named(y, c("first_name", "age"))

  old <- attr(y, "old_names")
  expect_equal(old, c("First Name", "AGE "))
})
