test_that("property_type_to_widget maps string to text", {
  expect_equal(property_type_to_widget("string"), "text")
})

test_that("property_type_to_widget maps integer to numeric_range", {
  expect_equal(property_type_to_widget("integer"), "numeric_range")
})

test_that("property_type_to_widget maps number to numeric_range", {
  expect_equal(property_type_to_widget("number"), "numeric_range")
})

test_that("property_type_to_widget maps boolean to checkbox", {
  expect_equal(property_type_to_widget("boolean"), "checkbox")
})

test_that("property_type_to_widget maps date to date_range", {
  expect_equal(property_type_to_widget("date"), "date_range")
})

test_that("property_type_to_widget maps datetime to date_range", {
  expect_equal(property_type_to_widget("datetime"), "date_range")
})

test_that("property_type_to_widget maps json to none", {
  expect_equal(property_type_to_widget("json"), "none")
})

test_that("property_type_to_widget returns none for unknown types", {
  expect_equal(property_type_to_widget("unknown"), "none")
})
