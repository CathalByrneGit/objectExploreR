#' @keywords internal
"_PACKAGE"

# `%like%` is dbplyr's SQL LIKE operator. It only appears inside os_filter()
# expressions, which dbplyr translates to SQL rather than evaluating in R.
utils::globalVariables("%like%")
