#' Validate and filter by year
#'
#' Checks that the specified year is valid and filters the data accordingly.
#'
#' @param data A data frame containing a `year` column.
#' @param year A numeric value. Must be one of `2019` or `2020`.
#'
#' @return A filtered data frame containing only rows matching `year`.
#'
#' @importFrom dplyr filter
validate_year <- function(data, year) {
  yr <- year
  if (!year %in% c(2019, 2020)) {
    stop("Invalid year. Specified year must be 2019 or 2020.")
  }
  data <- data |>
    dplyr::filter(year == yr)
}

#' Validate and filter by region
#'
#' Checks that the specified region is valid and filters the data accordingly.
#'
#' @param data A data frame containing a `region` column.
#' @param region A character string. Must be one of `"Africa"`, `"Asia"`,
#'   `"Americas"`, `"Europe"`, or `"Oceania"`.
#'
#' @return A filtered data frame containing only rows matching `region`.
#'
#' @importFrom dplyr filter
validate_region <- function(data, region) {
  rgn <- region
  if (!region %in% c("Africa", "Asia", "Americas", "Europe", "Oceania")) {
    stop("Invalid region. Specified region must be one of Africa, Asia, Americas, Europe or Oceania.")
  }
  data <- data |>
    dplyr::filter(region == rgn)
}
