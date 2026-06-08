#' Get Country Summary Statistics
#'
#' Returns a tibble with key plastic waste statistics for a given country,
#' including total plastic, per capita figures, GDP per capita, global ranks,
#' and a breakdown by plastic type.
#'
#' @param data A tibble returned by \code{load_data()}.
#' @param country A string giving the country name (e.g. "Montenegro").
#' @param year Optional numeric year (2019 or 2020). If NULL, both years are
#'   combined.
#'
#' @returns A tibble with one row containing summary statistics for the
#'   specified country.
#' @export
#'
#' @importFrom dplyr filter group_by summarise mutate across all_of bind_cols left_join pull desc
#' @importFrom stringr str_to_title
#'
#' @examples
#' data <- load_data()
#' get_country_summary(data, "Montenegro")
#' get_country_summary(data, "Philippines", year = 2020)
get_country_summary <- function(data, country, year = NULL) {

  cntry <- country

  plastic_types <- c("hdpe", "ldpe", "o", "pet", "pp", "ps", "pvc")

  country_stats <- compute_country_stats(data, year = year) |>
    dplyr::filter(country == stringr::str_to_title(cntry))

  if (nrow(country_stats) == 0) {
    stop(paste0("Country '", country, "' not found in the dataset."))
  }

  plastic_breakdown <- data |>
    dplyr::filter(
      country == stringr::str_to_title(cntry),
      parent_company != "Grand Total"
    ) |>
    dplyr::summarise(
      dplyr::across(dplyr::all_of(plastic_types), ~ sum(.x, na.rm = TRUE))
    ) |>
    dplyr::rename(other = o, polypropylene = pp, polystyrene = ps)

  dplyr::bind_cols(country_stats, plastic_breakdown)
}
