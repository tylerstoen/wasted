#' Compute Country-Level Summary Statistics
#'
#' Internal helper function that aggregates plastic waste data to the country
#' level and computes global ranks. Used by \code{get_country_summary()} and
#' other summary functions.
#'
#' @param data A tibble returned by \code{load_data()}.
#' @param year Optional numeric year (2019 or 2020). If NULL, both years are
#'   combined.
#'
#' @returns A tibble with one row per country containing totals, per capita
#'   figures, and global ranks.
#' @keywords internal
#'
#' @importFrom dplyr filter group_by summarise mutate desc
#' @export
compute_country_stats <- function(data, year = NULL) {

  yr <- year

  if (!is.null(yr)) {
    data <- validate_year(data, yr)
  }

  data |>
    dplyr::filter(
      parent_company != "Grand Total"
    ) |>
    dplyr::group_by(country, region) |>
    dplyr::summarise(
      total_plastic    = sum(grand_total, na.rm = TRUE),
      population       = mean(population, na.rm = TRUE),
      gdp_per_capita   = mean(gdppercapita, na.rm = TRUE),
      .groups          = "drop"
    ) |>
    dplyr::mutate(
      plastic_per_capita = total_plastic / population,
      total_rank         = rank(dplyr::desc(total_plastic),      ties.method = "min"),
      per_capita_rank    = rank(dplyr::desc(plastic_per_capita), ties.method = "min"),
      gdp_rank           = rank(dplyr::desc(gdp_per_capita),     ties.method = "min")
    )
}
