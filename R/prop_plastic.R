#' Calculate plastic type proportions
#'
#' Calculates the proportion of each plastic type out of the total recorded
#' plastic waste, optionally filtered by region or year.
#'
#' @param data A data frame containing plastic waste data.
#' @param region Optional character string to filter to a single region.
#'   If NULL all regions are shown.
#' @param year Optional numeric year (2019 or 2020). If NULL both years
#'   are combined.
#'
#' @return A data frame containing plastic type proportions by year.
#' @examples
#' data <- load_data()
#' plastic_type_summary(data)
#' plastic_type_summary(data, region = "Asia")
#' plastic_type_summary(data, year = 2020)
#' @export
#' @importFrom dplyr filter group_by summarise mutate recode rename
#' @importFrom purrr map_dfr
#' @importFrom tidyr pivot_wider
plastic_type_summary <- function(data, region = NULL, year = NULL) {

  if (!is.null(year))   data <- validate_year(data, year)
  if (!is.null(region)) data <- validate_region(data, region)

  plastic_vars <- c("empty", "hdpe", "ldpe", "o", "pet", "pp", "ps", "pvc")

  calc_prop <- function(var, data) {
    data |>
      dplyr::filter(
        .data$grand_total > 0,
        .data$parent_company != "Grand Total"
      ) |>
      dplyr::group_by(year) |>
      dplyr::summarise(
        prop = sum(.data[[var]], na.rm = TRUE) /
          sum(grand_total, na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::mutate(plastic_type = var)
  }

  purrr::map_dfr(plastic_vars, calc_prop, data = data) |>
    dplyr::mutate(
      plastic_type = dplyr::recode(
        plastic_type,
        empty = "Empty",
        hdpe = "HDPE",
        ldpe = "LDPE",
        o = "Other",
        pet = "PET",
        pp = "PP",
        ps = "PS",
        pvc = "PVC"
      )
    ) |>
    tidyr::pivot_wider(
      names_from = plastic_type,
      values_from = prop
    ) |>
    dplyr::rename(Year = year)
}

