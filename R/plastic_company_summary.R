#' Summarize plastic waste by company
#'
#' Calculates the total recorded plastic waste for each parent company
#' and returns the top companies by total plastic waste.
#'
#' @param data A data frame containing plastic waste data.
#' @param top_n Number of companies to return. Defaults to 10.
#' @param region Optional character string to filter to a single region.
#'   If NULL all regions are shown.
#' @param year Optional numeric year (2019 or 2020). If NULL both years
#'   are combined.
#'
#' @return A tibble containing parent companies and total plastic waste.
#' @importFrom dplyr filter group_by summarise arrange desc slice_head
#' @examples
#' data <- load_data()
#' plastic_by_company(data)
#' plastic_by_company(data, top_n = 5)
#' plastic_by_company(data, region = "Asia")
#' plastic_by_company(data, year = 2020)
#' @export
plastic_by_company <- function(data, top_n = 10, region = NULL, year = NULL) {

  if (!is.null(year))   data <- validate_year(data, year)
  if (!is.null(region)) data <- validate_region(data, region)

  data |>
    dplyr::filter(
      !is.na(parent_company),
      parent_company != "Grand Total",
      parent_company != "NULL",
      parent_company != "null",
      parent_company != "Unbranded",
      parent_company != "Assorted",
    ) |>
    dplyr::group_by(parent_company) |>
    dplyr::summarise(
      total_plastic = sum(grand_total, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::arrange(dplyr::desc(total_plastic)) |>
    dplyr::slice_head(n = top_n)
}
