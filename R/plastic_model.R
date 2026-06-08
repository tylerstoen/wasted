#' Fit a mixed effects model for plastic waste
#'
#' Summarizes data to the country-year level and fits a linear mixed effects
#' model predicting log total plastic waste, with a random intercept for
#' country. Returns a formatted gt table of the fixed effects.
#'
#' @param data A data frame containing joined plastic waste, GDP, population,
#'   and region data.
#'
#' @return A gt table of fixed effects from the fitted lmer model.
#'
#' @examples
#' data <- load_data()
#' fit_plastic_lmer(data)
#' @export
#'
#' @importFrom dplyr group_by summarise filter mutate
#' @importFrom lme4 lmer
#' @importFrom broom.mixed tidy
#' @importFrom gt gt fmt_number cols_label tab_header
fit_plastic_lmer <- function(data) {
  country_year <- data |>
    dplyr::group_by(.data$country, .data$year, .data$region) |>
    dplyr::summarise(
      total_plastic = sum(.data$grand_total, na.rm = TRUE),
      gdp = mean(.data$gdp, na.rm = TRUE),
      population = mean(.data$population, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::filter(
      .data$total_plastic > 0,
      !is.na(.data$gdp),
      !is.na(.data$region),
      !is.na(.data$country)
    ) |>
    dplyr::mutate(
      log_total_plastic = log(.data$total_plastic),
      log_gdp = log(.data$gdp)
    )

  model <- lme4::lmer(
    log_total_plastic ~ log_gdp * region +
      (1 | country),
    data = country_year
  )

  broom.mixed::tidy(model, effects = "fixed") |>
    dplyr::select(-effect) |>
    gt::gt() |>
    gt::fmt_number(
      columns = c(estimate, std.error, statistic),
      decimals = 3
    ) |>
    gt::cols_label(
      term = "Predictor",
      estimate = "Estimate",
      std.error = "SE",
      statistic = "t value"

    ) |>
    gt::tab_header(
      title = "Model Results: Fixed Effects"
    )
}

