#' Collate regression models into a standardised tibble
#'
#' @param models list of regression models supported by `{broom}`
#'
#' @return A tibble with statistics for multiple models.
#' @export
#'
#' @importFrom rlang .data
#'
#' @examples
#' # Use the  R Swiss data for examples with a random treatment
#' set.seed(1234)
#' swiss <- dplyr::bind_cols(swiss,
#'                            treatment = rbinom(seq(nrow(swiss)),1, 0.5)) |>
#'          dplyr::mutate(Education = Education + 0.15 * treatment)
#'
#' reg1 <- lm(formula = Education ~ treatment, swiss)
#' reg2 <- lm(formula = Education ~ treatment + Catholic, swiss)
#' reg3 <- lm(formula = Education ~ treatment * Catholic, swiss)
#'
#' models <- list(reg1,
#'                reg2,
#'                reg3)
#' collate_models(models = models)

collate_models <- function(models) {
  # Check input
  if (!is.list(models)) {
    stop("Error: Models have to be in a list.")
  }
  # Broom and append to model collection tibble
  results <- dplyr::tibble()
  for (i in seq(length(models))) {
    results <- results |>
      dplyr::bind_rows(dplyr::tibble(
        modelID = i,
        broom::tidy(models[[i]], conf.int = T),
        dplyr::rename(
          broom::glance(models[[i]]),
          statistic_model = .data$statistic,
          p.value_model = .data$p.value
        )
      ))
  }
  results
}

#' Collate predicted values of a list of models into a large data frame.
#'
#' @param models List of regression models.
#' @param by String of the variable name to take the mean prediction by. `NULL`
#' by default.
#'
#' @return A tibble containing the collated predictions grouped by treatment or
#' not.
#' @export
#'
#' @examples
#' # Generate an example dataset
#' swiss2 <- dplyr::bind_cols(swiss,
#'                           treatment = rbinom(seq(nrow(swiss)),1, 0.5)) |>
#'   dplyr::mutate(Education = Education + 2.5 * treatment)
#' reg1 <- lm(formula = Education ~ treatment, swiss2)
#' reg2 <- lm(formula = Education ~ treatment + Catholic, swiss2)
#' reg3 <- lm(formula = Education ~ treatment * Catholic, swiss2)
#' models <- list(reg1,
#'                reg2,
#'                reg3)
#' # Full predictions:
#' collate_predictions(models = models)
#' # Grouped by model and treatment status:
#' collate_predictions(models = models, by = "treatment")


collate_predictions <- function(models, by = NULL) {
  # Check input
  if (!is.list(models)) {
    stop("Error: Models have to be in a list.")
  }
  # Broom and append to model collection tibble
  results <- dplyr::tibble()
  for (i in seq(length(models))) {
    results <- results |>
      dplyr::bind_rows(dplyr::tibble(modelID = i,
                                     broom::augment(models[[i]], conf.int = T)))
  }
  if (!is.null(by)) {
    results |>
      dplyr::group_by(.data[["modelID"]], .data[[by]]) |>
      dplyr::summarise(mean_fitted_value = mean(.data[[".fitted"]]), .groups = "drop")
  } else {
    results
  }
}
