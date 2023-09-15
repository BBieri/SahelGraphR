
#' Sahel Prepare ggplot function
#'
#' This function prepares standard ASP data from Stata into a format ready for
#' plotting with one of the included plotting functions.
#'
#' @param data Dataframe to pivot.
#' @param key String. Name of the variable to pivot on.
#' @param value String. Name of the value variable.
#' @param ... Additional agruments passed to `tidyr::gather()` such as variables
#' to pivot or `tidyselect` expressions
#'
#' @return A plotting ready tibble.
#' @export
#'
#' @examples
#' sahel_sim_long_monetary <- tidyr::gather(sahel_sim, key = "variable",
#'                                          value = "value", hh_income,
#'                                          hh_consumption)
#' sahel_sim_long_count <- tidyr::gather(sahel_sim, key = "variable",
#'                                        value = "value", children_nr,
#'                                        animals_nr, yr_educ_hhmean)
#' sahel_sim_long_binary <- tidyr::gather(sahel_sim, key = "variable",
#'                                        value = "value", has_children,
#'                                        owns_animals, female_head)
#'
#' dplyr::summarize(sahel_sim_long_monetary)
#' dplyr::summarize(sahel_sim_long_count)
#' dplyr::summarize(sahel_sim_long_binary)

sahel_prep_gg <- function(data, key = "variable", value = "value", ...) {
  tidyr::gather(data, key = "variable", value = "value", ...)
}


#' Prepare Sahel binary variables for plotting.
#'
#' @param data A data frame, data frame extension (e.g. a tibble), or a
#' lazy data frame (e.g. from dbplyr or dtplyr)
#' @param ... Passed to group_by(), variables or computations to group by.
#' Computations are always done on the ungrouped data frame. To perform
#' computations on the grouped data, you need to use a separate mutate()
#' step before the group_by(). Computations are not allowed in nest_by().
#' In ungroup(), variables to remove from the grouping.
#' @param alpha Signifiicance level for confidence interval
#'
#' @return A dataframe ready for plotting. Computes the
#' @export
#'
#' @examples
#' sahel_sim_long_binary <- tidyr::gather(sahel_sim, key = "variable",
#'                                        value = "value", has_children,
#'                                        owns_animals, female_head)
#' sahel_prep_gg_binary(sahel_sim_long_binary, country_names, variable,
#'                        alpha = 0.05)

sahel_prep_gg_binary <- function(data,
                                 ...,
                                 alpha = 0.05){
  data |>
    dplyr::group_by(...) |>
    dplyr::summarize(
      mean = mean(value),
      sd = sd(value),
      se = sd(value) / sqrt(length(value)),
      t = qt((1 - alpha) / 2 + .5, length(value) - 1),
      CI = t * se
    )
}

#' Sahel Prepare data for `sahel_ggcoefs()`
#'
#' This function prepares the coefficient information saved post regression
#' workflow in Stata and makes them ready for plotting.
#'
#' @param data A data frame filled with regression output from Stata.
#' @param variables A character vector of variable names to plot.
#' @param var_labels A chacater vector of corresponding variable names.
#' @param measure A string indicating the measure to plot e.g. `"avg", b`
#' @param model_number Some dataframes have a mht_family variable indicating the
#'  model type number. Select the model you want to plot by passing it the
#'  corresponding number.
#'
#' @return A dataframe ready for barchart plotting.
#' @export
#'
#' @examples
#' # Don't forget to mount the data if using the actual data
#' # prodregs <-
#' #  haven::read_dta(r"(U:\fu2_MRT\05_Regstats\fu2_MRT_regstats_hh_prod.dta)")
#'
#' # Example with simulated data
#' set.seed(1234)
#' prodregs <- fabricatr::fabricate(N = 8,
#'                                  var_name = c("consum_2_day_eq_ppp",
#'                                                "consum_2_day_ppp",
#'                                                "food_2_day_eq_ppp",
#'                                                "food_2_day_ppp",
#'                                                "food_2_day_g_ppp",
#'                                                "food_2_day_g_eq_ppp",
#'                                                "food_2_g_d",
#'                                                "FIES_rvrs_raw"),
#'                                  b0 = 0,
#'                                  b1 = rnorm(N, 0, 1),
#'                                  b2 = rnorm(N, 0, 1),
#'                                  b3 = rnorm(N, 0, 1),
#'                                  avg0 = rnorm(N, 7, 1),
#'                                  avg1 = rnorm(N, 8, 1),
#'                                  avg2 = rnorm(N, 9, 1),
#'                                  avg3 = rnorm(N, 10, 1),
#'                                  se0 = rnorm(N, 0, 1),
#'                                  se1 = rnorm(N, 0, 1),
#'                                  se2 = rnorm(N, 0, 1),
#'                                  se3 = rnorm(N, 0, 1),
#'                                  ci95_0 = rnorm(N, 2, 1),
#'                                  ci95_1 = rnorm(N, 2, 1),
#'                                  ci95_2 = rnorm(N, 2, 1),
#'                                  ci95_3 = rnorm(N, 2, 1),
#'                                  p0 = rnorm(N, 0.8, 0.5),
#'                                  p1 = rnorm(N, 0.8, 0.5),
#'                                  p2 = rnorm(N, 0.8, 0.5),
#'                                  p3 = rnorm(N, 0.8, 0.5),
#'                                  mht_family = 1) |>
#'   dplyr::select(-ID)
#'
#'  sahel_prep_ggcoefs(prodregs)

sahel_prep_ggcoefs <- function(data,
                               variables = c("consum_2_day_eq_ppp", "consum_2_day_ppp", "food_2_day_eq_ppp",
                                             "food_2_day_ppp", "food_2_day_g_ppp", "food_2_day_g_eq_ppp",
                                             "food_2_g_d", "FIES_rvrs_raw"),
                               var_labels = c("Daily Cons.\n Ad. Equiv.", "Daily Consumption", "Daily Food Cons.\n Ad. Equiv",
                                              "Daily Food Cons.", "Gifted Daily Food\n Cons. Ad. Equiv", "Gifted Daily\n Food Cons.",
                                              "Consumed Gifted\n Food {0,1}", "Food Security"),
                               measure = "avg",
                               model_number = 1) {
  # Input checks Pending
  # Prep data
  out <- data |>
    dplyr::filter(var_name %in% variables &
                    mht_family == model_number) |>
    tidyr::pivot_longer(cols = -c(var_name, mht_family),
                        names_to = "Variable",
                        values_to = "Value") |>
    dplyr::mutate(Treatment = ifelse(stringr::str_detect(Variable, "0"), "Control",
                                     ifelse(stringr::str_detect(Variable, "1"), "Psychosocial",
                                            ifelse(stringr::str_detect(Variable, "2"), "Cash", "Full"))),
                  Treatment = factor(Treatment, levels = c("Control", "Psychosocial", "Cash", "Full")),
                  var_name = factor(var_name, levels = variables, labels = var_labels))

  # Some dark magic pivoting. There is certainly an easier way of doing this
  out |>
    dplyr::filter(stringr::str_detect(Variable, "ci95")) |>
    dplyr::inner_join(dplyr::filter(out, stringr::str_detect(Variable, measure)),
                      c("var_name", "Treatment", "mht_family")) |>
    dplyr::mutate(Value.x = ifelse(stringr::str_detect(Variable.x, "_0"), NA, Value.x))
}
