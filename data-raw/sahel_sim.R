## code to prepare `sahel_sim` dataset goes here
set.seed(1)

sahel_sim <- fabricatr::fabricate(
  country = fabricatr::add_level(
    N = 4,
    country_names = c("Burkina Faso", "Mauritania", "Niger", "Senegal"),
    sample_size = c(1500, 2000, 1800, 2500),
    # Number of Surveyed HH in each Cty
    income_FE = c(2, 2.2, 4, 3.1),
    consumption_FE = c(1.5, 2.2, 3, 2.8)
  ),
  hh = fabricatr::add_level(
    N = sample_size,
    # Household Size
    hh_size = runif(N, 0, 10),
    hh_size_mean_country = ave(hh_size, country),
    # Gender of hh head
    female_head = fabricatr::draw_binary(N = N, prob = 0.15),
    # Owns Farm Animals and how many
    owns_animals = fabricatr::draw_binary(N = N, prob = 0.75),
    animals_nr = ifelse(owns_animals == 1, fabricatr::draw_count(12, N = N), 0),
    # Years of Education
    yr_educ_hhmean = sample(
      1:12 ,
      N ,
      prob = c(runif(5, 1, 5), 6, runif(5, 1, 5), 6),
      replace = TRUE
    ),
    yr_educ_mean_country = ave(yr_educ_hhmean, country),
    # Country averages
    # Has children and number of children:
    has_children = fabricatr::draw_binary(N = N, prob = 0.8),
    children_nr = ifelse(has_children == 1, fabricatr::draw_count(3, N = N), 0),
    # Household Income
    hh_income = rlnorm(N, meanlog = income_FE, sdlog = 1.5),
    # Household Consumption
    hh_consumption = rlnorm(N, meanlog = consumption_FE, sdlog = 0.8)
  )
)


usethis::use_data(sahel_sim, overwrite = TRUE)
