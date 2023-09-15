#
#
# set.seed(1234)
# swiss <- dplyr::bind_cols(swiss,
#                           treatment = rbinom(seq(nrow(swiss)),1, 0.5))
# reg1 <- lm(formula = Education ~ treatment, swiss)
# reg2 <- lm(formula = Education ~ treatment + Catholic, swiss)
# reg3 <- lm(formula = Education ~ treatment * Catholic, swiss)
# models <- list(reg1,
#               reg2,
#               reg3)
# results <- collate_models(models = models)
#
# ?broom::tidy()
#
# # Simple coef plot
# results |>
#   filter(modelID == 1 & term == "treatment") |>
#   ggplot(aes(estimate, term, xmin = conf.low, xmax = conf.high, height = 0)) +
#     geom_point(color = "white") +
#     geom_vline(xintercept = 0, lty = 4, color = "white") +
#     geom_errorbarh(color = "white") +
#     SahelGraphR::themeaspdark()
#
# # Predicted values histogram
# predicted_data <- broom::augment(reg3, type.predict = "response")
# # Extract predicted values by binary treatment status
# predicted_values <- predicted_data %>%
#   group_by(treatment) %>%
#   summarize(mean_predicted_value = mean(.fitted))
#
