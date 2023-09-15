# set.seed(1)
# library(fabricatr)
# library(tidyverse)
# library(ggridges)
# library(patchwork)
#
# sahel_sim <- fabricate(
#   country = add_level(
#     N = 4,
#     country_names = c("Burkina Faso", "Mauritania", "Niger", "Senegal"),
#     sample_size = c(1500, 2000, 1800, 2500), # Number of Surveyed HH in each Cty
#     income_FE = c(2, 2.2, 4, 3.1),
#     consumption_FE = c(1.5, 2.2, 3, 2.8)
#   ),
#   hh = add_level(
#     N = sample_size,
#     # Household Size
#     hh_size = runif(N, 0, 10),
#     hh_size_mean_country = ave(hh_size, country),
#     # Gender of hh head
#     female_head = draw_binary(N = N, prob = 0.15),
#     # Owns Farm Animals and how many
#     owns_animals = draw_binary(N = N, prob = 0.75),
#     animals_nr = ifelse(owns_animals == 1, draw_count(12, N = N), 0),
#     # Years of Education
#     yr_educ_hhmean = sample(1:12 ,N , prob = c(runif(5, 1, 5), 6, runif(5, 1, 5), 6), replace = TRUE),
#     yr_educ_mean_country = ave(yr_educ_hhmean, country), # Country averages
#     # Has children and number of children:
#     has_children = draw_binary(N = N, prob = 0.8),
#     children_nr = ifelse(has_children == 1, draw_count(3, N = N), 0),
#     # Household Income
#     hh_income = rlnorm(N, meanlog = income_FE, sdlog = 1.5),
#     # Household Consumption
#     hh_consumption = rlnorm(N, meanlog = consumption_FE, sdlog = 0.8)
#   )
# )
#
# # Geom Half Violin ----
# ## Vertical ----
# "%||%" <- function(a, b) {
#   if (!is.null(a)) a else b
# }
#
# geom_flat_violin <- function(mapping = NULL, data = NULL, stat = "ydensity",
#                              position = "dodge", trim = TRUE, scale = "area",
#                              show.legend = NA, inherit.aes = TRUE, ...) {
#   layer(
#     data = data,
#     mapping = mapping,
#     stat = stat,
#     geom = GeomFlatViolin,
#     position = position,
#     show.legend = show.legend,
#     inherit.aes = inherit.aes,
#     params = list(
#       trim = trim,
#       scale = scale,
#       ...
#     )
#   )
# }
#
# GeomFlatViolin <-
#   ggproto("GeomFlatViolin", Geom,
#           setup_data = function(data, params) {
#             data$width <- data$width %||%
#               params$width %||% (resolution(data$x, FALSE) * 0.9)
#
#             # ymin, ymax, xmin, and xmax define the bounding rectangle for each group
#             data %>%
#               group_by(group) %>%
#               mutate(ymin = min(y),
#                      ymax = max(y),
#                      xmin = x - width / 2,
#                      xmax = x)
#           },
#
#           draw_group = function(data, panel_scales, coord) {
#             # Find the points for the line to go all the way around
#             data <- transform(data,
#                               xmaxv = x,
#                               xminv = x + violinwidth * (xmin - x))
#
#             # Make sure it's sorted properly to draw the outline
#             newdata <- rbind(plyr::arrange(transform(data, x = xminv), y),
#                              plyr::arrange(transform(data, x = xmaxv), -y))
#
#             # Close the polygon: set first and last point the same
#             # Needed for coord_polar and such
#             newdata <- rbind(newdata, newdata[1,])
#
#             ggplot2:::ggname("geom_flat_violin", GeomPolygon$draw_panel(newdata, panel_scales, coord))
#           },
#
#           draw_key = draw_key_polygon,
#
#           default_aes = aes(weight = 1, colour = "grey20", fill = "white", size = 0.5,
#                             alpha = NA, linetype = "solid"),
#
#           required_aes = c("x", "y")
#   )
# ## Horizontal ----
# "%||%" <- function(a, b) {
#   if (!is.null(a)) a else b
# }
#
# geom_flat_violin_h <- function(mapping = NULL, data = NULL, stat = "ydensity",
#                              position = "dodge", trim = TRUE, scale = "area",
#                              show.legend = NA, inherit.aes = TRUE, ...) {
#   layer(
#     data = data,
#     mapping = mapping,
#     stat = stat,
#     geom = GeomFlatViolin_H,
#     position = position,
#     show.legend = show.legend,
#     inherit.aes = inherit.aes,
#     params = list(
#       trim = trim,
#       scale = scale,
#       ...
#     )
#   )
# }
#
# GeomFlatViolin_H <-
#   ggproto("GeomFlatViolin_H", Geom,
#           setup_data = function(data, params) {
#             data$width <- data$width %||%
#               params$width %||% (resolution(data$y, FALSE) * 0.9)
#
#             # ymin, ymax, xmin, and xmax define the bounding rectangle for each group
#             data %>%
#               group_by(group) %>%
#               mutate(xmin = min(x),
#                      xmax = max(x),
#                      ymin = y,
#                      ymax = y - width / 2)
#           },
#
#           draw_group = function(data, panel_scales, coord) {
#             # Find the points for the line to go all the way around
#             data <- transform(data,
#                               ymaxv = y,
#                               yminv = y + violinwidth * (ymin - y))
#
#             # Make sure it's sorted properly to draw the outline
#             newdata <- rbind(plyr::arrange(transform(data, y = yminv), x),
#                              plyr::arrange(transform(data, y = ymaxv), -x))
#
#             # Close the polygon: set first and last point the same
#             # Needed for coord_polar and such
#             newdata <- rbind(newdata, newdata[1,])
#
#             ggplot2:::ggname("geom_flat_violin_h", GeomPolygon$draw_panel(newdata, panel_scales, coord))
#           },
#
#           draw_key = draw_key_polygon,
#
#           default_aes = aes(weight = 1, colour = "grey20", fill = "white", size = 0.5,
#                             alpha = NA, linetype = "solid"),
#
#           required_aes = c("x", "y")
#   )
#
#
# # Combine the variables in one
#
# sahel_sim_long_monetary <- tidyr::gather(sahel_sim, key = "variable", value = "value", hh_income, hh_consumption)
# sahel_sim_long_count <- tidyr::gather(sahel_sim, key = "variable", value = "value", children_nr, animals_nr, yr_educ_hhmean)
# sahel_sim_long_binary <- tidyr::gather(sahel_sim, key = "variable", value = "value", has_children, owns_animals, female_head)
#
# ### Continuous variable ----
# # Ridgeline
# ggplot(sahel_sim_long_monetary) +
#   geom_density_ridges(aes(x = value, y = variable, fill = country_names), alpha = .5) +
#   scale_fill_manual(c("Brukina Faso", "Niger", "Mauritania", "Senegal"), values = unname(SahelGraphR::asp_palettes$Dark[2:5])) +
#   scale_y_discrete(labels = c("Consumption", "Income")) +
#   scale_x_log10() +
#   labs(title = "Household Income & Household Consumption",  subtitle = "Ridgeline Plot", caption = "Synthetic Data",
#        x = "2016 USD PPP", y = "", fill = "Variable") +
#   SahelGraphR::themeaspdark()
# ggsave(filename = "plotcontinuousridge.jpg", dpi = 192)
#
# # Boxplot
# ggplot(sahel_sim_long_monetary, aes(x = value, y = variable, fill = country_names)) +
#   geom_boxplot(alpha = 0.8, color = "grey85") +
#   scale_fill_manual(c("Brukina Faso", "Niger", "Mauritania", "Senegal"), values = unname(SahelGraphR::asp_palettes$Dark[2:5])) +
#   scale_y_discrete(labels = c("Consumption", "Income")) +
#   scale_x_log10() +
#   labs(title = "Household Income & Household Consumption", subtitle = "Boxplot", caption = "Synthetic Data",
#        x = "2016 USD PPP", y = "", fill = "Variable") +
#   SahelGraphR::themeaspdark()
# ggsave(filename = "plotcontinuousboxplot.jpg", dpi = 192)
#
# # Violin full
# ggplot(sahel_sim_long_monetary, aes(x = value, y = variable, fill = country_names)) +
#   geom_violin(alpha = 0.9) +
#   geom_boxplot(width = 0.1, color = "grey90", alpha = 0.2, position = position_dodge(0.9)) +
#   scale_fill_manual(c("Brukina Faso", "Niger", "Mauritania", "Senegal"), values = unname(SahelGraphR::asp_palettes$Dark[2:5])) +
#   scale_y_discrete(labels = c("Consumption", "Income")) +
#   scale_x_log10() +
#   labs(title = "Household Income & Household Consumption", subtitle = "Violin Plot", caption = "Synthetic Data",
#        x = "2016 USD PPP", y = "", fill = "Variable") +
#   SahelGraphR::themeaspdark()
# ggsave(filename = "plotcontinuousviolin.jpg", dpi = 192)
#
# # Violin half
# ggplot(sahel_sim_long_monetary, aes(x = value, y = variable, fill = country_names)) +
#   geom_flat_violin_h(alpha = 0.9, position = position_dodge(1), width = 3) +
#   geom_boxplot(width = 0.1, color = "grey90", alpha = 0.2, position = position_dodge(1)) +
#   scale_fill_manual(c("Brukina Faso", "Niger", "Mauritania", "Senegal"), values = unname(SahelGraphR::asp_palettes$Dark[2:5])) +
#   scale_y_discrete(labels = c("Consumption", "Income")) +
#   scale_x_log10() +
#   labs(title = "Household Income & Household Consumption", subtitle = "Violin Plot", caption = "Synthetic Data",
#        x = "2016 USD PPP", y = "", fill = "Variable") +
#   SahelGraphR::themeaspdark()
# ggsave(filename = "plotcontinuousviolinhalf.jpg", dpi = 192)
#
# ### Count variable ----
# # Ridgeline
# ggplot(sahel_sim_long_count) +
#   geom_density_ridges(aes(x = value, y = variable, fill = country_names), alpha = .5) +
#   scale_fill_manual(c("Brukina Faso", "Niger", "Mauritania", "Senegal"), values = unname(SahelGraphR::asp_palettes$Dark[2:5])) +
#   scale_y_discrete(labels = c("Children", "Animals", "Years of education")) +
#   labs(title = "Number of Kids, Animals, and Years of Education",  subtitle = "Ridgeline Plot", caption = "Synthetic Data",
#        x = "Count", y = "", fill = "Variable") +
#   SahelGraphR::themeaspdark()
# ggsave(filename = "plotdiscreteridgeline.jpg", dpi = 192)
#
# # Boxplot
# ggplot(sahel_sim_long_count, aes(x = value, y = variable, fill = country_names)) +
#   geom_boxplot(alpha = 0.8, color = "grey85") +
#   scale_fill_manual(c("Brukina Faso", "Niger", "Mauritania", "Senegal"), values = unname(SahelGraphR::asp_palettes$Dark[2:5])) +
#   scale_y_discrete(labels = c("Children", "Animals", "Years of education")) +
#   labs(title = "Number of Kids, Animals, and Years of Education",  subtitle = "Ridgeline Plot", caption = "Synthetic Data",
#        x = "Count", y = "", fill = "Variable") +
#   SahelGraphR::themeaspdark()
# ggsave(filename = "plotdiscreteboxplot.jpg", dpi = 192)
#
# # Violin
# ggplot(sahel_sim_long_count, aes(x = value, y = variable, fill = country_names)) +
#   geom_violin(alpha = 0.9) +
#   geom_boxplot(width = 0.1, color = "grey90", alpha = 0.2, position = position_dodge(0.9)) +
#   scale_fill_manual(c("Brukina Faso", "Niger", "Mauritania", "Senegal"), values = unname(SahelGraphR::asp_palettes$Dark[2:5])) +
#   scale_y_discrete(labels = c("Children", "Animals", "Years of education")) +
#   scale_x_log10() +
#   labs(title = "Number of Kids, Animals, and Years of Education",  subtitle = "Ridgeline Plot", caption = "Synthetic Data",
#        x = "Count", y = "", fill = "Variable") +
#   SahelGraphR::themeaspdark()
# ggsave(filename = "plotdiscreteviolin.jpg", dpi = 192)
#
# # Half Violin
# ggplot(sahel_sim_long_count, aes(x = value, y = variable, fill = country_names)) +
#   geom_flat_violin_h(alpha = 0.9, position = position_dodge(1), width = 3) +
#   geom_boxplot(width = 0.1, color = "grey90", alpha = 0.2, position = position_dodge(1)) +
#   scale_fill_manual(c("Brukina Faso", "Niger", "Mauritania", "Senegal"), values = unname(SahelGraphR::asp_palettes$Dark[2:5])) +
#   scale_y_discrete(labels = c("Children", "Animals", "Years of education")) +
#   scale_x_log10() +
#   labs(title = "Number of Kids, Animals, and Years of Education",  subtitle = "Ridgeline Plot", caption = "Synthetic Data",
#        x = "Count", y = "", fill = "Variable") +
#   SahelGraphR::themeaspdark()
# ggsave(filename = "plotdiscreteviolinhalf.jpg", dpi = 192)
#
# ### Binary variable
# # Calculate standard deviation by country and variable
# alpha = 0.05
# df_sd <- sahel_sim_long_binary |>
#   group_by(country_names, variable) |>
#   summarize(
#     mean = mean(value),
#     sd = sd(value),
#     se = sd(value) / sqrt(length(value)),
#     t = qt((1 - alpha) / 2 + .5, length(value) - 1),
#     CI = t * se
#   )
# # # BY Variable
# # # Plot things vertically
# # ggplot(df_sd, aes(x = variable, y = mean, fill = country_names)) +
# #   geom_bar(stat = "identity", position = position_dodge(0.6), color = NA, width = 0.5) +
# #   geom_errorbar(data = df_sd, aes(ymin = mean - CI, ymax = mean + CI), colour = "grey90", alpha = 0.9, linewidth = .5, width = 0.2, position = position_dodge(0.6)) +
# #   scale_fill_manual(labels = c("Brukina Faso", "Niger", "Mauritania", "Senegal"), values = unname(SahelGraphR::asp_palettes$Dark[2:5])) +
# #   scale_x_discrete(labels = c("Female Head", "Has Children", "Owns Animals")) +
# #   labs(title = "Has Children, Animals, Female Head",  subtitle = "Bar Plot (For binary variables)", caption = "Synthetic Data. 95% Confidence Intervals",
# #        x = "", y = "", fill = "Country") +
# #   SahelGraphR::themeaspdark()
# # ggsave(filename = "plotbinaryverticalbarvariable.jpg", dpi = 192)
# #
# # # Plot things Horizontally
# # ggplot(df_sd, aes(x = variable, y = mean, fill = country_names)) +
# #   geom_bar(stat = "identity", position = position_dodge(0.6), color = NA, width = 0.5) +
# #   geom_errorbar(data = df_sd, aes(ymin = mean - CI, ymax = mean + CI), colour = "grey90", alpha = 0.9, linewidth = .5, width = 0.2, position = position_dodge(0.6)) +
# #   coord_flip() +
# #   scale_x_discrete(labels = c("Has Children", "Owns Animals", "Female Head")) +
# #   scale_fill_manual(labels = c("Brukina Faso", "Niger", "Mauritania", "Senegal"), values = unname(SahelGraphR::asp_palettes$Dark[2:5])) +
# #   labs(title = "Has Children, Animals, Female Head",  subtitle = "Bar Plot (For binary variables)", caption = "Synthetic Data. 95% Confidence Intervals",
# #        x = "", y = "", fill = "Country") +
# #   SahelGraphR::themeaspdark()
# # ggsave(filename = "plotbinaryhorizontalbarvariable.jpg", dpi = 192)
# geom_hal
# # BY COUNTRY
# # Plot things vertically
# ggplot(df_sd, aes(x = country_names, y = mean, fill = variable)) +
#   geom_bar(stat = "identity", position = position_dodge(0.6), color = NA, width = 0.5) +
#   geom_errorbar(data = df_sd, aes(ymin = mean - CI, ymax = mean + CI), colour = "grey90", alpha = 0.9, linewidth = .5, width = 0.2, position = position_dodge(0.6)) +
#   scale_fill_manual(labels = c("Has Children", "Owns Animals", "Female Head"), values = unname(SahelGraphR::asp_palettes$Dark[3:5])) +
#   labs(title = "Has Children, Animals, Female Head",  subtitle = "Bar Plot (For binary variables)", caption = "Synthetic Data. 95% Confidence Intervals",
#        x = "", y = "", fill = "Variable") +
#   SahelGraphR::themeaspdark()
# ggsave(filename = "plotbinaryverticalbarcountry.jpg", dpi = 192)
#
# # Plot things Horizontally
# ggplot(df_sd, aes(x = country_names, y = mean, fill = variable)) +
#   geom_bar(stat = "identity", position = position_dodge(0.6), color = NA, width = 0.5) +
#   geom_errorbar(data = df_sd, aes(ymin = mean - CI, ymax = mean + CI), colour = "grey90", alpha = 0.9, linewidth = .5, width = 0.2, position = position_dodge(0.6)) +
#   coord_flip() +
#   scale_x_discrete(labels = c("Brukina Faso", "Niger", "Mauritania", "Senegal")) +
#   scale_fill_manual(labels = c("Has Children", "Owns Animals", "Female Head"), values = unname(SahelGraphR::asp_palettes$Dark[3:5])) +
#   labs(title = "Has Children, Animals, Female Head",  subtitle = "Bar Plot (For binary variables)", caption = "Synthetic Data. 95% Confidence Intervals",
#        x = "", y = "", fill = "Country") +
#   SahelGraphR::themeaspdark()
# ggsave(filename = "plotbinaryhorizontalbarcountry.jpg", dpi = 192)
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
# sahel_sim_long_monetary <- sahel_sim_long_monetary
#
# ggplot(sahel_sim, aes(x = country_names, y = hh_income, fill = country_names)) +
#   geom_flat_violin(alpha = 0.9) +
#   geom_boxplot(width = 0.1, color = "grey90", alpha = 0.2) +
#   scale_fill_manual(c("Brukina Faso", "Niger", "Mauritania", "Senegal"), values = unname(SahelGraphR::asp_palettes$Dark[2:5])) +
#   scale_y_log10() +
#   labs(title = "Household Income & Household Consumption", subtitle = "Violin Plot", caption = "Synthetic Data",
#        x = "2016 USD PPP", y = "", fill = "Variable") +
#   SahelGraphR::themeaspdark()
#
#
# ggplot(sahel_sim, aes(x = hh_income, y = country_names)) +
#   geom_flat_violin_h(alpha = 0.9, scale = "area") +
#   # geom_boxplot(width = 0.1, color = "grey90", alpha = 0.2) +
#   scale_x_log10() +
#   labs(title = "Household Income & Household Consumption", subtitle = "Violin Plot", caption = "Synthetic Data",
#        x = "2016 USD PPP", y = "", fill = "Variable") +
#   SahelGraphR::themeaspdark()
