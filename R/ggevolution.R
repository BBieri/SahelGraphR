# prodregs_lst <- list()
# for (i in c('2018', '2020', '2021', '2022')) {
#   print(paste0("Adding: ", i))
#   prodregs_lst[[i]] <- fabricatr::fabricate(N = 8,
#                                    var_name = c("consum_2_day_eq_ppp",
#                                                 "consum_2_day_ppp",
#                                                 "food_2_day_eq_ppp",
#                                                 "food_2_day_ppp",
#                                                 "food_2_day_g_ppp",
#                                                 "food_2_day_g_eq_ppp",
#                                                 "food_2_g_d",
#                                                 "FIES_rvrs_raw"),
#                                    b0 = 0,
#                                    b1 = rnorm(N, 0, 1),
#                                    b2 = rnorm(N, 0, 1),
#                                    b3 = rnorm(N, 0, 1),
#                                    avg0 = rnorm(N, 7, 1),
#                                    avg1 = rnorm(N, 8, 1),
#                                    avg2 = rnorm(N, 9, 1),
#                                    avg3 = rnorm(N, 10, 1),
#                                    se0 = rnorm(N, 0, 1),
#                                    se1 = rnorm(N, 0, 1),
#                                    se2 = rnorm(N, 0, 1),
#                                    se3 = rnorm(N, 0, 1),
#                                    ci95_0 = rnorm(N, 2, 1),
#                                    ci95_1 = rnorm(N, 2, 1),
#                                    ci95_2 = rnorm(N, 2, 1),
#                                    ci95_3 = rnorm(N, 2, 1),
#                                    p0 = rnorm(N, 0.8, 0.5),
#                                    p1 = rnorm(N, 0.8, 0.5),
#                                    p2 = rnorm(N, 0.8, 0.5),
#                                    p3 = rnorm(N, 0.8, 0.5),
#                                    mht_family = 1) |>
#     dplyr::select(-ID)
# }
#
# prodregs_avg <- lapply(prodregs_lst, SahelGraphR::sahel_prep_ggcoefs)
#
# prodregs_avg <- dplyr::bind_rows(prodregs_avg, .id = "Year") |>
#   dplyr::mutate(Year = as.numeric(Year)) |>
#   dplyr::filter(var_name == "Daily Cons.\n Ad. Equiv.")
#
# # Line Graph of treatment mean evolution
# g1 <- ggplot2::ggplot(prodregs_avg, ggplot2::aes(x = Year, y = Value.y,
#                              group = Treatment, fill = Treatment,
#                              color = Treatment)) +
#   ggplot2::geom_line(linewidth = 0.8) +
#   ggplot2::geom_point(size = 3) +
#   ggplot2::labs(title = "Evolution of Means over Survey Rounds",
#                 subtitle = "Simulated Daily Cons. Ad. Equiv.",
#                 caption = "This graph can easily be filtered to include only one or two treatment arms.",
#                 y = "Daily Cons. Ad. Equiv. (2016, PPP)") +
#   ggplot2::scale_color_manual(values = unname(SahelGraphR::asp_palettes$Dark[2:5])) +
#   SahelGraphR::themeaspdark()
#
# ggplot2::ggsave("C:/Users/bjorn/Desktop/Rplot.png", dpi = 196)
#
# # Column plot of treatment effects (NOT DIFFERENCES IN AVGs)
# prodregs_b <- lapply(prodregs_lst, SahelGraphR::sahel_prep_ggcoefs, measure = "b")
# prodregs_b <- dplyr::bind_rows(prodregs_b, .id = "Year") |>
#   dplyr::mutate(Year = as.numeric(Year)) |>
#   dplyr::filter(var_name == "Daily Cons.\n Ad. Equiv.")
#
# g2 <- ggplot2::ggplot(prodregs_b, ggplot2::aes(x = Year, y = Value.y,
#                                        group = Treatment, fill = Treatment,
#                                        color = Treatment)) +
#   ggplot2::geom_col(position = ggplot2::position_dodge(0.9),
#                     color = NA,
#                     width = 0.8) + #0.8
#   ggplot2::geom_errorbar(data = prodregs_b,
#                          ggplot2::aes(ymin = (Value.y - 0.2*Value.x), # Value.y - Value.x
#                                       ymax = (Value.y + 0.2*Value.x)), # Value.y + Value.x
#                          position =  ggplot2::position_dodge(0.9), # ggplot2::position_dodge(0.9)
#                          color = "grey75", #"grey75"
#                          width = 0.2) + # 0.2
#   ggplot2::geom_hline(ggplot2::aes(yintercept = 0), color = "white", linewidth = 0.7) +
#   ggplot2::labs(title = "Evolution of Treatment Effects over Survey Rounds",
#                 subtitle = "Simulated Daily Cons. Ad. Equiv.",
#                 caption = "This graph can easily be filtered to include only one or two treatment arms. Note confidence intervals are not accurate here.",
#                 y = "Beta") +
#   ggplot2::scale_fill_manual(values = unname(SahelGraphR::asp_palettes$Dark[2:5])) +
#   SahelGraphR::themeaspdark()
#
# ggplot2::ggsave("C:/Users/bjorn/Desktop/Rplot02.png", dpi = 196)
#
# # Combine both in one graph
# g3 <- ggplot2::ggplot(prodregs_b, ggplot2::aes(x = Year, y = Value.y,
#                                          group = Treatment, fill = Treatment,
#                                          color = Treatment)) +
#   ggplot2::geom_col(position = ggplot2::position_dodge(0.9),
#                     color = NA,
#                     width = 0.8) + #0.8
#   ggplot2::geom_errorbar(data = prodregs_b,
#                          ggplot2::aes(ymin = (Value.y - 0.2*Value.x), # Value.y - Value.x
#                                       ymax = (Value.y + 0.2*Value.x)), # Value.y + Value.x
#                          position =  ggplot2::position_dodge(0.9), # ggplot2::position_dodge(0.9)
#                          color = "grey75", #"grey75"
#                          width = 0.2) + # 0.2
#   ggplot2::geom_line(data = prodregs_avg, linewidth = 0.8) +
#   ggplot2::geom_point(data = prodregs_avg, size = 3) +
#   ggplot2::geom_hline(ggplot2::aes(yintercept = 0), color = "white", linewidth = 0.7) +
#   ggplot2::labs(title = "Evolution of Means and Treatment Effects over Survey Rounds",
#                 subtitle = "Simulated Daily Cons. Ad. Equiv.",
#                 caption = "This graph can easily be filtered to include only one or two treatment arms. Note confidence intervals are not accurate here.",
#                 y = "Beta/Daily Cons. Ad. Equiv. (2016, PPP)") +
#   ggplot2::scale_fill_manual(values = unname(SahelGraphR::asp_palettes$Dark[2:5])) +
#   ggplot2::scale_color_manual(values = unname(SahelGraphR::asp_palettes$Dark[2:5])) +
#   SahelGraphR::themeaspdark()
#
# ggplot2::ggsave("C:/Users/bjorn/Desktop/Rplot03.png", dpi = 196)
#
#
# # Create six graphs
#
# library(patchwork)
#
# g4 <- ggplot2::ggplot(prodregs_b, ggplot2::aes(x = Year, y = Value.y,
#                                                group = Treatment, fill = Treatment,
#                                                color = Treatment)) +
#   ggplot2::geom_line(data = prodregs_avg, linewidth = 0.8) +
#   ggplot2::geom_point(data = prodregs_avg, size = 3) +
#   ggplot2::geom_col(position = ggplot2::position_dodge(0.9),
#                     color = NA,
#                     width = 0.8) + #0.8
#   ggplot2::geom_errorbar(data = prodregs_b,
#                          ggplot2::aes(ymin = (Value.y - 0.2*Value.x), # Value.y - Value.x
#                                       ymax = (Value.y + 0.2*Value.x)), # Value.y + Value.x
#                          position =  ggplot2::position_dodge(0.9), # ggplot2::position_dodge(0.9)
#                          color = "grey75", #"grey75"
#                          width = 0.2) + # 0.2
#   ggplot2::geom_hline(ggplot2::aes(yintercept = 0), color = "white", linewidth = 0.7) +
#   ggplot2::labs(y = "Beta/Daily Cons. Ad. Equiv. (2016, PPP)") +
#   ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(alpha = 0.6))) +
#   SahelGraphR::themeaspdark()
#
# g4 + g4 + g4 + g4 + g4 + g4 + plot_layout(nrow = 3, byrow = FALSE, guides = 'collect') +
#   plot_annotation(title = "Evolution of Means and Treatment Effects over Survey Rounds",
#                 subtitle = "Simulated Daily Cons. Ad. Equiv.",
#                 caption = "This graph can easily be filtered to include only one or two treatment arms. Note confidence intervals are not accurate here.") &
#   ggplot2::scale_fill_manual(values = unname(SahelGraphR::asp_palettes$Dark[2:5])) &
#   ggplot2::scale_color_manual(values = unname(SahelGraphR::asp_palettes$Dark[2:5])) &
#   SahelGraphR::themeaspdark(legend.key.size = ggplot2::unit(1.5, 'cm'), #change legend key size
#                             legend.key.height = ggplot2::unit(1.5, 'cm'), #change legend key height
#                             legend.key.width = ggplot2::unit(1.5, 'cm'), #change legend key width
#                             legend.title = ggplot2::element_text(size = 20), #change legend title font size
#                             legend.text = ggplot2::element_text(size = 16),
#                             legend.position="top")
#
# ggplot2::ggsave("C:/Users/bjorn/Desktop/Rplot04.png",
#                 width = 20,
#                 height = 20,
#                 units = "in",
#                 dpi = 300)
# # With title as a grob
# wrap_elements(grid::textGrob("Evolution of Means and Treatment Effects\nover Survey Rounds\n----\nSimulated Daily Cons. Ad. Equiv.", gp = grid::gpar(col = "grey85", fontsize = 24))) + g4 + g4 + g4 + g4 + g4 +
#   plot_layout(nrow = 3, byrow = FALSE, guides = "collect") +
#   plot_annotation(caption = "This graph can easily be filtered to include only one or two treatment arms. Note confidence intervals are not accurate here.") &
#   ggplot2::scale_fill_manual(values = unname(SahelGraphR::asp_palettes$Dark[2:5])) &
#   ggplot2::scale_color_manual(values = unname(SahelGraphR::asp_palettes$Dark[2:5])) &
#   SahelGraphR::themeaspdark(legend.key.size = ggplot2::unit(1.5, 'cm'), #change legend key size
#                             legend.key.height = ggplot2::unit(1.5, 'cm'), #change legend key height
#                             legend.key.width = ggplot2::unit(1.5, 'cm'), #change legend key width
#                             legend.title = ggplot2::element_text(size = 20), #change legend title font size
#                             legend.text = ggplot2::element_text(size = 16),
#                             legend.position="top")
#
# ggplot2::ggsave("C:/Users/bjorn/Desktop/Rplot05.png",
#                 width = 20,
#                 height = 20,
#                 units = "in",
#                 dpi = 300)
