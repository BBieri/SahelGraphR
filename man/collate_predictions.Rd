% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/collate_models.R
\name{collate_predictions}
\alias{collate_predictions}
\title{Collate predicted values of a list of models into a large data frame.}
\usage{
collate_predictions(models, by = NULL)
}
\arguments{
\item{models}{List of regression models.}

\item{by}{String of the variable name to take the mean prediction by. \code{NULL}
by default.}
}
\value{
A tibble containing the collated predictions grouped by treatment or
not.
}
\description{
Collate predicted values of a list of models into a large data frame.
}
\examples{
# Generate an example dataset
swiss2 <- dplyr::bind_cols(swiss,
                          treatment = rbinom(seq(nrow(swiss)),1, 0.5)) |>
  dplyr::mutate(Education = Education + 2.5 * treatment)
reg1 <- lm(formula = Education ~ treatment, swiss2)
reg2 <- lm(formula = Education ~ treatment + Catholic, swiss2)
reg3 <- lm(formula = Education ~ treatment * Catholic, swiss2)
models <- list(reg1,
               reg2,
               reg3)
# Full predictions:
collate_predictions(models = models)
# Grouped by model and treatment status:
collate_predictions(models = models, by = "treatment")
}
