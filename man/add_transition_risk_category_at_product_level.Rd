% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_transition_risk_category_at_product_level.R
\name{add_transition_risk_category_at_product_level}
\alias{add_transition_risk_category_at_product_level}
\title{Adds transition risk categories for transition risk scores}
\usage{
add_transition_risk_category_at_product_level(data)
}
\arguments{
\item{data}{A dataframe}
}
\value{
A dataframe.
}
\description{
Adds transition risk categories for transition risk scores
}
\examples{
library(tiltToyData)
library(readr)
library(dplyr)
options(readr.show_col_types = FALSE)

co2 <- read_csv(toy_emissions_profile_products_ecoinvent())
all_activities_scenario_sectors <- read_csv(toy_all_activities_scenario_sectors())
scenarios <- read_csv(toy_sector_profile_any_scenarios())

transition_risk_thresholds <- add_thresholds_transition_risk(
  co2,
  all_activities_scenario_sectors,
  scenarios
)

output <- add_transition_risk_category_at_product_level(transition_risk_thresholds)
output
}
\keyword{internal}
