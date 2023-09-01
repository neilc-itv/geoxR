test <- function(){
  library(GeoLift)

  all_regions <- c("London",
                   "Westcountry",
                    "West",
                    "Midlands",
                    "Tyne Tees",
                    "Scotland",
                    "Border",
                    "Granada",
                    "Anglia",
                    "Meridian",
                    "Yorkshire",
                    "Wales")

  regional_traffic <- regionalise_ga(ga$aggregated$results_total,
                                     test_region = all_regions,
                                     control_region = "xxx")

  GeoTestData_PreTest <- GeoDataRead(data = regional_traffic,
                                     date_id = "date",
                                     location_id = "region",
                                     Y_id = "sessions",
                                     X = c(), #empty list as we have no covariates
                                     format = "yyyy-mm-dd",
                                     summary = TRUE)

  GeoPlot(GeoTestData_PreTest,
          Y_id = "Y",
          time_id = "time",
          location_id = "location")


  MarketSelections <- GeoLiftMarketSelection(data = GeoTestData_PreTest,
                                             treatment_periods = c(28,42,56),
                                             N = c(2,3,4),
                                             Y_id = "Y",
                                             location_id = "location",
                                             time_id = "time",
                                             effect_size = seq(0, 0.2, 0.05),
                                             lookback_window = 1,
                                             # include_markets = c("chicago"),
                                             # exclude_markets = c("honolulu"),
                                             holdout = c(0.5, 1),
                                             cpic = 100,
                                             budget = 1000000,
                                             alpha = 0.1,
                                             Correlations = TRUE,
                                             fixed_effects = TRUE,
                                             side_of_test = "two_sided")

  qplot(MarketSelections, market_ID = 1, print_summary = FALSE)
}
