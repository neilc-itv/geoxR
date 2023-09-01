runtest <- function(){
  ga_id <- 115145481

  spots <- baRb::barb_get_spots('2022-12-01', '2023-01-15', "dogmates") |>
    dplyr::filter(sales_house_name != "ITV Breakfast")

  test_region <- c("London")

  control_region = c(
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

  # ga <- readr::read_rds('ga.rds')

  ga <- get_ga_all(ga_id, '2022-12-01', '2023-01-15')

  ga <- readr::read_rds('/Users/neilchar/dev/GeoX/katie_loxton/ga.rds')

  adjustment_factor <- get_adjustment_factor(ga$granular$results_minute, spots, test_region, control_region)

  test <- calculate_uplifts(ga$aggregated$results_total,
                            test_region = test_region,
                            control_region = control_region,
                            pre_start = '2023-05-01',
                            pre_end = '2023-06-04',
                            test_start = '2023-06-05',
                            adjust_for_bleed_into_control = FALSE,
                            NA
                            )

  test <- purrr::map(
    ga$aggregated,
    .f = ~
      calculate_uplifts(.,
                            test_region = test_region,
                            control_region = control_region,
                        pre_start = '2023-05-01',
                        pre_end = '2023-06-04',
                        test_start = '2023-06-05',
                        adjust_for_bleed_into_control = FALSE,
                        NA
      ))




  readr::write_rds(ga, 'ga.rds')


  meta <- googleAnalyticsR::ga_meta("data")

  ga <- googleAnalyticsR::ga_data(
    251419818,
    metrics = "sessions",
    dimensions = c("date", "city"),
    date_range = c("2023-05-01", "2023-05-31"),
    dim_filters = googleAnalyticsR::ga_data_filter(country %contains% "United Kingdom"),
    limit = -1
  )
}
