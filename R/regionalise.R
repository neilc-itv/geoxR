#' Regionalise a Google Analytics dataset with lat long columns
#'
#' @param ga
#' @param test_region
#' @param control_region
#'
#' @return
#' @export
#'
#' @examples
regionalise_ga <- function(ga, test_region, control_region, micro = FALSE){

  if(any(names(ga)=="city")){
    # Data is GA city names

    city_lookup <- city_macro_lookup

    if(micro){
      city_lookup$region = city_lookup$micro
    } else {
      city_lookup$region = city_lookup$macro
    }

    results_regionalised <- ga |>
      dplyr::left_join(dplyr::select(city_lookup, city, region), by = "city") |>
      tidyr::drop_na() |>
      dplyr::select(-city) |>
      dplyr::group_by(across(c(-sessions))) |>
      dplyr::summarise(sessions = sum(sessions, na.rm = TRUE)) |>
      dplyr::ungroup()

  } else {

    # Assume data is lat longs

    if(micro){
      region_shp <- micro_regions
      region_shp$region <- ITV_Micro
    } else {
      region_shp <- macro_regions
      region_shp$region <- ITV.Macro
    }


    results_sf <- ga |>
      sf::st_as_sf(coords = c("longitude", "latitude"),
               crs = 4326, agr = "constant")

    results_regionalised <- results_sf |>
      sf::st_join(macro_regions, sf::st_within) |>
      tidyr::drop_na() |>
      sf::st_drop_geometry() |>
      dplyr::select(-contains("geometry")) |>
      dplyr::group_by(across(c(-sessions))) |>
      dplyr::summarise(sessions = sum(sessions, na.rm = TRUE)) |>
      dplyr::ungroup()

  }

  results_regionalised
}

#' Title
#'
#' @param ga
#' @param test_regions
#' @param exclude_regions
#' @param test_start
#' @param test_end
#' @param benchmark_advertiser
#' @param benchmark_source
#' @param benchmark_comment
#' @param benchmark_schedule
#' @param auto_regionalise
#'
#' @return
#' @export
#'
#' @examples
uplift_ga <- function(ga = NULL,
                      test_regions = NULL,
                      exclude_regions = NULL,
                      test_start = NULL,
                      test_end = NULL,
                      benchmark_advertiser = NA,
                      benchmark_source = NA,
                      benchmark_comment = NA,
                      benchmark_schedule = NA,
                      auto_regionalise = TRUE){

  if(auto_regionalise){
    traffic_regional <- regionalise_ga(ga)
  } else {
    traffic_regional <- ga
  }

  #Check region names for test and exclusion are valid
  if(!any(test_regions %in% traffic_regional$region)){
    stop('You have specified a test region that is not present in the data')
  }

  if(!any(exclude_regions %in% traffic_regional$region)){
    stop('You have named an excluded region that is not present in the data')
  }

  traffic_regional <- traffic_regional |>
    dplyr::filter(!region %in% exclude_regions)

  test_start_n <- as.numeric(as.Date(test_start) - min(as.Date(ga$date))) + 1
  test_end_n <- as.numeric(as.Date(test_end) - min(as.Date(ga$date))) + 1

  traffic_geo <- GeoLift::GeoDataRead(data = traffic_regional,
                                 date_id = "date",
                                 location_id = "region",
                                 Y_id = "sessions",
                                 X = c(), #empty list as we have no covariates
                                 format = "yyyy-mm-dd",
                                 summary = TRUE)

  traffic_geoLift <- GeoLift::GeoLift(Y_id = "Y",
                                 data = traffic_geo,
                                 locations = c(test_regions),
                                 treatment_start_time = test_start_n,
                                 treatment_end_time = test_end_n,
                                 ConfidenceIntervals = TRUE)

  traffic_geoLift$benchmark_advertiser <- benchmark_advertiser
  traffic_geoLift$benchmark_source <- benchmark_source
  traffic_geoLift$benchmark_comment <- benchmark_comment
  traffic_geoLift$benchmark_schedule <- benchmark_schedule

  traffic_geoLift
}

uplift_ga_carryover <- function(ga = NULL,
                                test_regions = NULL,
                                exclude_regions = NULL,
                                campaign_start = NULL,
                                campaign_end = NULL) {

  max_date <- as.Date(max(ga$date))

  during_campaign <- uplift_ga(
    ga = ga,
    test_regions = test_regions,
    exclude_regions = exclude_regions,
    test_start = campaign_start,
    test_end = campaign_end
  )$inference$Perc.Lift

  if(max_date >= as.Date(campaign_end)+14){
    two_weeks_post <- uplift_ga(
      ga = ga,
      test_regions = test_regions,
      exclude_regions = exclude_regions,
      test_start = campaign_start,
      test_end = as.Date(campaign_end) + 14
    )$inference$Perc.Lift
  } else two_weeks_post <- NA

  if(max_date >= as.Date(campaign_end)+28){
    four_weeks_post <- uplift_ga(
      ga = ga,
      test_regions = test_regions,
      exclude_regions = exclude_regions,
      test_start = campaign_start,
      test_end = as.Date(campaign_end) + 28
    )$inference$Perc.Lift
  } else four_weeks_post <- NA

  list(campaign = during_campaign, two_weeks_post = two_weeks_post, four_weeks_post = four_weeks_post)
}

#' Title
#'
#' @param kpi_list
#' @param test_regions
#' @param exclude_regions
#' @param test_start
#' @param test_end
#' @param benchmark_advertiser
#' @param benchmark_comment
#' @param benchmark_schedule
#'
#' @return
#' @export
#'
#' @examples
calculate_geoLifts <- function(kpi_list = NULL,
                               test_regions = NULL,
                               exclude_regions = NULL,
                               test_start = NULL,
                               test_end = NULL,
                               benchmark_advertiser = NA,
                               benchmark_comment = NA,
                               benchmark_schedule = NA){

  geoLifts_out <- purrr::map(
    .x = kpi_list,
    .f = ~ uplift_ga(
      .x,
      test_regions = test_regions,
      exclude_regions = exclude_regions,
      test_start = test_start,
      test_end = test_end,
      benchmark_advertiser = benchmark_advertiser,
      benchmark_source = "source",
      benchmark_comment = benchmark_comment,
      benchmark_schedule = benchmark_schedule
    )
  )

}

compare_demographics_ga <- function(ga = NULL,
                      test_regions = NULL,
                      exclude_regions = NULL){

  regional <- regionalise_ga(ga) |>
    dplyr::filter(!region %in% exclude_regions) |>
    dplyr::mutate(test_control = ifelse(region %in% test_regions, "test", "control"))

    changes <- regional |>
      dplyr::group_by(test_control, pre_post, userAgeBracket) |>
      summarise(sessions = sum(sessions)) |>
      filter(userAgeBracket != "unknown") |>
      dplyr::group_by(pre_post, test_control) |>
      dplyr::mutate(sessions_pct = sessions / sum(sessions)) |>
    dplyr::select(-sessions) |>
    tidyr::pivot_wider(names_from = pre_post, values_from = sessions_pct) |>
    dplyr::mutate(change = post / pre - 1) |>
    dplyr::filter(test_control=="test")

  changes
}
