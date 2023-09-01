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
regionalise_ga <- function(ga, test_region, control_region){

  if(any(names(ga)=="city")){
    # Data is GA city names

    results_regionalised <- ga |>
      dplyr::left_join(dplyr::select(city_macro_lookup, city, macro), by = "city") |>
      tidyr::drop_na() |>
      dplyr::select(-city) |>
      dplyr::group_by(across(c(-sessions))) |>
      dplyr::summarise(sessions = sum(sessions, na.rm = TRUE)) |>
      dplyr::filter(macro %in% test_region |
                      macro %in% control_region) |>
      dplyr::rename(region = macro) |>
      dplyr::ungroup()

  } else {

    # Assume data is lat longs
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
      dplyr::filter(ITV.Macro %in% test_region |
               ITV.Macro %in% control_region) |>
      dplyr::rename(region = ITV.Macro) |>
      dplyr::ungroup()

  }

  results_regionalised
}

#' Roll up a GA lat-long dataset into test and control areas
#'
#' @param ga_regionalised
#' @param test_region
#' @param control_region
#' @param pre_start
#' @param pre_end
#' @param test_start
#'
#' @return
#' @export
#'
#' @examples
assign_test_control <- function(ga_regionalised,
                                test_region = NULL,
                                control_region = NULL,
                                pre_start = NULL,
                                pre_end = NULL,
                                test_start = NULL){

  # Creates pre and post column and replaces region with test/control

  results_regionalised_summary_sf <- ga_regionalised |>
    dplyr::mutate(pre_test = ifelse(date >= pre_start & date <= pre_end, "pre_period",
                             ifelse(date >= test_start, "test_period", ""))) |>
    dplyr::filter(pre_test != "") |>
    dplyr::mutate(region = ifelse(region %in% test_region, "test",
                           ifelse(region %in% control_region, "control", ""))) |>
    dplyr::filter(region != "") |>
    dplyr::group_by(dplyr::across(c(-sessions))) |>
    dplyr::summarise(sessions = sum(sessions, na.rm = TRUE))
}


rebase_control <- function(ga_test_control){

  # Add a date 'all' column if none exists
  if(is.null(ga_test_control$date)) ga_test_control$date <- 'all'

  # Rebases the control region to match test region size
  ga_base_levels <- ga_test_control |>
    dplyr::group_by(dplyr::across(c(-date, -sessions))) |>
    dplyr::summarise(sessions = sum(sessions, na.rm = TRUE)) |>
    dplyr::filter(pre_test=='pre_period') |>
    tidyr::pivot_wider(names_from = region, values_from = sessions) |>
    dplyr::mutate(adj_factor = test / control) |>
    dplyr::select(-pre_test, -pre_test)

  ga_test_control_rebased <- ga_test_control |>
    dplyr::left_join(ga_base_levels, by = "split") |>
    dplyr::mutate(sessions_rebased = ifelse(region=="control", sessions * adj_factor,
                                     sessions))

  ga_test_control_rebased
}

#' Calculate an uplifts table from GA lat-long data
#'
#' @param ga
#' @param test_region
#' @param control_region
#' @param pre_start
#' @param pre_end
#' @param test_start
#'
#' @return
#' @export
#'
#' @examples
calc_ga_uplifts <- function(ga,
                            test_region = test_region,
                            control_region = control_region,
                            pre_start = pre_start,
                            pre_end = pre_end,
                            test_start = test_start) {

  ga |>
    regionalise_ga(test_region, control_region) |>
    assign_test_control(
      test_region = test_region,
      control_region = control_region,
      pre_start = pre_start,
      pre_end = pre_end,
      test_start = test_start
    ) |>
    rebase_control() |>
    dplyr::select(-sessions, -control, -test, -adj_factor) |>
    tidyr::pivot_wider(names_from = region,
                values_from = sessions_rebased)

}

