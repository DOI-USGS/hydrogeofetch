rescale_characteristics <- function(vars, lookup_table) {
  # assign columns based on the desired summary operation
  cols_sum <- vars$characteristic_id[vars$summary_statistic == "sum"]
  cols_area_wtd_mean <- vars$characteristic_id[vars$summary_statistic == "area_weighted_mean"]
  cols_length_wtd_mean <- vars$characteristic_id[vars$summary_statistic == "length_weighted_mean"]
  cols_min <- vars$characteristic_id[vars$summary_statistic == "min"]
  cols_max <- vars$characteristic_id[vars$summary_statistic == "max"]

  # adjust certain column names to use rescaled values for split catchments (if applicable)
  if(!all(lookup_table$comid == lookup_table$member_comid)){
    cols_area_wtd_mean <- paste0(cols_area_wtd_mean, "_rescaled")
    cols_sum <- paste0(cols_sum, "_rescaled")
  }

  # adjust column names to include percent_nodata columns
  vars_nodata <- names(select(lookup_table, starts_with("percent_nodata")))
  vars_nodata <- grep("[0-9]+$", vars_nodata, value = TRUE, invert = TRUE)
  if(length(vars_nodata) > 0) {
    cols_area_wtd_mean <- c(cols_area_wtd_mean, vars_nodata)
  }

  # rescale NHDPlusV2 attributes to desired catchments
  # !note that there are currently no adjustments made for the length of split flowlines
  lookup_table |>
    group_by(.data$id) |>
    summarize(
      areasqkm_sum = sum(.data$split_catchment_areasqkm),
      lengthkm_sum = sum(.data$lengthkm),
      across(any_of(cols_area_wtd_mean), \(x) weighted.mean(x, w = .data$split_catchment_areasqkm, na.rm = TRUE), .names = "{col}_area_wtd"),
      across(any_of(cols_length_wtd_mean), \(x) weighted.mean(x, w = .data$lengthkm, na.rm = TRUE), .names = "{col}_length_wtd"),
      across(any_of(cols_sum), \(x) sum(x, na.rm = TRUE), .names = "{col}_sum"),
      across(any_of(cols_min), \(x) min(x, na.rm = TRUE), .names = "{col}_min"),
      across(any_of(cols_max), \(x) max(x, na.rm = TRUE), .names = "{col}_max")
    ) |>
    ungroup() |>
    rename_with(\(x) gsub("_rescaled", "", x), contains("_rescaled"))
}

#' @description
#' Get catchment area and flowline length for NHDPlusV2 COMID(s) of interest.
#' If any COMID represents a split catchment, the split catchment areas are
#' returned, along with a column that represents the proportion of the original
#' NHDPlusV2 catchment area that is covered by the split catchment.
#'
#' @param comids integer vector or character vector containing NHDPlusV2 identifiers
#' @param refactored_areas data frame containing columns "featureid" and
#' "areasqkm." Used to retrieve adjusted catchment areas in the case of split
#' catchments.
#'
#' @importFrom dplyr mutate select right_join left_join filter rename bind_rows all_of
#' @importFrom stats weighted.mean
#' @noRd
#'
get_catchment_areas <- function(comids, refactored_areas = NULL){

  # format comids (omit suffix "." in the case of split catchments)
  comids_fmt <- data.frame(member_comid = comids) |>
    mutate(comid = as.integer(.data$member_comid))

  # fetch basin area for all comids
  catchment_areas <- hydrogeofetch::get_vaa(atts = c("comid", "areasqkm", "lengthkm")) |>
    select(all_of(c("comid", "areasqkm", "lengthkm"))) |>
    right_join(comids_fmt, by = "comid", multiple = "all")

  # handle "split" catchments (if applicable)
  if(all(comids_fmt$member_comid == as.character(comids_fmt$comid))) {

    return(mutate(catchment_areas, comid = as.integer(.data$comid)) |>
             rename(split_catchment_areasqkm = "areasqkm"))

  } else {


    if(is.null(refactored_areas)) {
      stop("refactored_areas required when any catchments are split any(member_comid != comid)")
    }

    split_areas <- left_join(
      # create data frame containing the "split" catchments
      filter(catchment_areas, grepl(".", .data$member_comid, fixed = TRUE)),
      # get the catchment area for the split catchments
      refactored_areas |>
        filter(.data$featureid %in% catchment_areas$member_comid) |>
        rename(split_catchment_areasqkm = "areasqkm"),
      by = c("member_comid" = "featureid"))

    # create data frame containing the unsplit catchments
    unsplit_areas <- catchment_areas |>
      filter(!grepl(".", .data$member_comid, fixed = TRUE)) |>
      mutate(split_catchment_areasqkm = .data$areasqkm)

    # bind split and unsplit catchments back together
    return(bind_rows(unsplit_areas, split_areas) |>
             mutate(comid = as.integer(.data$comid),
                    split_area_prop = .data$split_catchment_areasqkm/.data$areasqkm))
  }

}

#' Rescale Catchment Characteristics
#' @description Given catchment characteristics to retrieve or process will
#' aggregate and / or split the characteristics according to a lookup table.
#'
#' @details
#' NOTE: Since this algorithm works on catchment characteristics that are
#' spatial averages, when splitting, the average condition is apportioned evenly
#' to each split. In some cases, such as with land cover or elevation, this may
#' not be appropriate and source data should be used to derive new characteristics.
#' In addition, this function handles catchment areas for split catchments but
#' makes no adjustments for the length of flowlines in those catchments.
#' Therefore, requests for length-weighted mean values may not be appropriate
#' when working with split catchments.
#'
#' @param vars data.frame containing `characteristic_id` retrieved from
#' \link{get_characteristics_metadata} and `summary_statistic` indicating
#' which summary statistic should be applied to rescale each characteristic.
#' Accepted values are "sum," "length_weighted_mean," "area_weighted_mean,"
#' "min," and "max."
#'
#' @param lookup_table data.frame containing `id` numeric vector of identifiers
#' at the desired scale; "comid" is a numeric vector of NHDPlusV2 identifiers;
#' "member_comid" contains formatted NHDPlusV2 COMIDs indicating that the
#' catchments in question need to be split. If catchments have not been split,
#' the columns "comid" and "member_comid" should be identical.
#'
#' @param refactored_areas data.frame containing columns "featureid" and "areasqkm."
#' Used to retrieve adjusted catchment areas in the case of split catchments. If
#' not provided, either no split catchments can be considered or the `catchment_areas`
#' parameter is required.
#'
#' @param catchment_characteristics data.frame containing columns
#' "characteristic_id", "comid", "characteristic_value", and "percent_nodata".
#' If not provided, it will be retrieved from \link{get_catchment_characteristics}
#' using the characteristic ids from `vars` and the comids from `lookup_table`.
#'
#' @param catchment_areas data.frame containing columns "comid", "areasqkm",
#' "split_catchment_areasqkm", and "split_area_prop". If not provided, it will
#' be retrieved from `refactored_areas` and/or \link{get_vaa}.
#'
#' @return data.frame with one row per unique `id` in `lookup_table`. Always
#' contains `id`, `areasqkm_sum` (total catchment area contributing to that
#' `id`), and `lengthkm_sum` (total flowline length). Each characteristic named
#' in `vars` adds a column called `<characteristic_id>_<statistic>`, where the
#' statistic suffix is one of "area_wtd", "length_wtd", "sum", "min", or "max"
#' following the `summary_statistic` requested for it -- for example
#' `CAT_IMPV11_area_wtd` or `CAT_BASIN_AREA_sum`. Where the source reports a
#' no-data fraction, a matching `percent_nodata_<characteristic_id>_area_wtd`
#' column is included. Returns `NULL` if `catchment_characteristics` was not
#' supplied and could not be retrieved from the web service.
#'
#' @examples
#' \donttest{
#' vars <- data.frame(characteristic_id = c("CAT_IMPV11","CAT_BASIN_AREA"),
#'                    summary_statistic = c("area_weighted_mean","sum"))
#' lookup_table <- data.frame(id = rep(10012268, 2),
#'                            comid = c(4146596, 4147382),
#'                            member_comid = c(4146596, 4147382))
#' rescale_catchment_characteristics(vars, lookup_table)
#'
#' vars <- data.frame(characteristic_id = c("CAT_ELEV_MIN","CAT_ELEV_MAX"),
#'                    summary_statistic = c("min","max"))
#' lookup_table <- data.frame(id = rep(10012268, 2),
#'                            comid = c(4146596, 4147382),
#'                            member_comid = c(4146596, 4147382))
#' rescale_catchment_characteristics(vars, lookup_table)
#'
#' vars <- data.frame(characteristic_id = c("CAT_EWT","CAT_TWI", "CAT_BASIN_AREA"),
#'                    summary_statistic = c("area_weighted_mean", "area_weighted_mean","sum"))
#' lookup_table <- data.frame(id = c(10012268, 10012268, 10024047, 10024048),
#'                            comid = c(4146596, 4147382, 4147396, 4147396),
#'                            member_comid = c("4146596", "4147382", "4147396.1", "4147396.2"))
#' comid_areas <- data.frame(featureid = c("4146596", "4147382", "4147396.1", "4147396.2"),
#'                                areasqkm = c(0.9558, 11.9790, 6.513294, 1.439999))
#' rescale_catchment_characteristics(vars, lookup_table, refactored_areas = comid_areas)
#'
#'  }
#'
#' @param source character \code{"usgs"} (default) or \code{"streamcat"}. Passed
#' through to \link{get_catchment_characteristics} when \code{catchment_characteristics}
#' is not provided.
#' @param aoi character area of interest, passed through to
#' \link{get_catchment_characteristics} when \code{catchment_characteristics}
#' is not provided. See \link{get_catchment_characteristics} for details.
#' @importFrom dplyr left_join rename_with mutate across group_by summarize ungroup distinct starts_with any_of contains
#' @importFrom data.table as.data.table dcast
#' @export
#'
rescale_catchment_characteristics <- function(vars, lookup_table,
                                              refactored_areas = NULL,
                                              catchment_characteristics = NULL,
                                              catchment_areas = NULL,
                                              source = "usgs",
                                              aoi = "cat"){

  # check that the inputs match what we are expecting
  if(!all(c("characteristic_id", "summary_statistic") %in% names(vars))){
    stop("Check that vars contains columns 'characteristic_id' and 'summary_statistic'")
  }

  summary_stat_acceptable <- vars$summary_statistic %in% c("min", "max", "sum",
                                                           "length_weighted_mean",
                                                           "area_weighted_mean")

  if(!all(summary_stat_acceptable)){
    stop("Check that all entries in vars$summary_statistic match accepted values")
  }

  if(!all(c("comid", "member_comid", "id") %in% names(lookup_table))){
    stop("Check that lookup_table contains columns 'comid,' 'member_comid,' and 'id'")
  }

  # omit any duplicated rows in the lookup table
  lookup_table <- distinct(lookup_table)

  if(is.null(catchment_characteristics)) {
    # download characteristics for the requested comids
    catchment_characteristics <- get_catchment_characteristics(varname = vars$characteristic_id,
                                              ids = unique(lookup_table$comid),
                                              source = source,
                                              aoi = aoi)

    if(is.null(catchment_characteristics)) {
      return(NULL)
    }

  }

  var_names <- unique(catchment_characteristics$characteristic_id)

  # pivot to wide format
  catchment_characteristics <- as.data.frame(dcast(
    as.data.table(catchment_characteristics),
    comid ~ characteristic_id,
    value.var = c("characteristic_value", "percent_nodata")))
  catchment_characteristics <- rename_with(.data = catchment_characteristics,
                                           .fn = \(x) gsub("characteristic_value_", "", x, fixed = TRUE))

  if(is.null(catchment_areas)) {
    # get comid catchment areas, adjusting area for catchments that have been "split"
    catchment_areas <- get_catchment_areas(unique(lookup_table$member_comid), refactored_areas)
  }

  # combine the nldi characteristics with the catchment identifier and basin area
  lookup_table <- lookup_table |>
    left_join(catchment_areas, by = c("member_comid","comid")) |>
    left_join(catchment_characteristics, by = "comid", multiple = "all")

  # rescale the nldi characteristics if needed (i.e., for split catchments)
  if(!all(lookup_table$comid == lookup_table$member_comid)){
    lookup_table <- mutate(lookup_table,
                           across(any_of(var_names), \(x) x * .data$split_area_prop, .names = "{col}_rescaled"))
  }

  return(rescale_characteristics(vars, lookup_table))
}


