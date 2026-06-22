#' @title Estimate drainage area from HUC and catchment data
#' @description Combines HUC12 areas upstream of HUC outlets with NHDPlusV2
#'   catchment areas for the portion of the basin between the outlet and HUC12
#'   outlets to produce a drainage area estimate. Non-contributing areas
#'   captured in HUC12 boundaries are included.
#' @details
#'   By default, network navigation is performed via the NLDI web service and
#'   flowline attributes are retrieved from the NHDPlusV2 OGC API. When
#'   \code{local_navigation = TRUE}, the NHDPlusV2 Value Added Attributes
#'   (\code{\link{get_vaa}}) are used for network navigation instead, and
#'   only HUC12 pour points are fetched from the NLDI.
#'
#'   HUC drainage area is used upstream of the nearest HUC12 outlet. Between
#'   the outlet (e.g. gage) and the HUC outlet(s), catchment areas are used.
#'   For large upstream areas the largest HUC level is used to define
#'   connectedness, but drainage estimates are derived from HUC12 because
#'   that is where the non-contributing area attribute lives.
#'
#'   Three pairs of drainage area estimates are returned: one using only
#'   NLDI-identified HUC12s (HUC12-level), one using HUC10-level queries,
#'   and one using HUC08-level queries for basins spanning multiple HUC08s.
#'   Each pair includes a total and a contributing-only estimate derived from
#'   the \code{ncontrb_a} (non-contributing acres) attribute on HUC12 features.
#'
#' @param start list with \code{featureSource} and \code{featureID} compatible
#'   with \code{\link{get_nldi_feature}}. May also include optional
#'   \code{reachcode} and \code{measure} fields; when present these override the
#'   NLDI-supplied values used to position the outlet-catchment split point,
#'   without affecting upstream network navigation. NLDI can be bypassed
#'   entirely by passing only \code{reachcode} and \code{measure} (no
#'   \code{featureSource}/\code{featureID}); in that case the start COMID is
#'   resolved from the reachcode via the VAA when \code{local_navigation = TRUE},
#'   otherwise via the NHD OGC API, and the start point geometry is computed
#'   from the flowline at the supplied measure.
#' @param catchments logical. If TRUE, fetch and return NHDPlusV2 catchment
#'   polygons for the full upstream network. Default FALSE.
#' @param nhdplushr logical. If TRUE (the default), compute a drainage area
#'   estimate from NHDPlusHR catchments. Set to FALSE to skip this step, which
#'   avoids the HR web service calls and speeds up computation.
#' @param local_navigation logical. If TRUE, use \code{\link{get_vaa}} for
#'   network navigation and flowline attributes instead of NLDI/OGC API web
#'   services. Only HUC12 pour points are fetched from the NLDI. Default FALSE.
#' @param huc12_data sf data.frame or NULL. In-memory HUC12 polygon table to
#'   use instead of fetching from web services. Column names are lowercased
#'   internally; must include at minimum \code{huc_12} and \code{ncontrb_a}
#'   (case-insensitive). When provided, all HUC12 polygon queries are resolved
#'   by subsetting this table. Default NULL (use web services).
#' @param huc12_outlets sf data.frame, character path, or NULL. HUC12 pour
#'   points to use instead of the NLDI \code{huc12pp} service. Accepts either
#'   a preloaded sf data.frame or a path to a GPKG, which is read with
#'   \code{\link[sf]{read_sf}}. Columns \code{COMID} and \code{FinalWBD_HUC12}
#'   are renamed to \code{comid} and \code{identifier}; any other columns are
#'   preserved. When provided, the outlets are filtered to the upstream
#'   network COMID set and no NLDI \code{huc12pp} queries are issued. Pair
#'   with \code{local_navigation = TRUE} and \code{huc12_data} to run fully
#'   offline. National CONUS outlets GPKGs are available from Blodgett, D.L.,
#'   2022, Mainstem Rivers of the Conterminous United States (ver. 3.0,
#'   February 2026): U.S. Geological Survey data release,
#'   \doi{10.5066/P13LNDDQ} (layer \code{hu_points}). Default NULL.
#' @param waterbody_data sf data.frame or NULL. In-memory NHDWaterbody polygon
#'   table to use instead of fetching from web services. Column names are
#'   lowercased internally; must include \code{comid} (case-insensitive). When
#'   provided and \code{start} is an \code{sfc_POINT}, the waterbody containing
#'   the point is resolved by spatial filter on this table. Default NULL (use
#'   web services).
#' @param catchment_data sf data.frame or NULL. In-memory NHDPlusV2 CatchmentSP
#'   polygon table. Column names are lowercased internally; must include
#'   \code{featureid} (case-insensitive). Used in three ways: (1) when
#'   \code{catchments = TRUE}, catchment polygons for the upstream network are
#'   subsetted from this table instead of fetched from the OGC API; (2) the
#'   gap-zone catchment polygons (between the outlet and the immediate HUC12
#'   outlets) are subsetted from this table instead of fetched; (3) when
#'   \code{start} is an \code{sfc_POINT} and no waterbody is found, the COMID
#'   is resolved by spatial join on this table. Default NULL (use web services).
#' @param HU_inclusion_override character vector or NULL. Eight- or ten-digit
#'   HUC codes whose HUC12s should be kept even when the parent HUC outlet is
#'   not in the on-network set. Useful for regions like the Prairie Potholes
#'   where landscape-connected HUCs lack a network outlet (e.g.
#'   \code{"10130106"} in South Dakota). Default NULL (no overrides).
#' @param outlet_split_threshold_m numeric. Minimum distance in meters from
#'   the gage to the outlet of its catchment before the catchment is split.
#'   When the gage is at least this far upstream, the outlet catchment is split
#'   at the gage point and only the upstream portion is included. Default 100.
#' @return list with elements:
#'   \describe{
#'     \item{da_huc12_sqkm}{numeric. Total DA using NLDI-identified HUC12s only.}
#'     \item{da_huc10_sqkm}{numeric or NA. Total DA using HUC10-level queries.
#'       NA when basin is within a single HUC10.}
#'     \item{da_huc08_sqkm}{numeric or NA. Total DA using HUC08-level queries.
#'       NA when basin is within a single HUC08.}
#'     \item{contrib_da_huc12_sqkm}{numeric. Contributing DA (HUC12-only).}
#'     \item{contrib_da_huc10_sqkm}{numeric or NA. Contributing DA (HUC10-level).}
#'     \item{contrib_da_huc08_sqkm}{numeric or NA. Contributing DA (HUC08-level).}
#'     \item{network_da_sqkm}{numeric. Network-derived total DA for comparison.}
#'     \item{nhdplushr_network_dasqkm}{numeric or NA. Drainage area from
#'       NHDPlusHR catchments upstream of the matched HR flowline. NA with a
#'       warning when the HR web service is unavailable or fails.}
#'     \item{nhdplushr_boundary}{sfc_GEOMETRY or NULL. Dissolved boundary of
#'       upstream NHDPlusHR catchments. NULL when the HR estimate is unavailable.}
#'     \item{start_feature}{sf data.frame. The resolved NLDI start feature.}
#'     \item{hu12_by_huc12}{sf data.frame. NLDI-identified upstream HUC12 polygons (EPSG:5070).}
#'     \item{hu12_by_huc10}{sf data.frame or NULL. Upstream HUC12 polygons (HUC10 query).
#'       NULL when basin is within a single HUC10.}
#'     \item{hu12_by_huc08}{sf data.frame or NULL. Upstream HUC12 polygons (HUC08 query).
#'       NULL when basin is within a single HUC08.}
#'     \item{extra_catchments}{sf data.frame. Catchments between outlet and HUC12 outlets.}
#'     \item{split_catchment}{sf data.frame. Split catchment at HUC12 outlet(s).}
#'     \item{all_network}{data.frame. Full upstream flowline attributes.}
#'     \item{all_catchments}{sf data.frame or NULL. NHDPlusV2 catchment polygons
#'       for the full upstream network. NULL when \code{catchments = FALSE}.}
#'     \item{outlet_flowline_measure}{numeric or NULL. Flowline measure (0--100)
#'       for the gage on its outlet flowline. NULL for non-gage starts.}
#'     \item{outlet_split_catchment}{sf data.frame or NULL. Split catchment at
#'       the gage point. Contains "catchment" and "splitCatchment" rows with
#'       areas. NULL when no split is needed (gage at outlet or below threshold).}
#'     \item{hu12_outlet}{sf data.frame. HUC12 pour points upstream of outlet.}
#'   }
#'
#' @importFrom sf st_transform st_geometry st_area st_buffer st_union st_as_sfc st_bbox st_centroid st_distance st_sf st_sfc st_crs st_drop_geometry
#' @importFrom units set_units
#' @importFrom dplyr select filter bind_rows
#' @importFrom hydroloom navigate_hydro_network navigate_network_dfs add_toids index_points_to_lines disambiguate_indexes
#' @importFrom dataRetrieval findNLDI
#' @export
#' @examples
#' \donttest{
#' # Black Earth Creek
#' start <- list(featureSource = "nwissite", featureID = "USGS-05406500")
#' result <- get_drainage_area_estimates(start)
#' result$da_huc10_sqkm
#' result$network_da_sqkm
#' }
get_drainage_area_estimates <- function(start, catchments = FALSE,
  nhdplushr = TRUE, local_navigation = FALSE, huc12_data = NULL,
  huc12_outlets = NULL, waterbody_data = NULL, catchment_data = NULL,
  HU_inclusion_override = NULL, outlet_split_threshold_m = 100) {

  if(!is.null(huc12_data))
    huc12_data <- prepare_huc12_data(huc12_data)

  if(!is.null(huc12_outlets))
    huc12_outlets <- prepare_huc12_outlets(huc12_outlets)

  if(!is.null(waterbody_data))
    waterbody_data <- prepare_waterbody_data(waterbody_data)

  if(!is.null(catchment_data))
    catchment_data <- prepare_catchment_data(catchment_data)

  vaa <- if(local_navigation) {
    message("Loading NHDPlusV2 VAA for local navigation...")
    get_vaa()
  }

  # 1. resolve start feature
  start_info <- resolve_start_feature(start, vaa,
    waterbody_data = waterbody_data,
    catchment_data = catchment_data)
  start_feature <- start_info$start_feature
  outlet_comids <- start_info$outlet_comids

  # 1.5 compute outlet flowline measure for gage splitting
  outlet_info <- negotiate_outlet_catchment(start_info, vaa,
    outlet_split_threshold_m)

  # 2. fetch upstream network + HUC12 outlets
  net_info <- fetch_upstream_network(outlet_comids, vaa, huc12_outlets)
  all_net <- net_info$all_net
  huc12_outlets <- net_info$huc12_outlets

  network_da <- max(all_net$totdasqkm)
  message("  ", nrow(all_net), " flowlines, totdasqkm = ",
    round(network_da, 2))
  message("  Found ", nrow(huc12_outlets), " HUC12 outlets")

  huc12_comids <- huc12_outlets$comid

  # 2.5 auto-override for closed-basin point starts: when the point falls in a
  # HUC12 that has no flowline pour point (i.e., not in huc12_outlets), the
  # disconnected-filter would otherwise drop most of the outlet HUC8/HUC10
  # because their max-by-sort outlet HUC12 isn't on-network. Include the start
  # HUC12's parent HUC10 in the override so the whole HUC8/HUC10 is kept.
  if(inherits(start, "sfc")) {
    HU_inclusion_override <- augment_override_for_closed_basin_start(
      start, huc12_outlets$identifier, huc12_data, HU_inclusion_override
    )
  }

  # 3. find immediately-upstream HUC12 outlets (pure logic)
  immediate_hu12_comids <- find_immediate_huc12_outlets(
    all_net, outlet_comids, huc12_comids
  )

  message("  ", length(immediate_hu12_comids),
    " immediately-upstream HUC12 outlets ",
    "(of ", length(huc12_comids), " total)")

  outlet_huc <- huc12_outlets[
    huc12_outlets$comid %in% immediate_hu12_comids,
  ]

  # 4. fetch + assemble HUC12 areas
  hu12_result <- get_upstream_huc12s(huc12_outlets, outlet_huc,
    huc12_data = huc12_data,
    HU_inclusion_override = HU_inclusion_override)

  # 5. compute gap area between outlet and HUC12 outlets
  nav_net <- if(local_navigation) vaa else all_net
  gap <- compute_gap_area(outlet_huc, all_net, nav_net,
    gap_catchments = catchment_data,
    outlet_info = outlet_info)

  # 6. assemble DA estimates
  da_estimates <- assemble_da_estimates(hu12_result, gap$extra_and_local)

  message("  HUC12 DA = ", round(da_estimates$da_huc12_sqkm, 2),
    ", contributing = ", round(da_estimates$contrib_da_huc12_sqkm, 2))
  if(!is.na(da_estimates$da_huc10_sqkm))
    message("  HUC10 DA = ", round(da_estimates$da_huc10_sqkm, 2),
      ", contributing = ", round(da_estimates$contrib_da_huc10_sqkm, 2))
  if(!is.na(da_estimates$da_huc08_sqkm))
    message("  HUC08 DA = ", round(da_estimates$da_huc08_sqkm, 2),
      ", contributing = ", round(da_estimates$contrib_da_huc08_sqkm, 2))
  message("  Network DA = ", round(network_da, 2))

  # 7. NHDPlusHR estimate
  if(nhdplushr) {
    hu12_polys <- if(!is.null(hu12_result$hu12_by_huc08)) {
      hu12_result$hu12_by_huc08
    } else if(!is.null(hu12_result$hu12_by_huc10)) {
      hu12_result$hu12_by_huc10
    } else {
      hu12_result$hu12_by_huc12
    }

    message("Computing NHDPlusHR drainage area estimate...")
    hr_result <- get_nhdplushr_da_estimate(
      start_feature, network_da, hu12_polys
    )

    if(!is.na(hr_result$nhdplushr_network_dasqkm))
      message("  NHDPlusHR DA = ",
        round(hr_result$nhdplushr_network_dasqkm, 2))
  } else {
    hr_result <- list(nhdplushr_network_dasqkm = NA_real_,
      nhdplushr_boundary = NULL)
  }

  # 8. optional full catchment retrieval
  if(catchments) {
    if(!is.null(catchment_data)) {
      message("Subsetting local catchment data...")
      all_catchments <- catchment_data[
        catchment_data$featureid %in% all_net$comid, ]
    } else {
      message("Fetching network catchment geometries...")
      all_catchments <- get_nhdplus(
        comid = all_net$comid, realization = "catchment"
      )
    }
  } else {
    all_catchments <- NULL
  }

  c(
    da_estimates,
    list(
      network_da_sqkm = network_da,
      nhdplushr_network_dasqkm = hr_result$nhdplushr_network_dasqkm,
      nhdplushr_boundary = hr_result$nhdplushr_boundary,
      start_feature = start_feature,
      hu12_by_huc12 = hu12_result$hu12_by_huc12,
      hu12_by_huc10 = hu12_result$hu12_by_huc10,
      hu12_by_huc08 = hu12_result$hu12_by_huc08,
      extra_catchments = gap$extra_catchments,
      split_catchment = gap$split_catchment,
      all_network = all_net,
      all_catchments = all_catchments,
      outlet_flowline_measure = if(!is.null(outlet_info)) outlet_info$flowline_measure else NULL,
      outlet_split_catchment = gap$outlet_split_catchment,
      hu12_outlet = huc12_outlets
    )
  )
}

#' Resolve start feature to COMID(s)
#'
#' Given a start specification (NLDI feature list or sfc geometry for a
#' waterbody), resolves it to an sf start feature and one or more outlet
#' COMIDs. All web service calls for start resolution live here.
#'
#' @param start list or sfc. NLDI feature list with \code{featureSource} and
#'   \code{featureID}, or an \code{sfc_POINT} for waterbody lookup.
#' @param vaa data.frame or NULL. NHDPlusV2 VAA table for local waterbody
#'   outlet finding. When NULL, uses the OGC API instead.
#' @param waterbody_data sf data.frame or NULL. Pre-prepared NHDWaterbody table
#'   (lowercase names). When provided, replaces the \code{get_waterbodies} call.
#' @param catchment_data sf data.frame or NULL. Pre-prepared CatchmentSP table
#'   (lowercase names). Used as fallback COMID lookup when no waterbody is found.
#' @return list with \code{start_feature} (sf data.frame) and
#'   \code{outlet_comids} (integer vector).
#' @noRd
resolve_start_feature <- function(start, vaa = NULL,
  waterbody_data = NULL, catchment_data = NULL) {

  if(inherits(start, "sfc")) {
    if(!is.null(waterbody_data)) {
      message("Looking up waterbody in local data...")
      # Some national NHDWaterbody polygons fail s2 validity checks
      # (duplicate-vertex loops). Disable s2 just for the spatial
      # filter and restore the previous setting on exit.
      old_s2 <- sf::sf_use_s2()
      sf::sf_use_s2(FALSE)
      on.exit(sf::sf_use_s2(old_s2), add = TRUE)
      wb <- sf::st_filter(waterbody_data,
        sf::st_transform(start, sf::st_crs(waterbody_data)))
    } else {
      message("Fetching waterbody...")
      wb <- get_waterbodies(start)
    }

    if(nrow(wb) == 0 && !is.null(catchment_data)) {
      message("No waterbody found; resolving COMID from local catchment data...")
      pt_sf <- sf::st_sf(geometry = sf::st_transform(start,
        sf::st_crs(catchment_data)))
      joined <- sf::st_join(pt_sf, catchment_data)
      comid_val <- as.integer(joined$featureid[1])
      if(is.na(comid_val))
        stop("No catchment found for the provided point in catchment_data.")
      return(list(
        start_feature = sf::st_sf(comid = comid_val,
          geometry = sf::st_sfc(sf::st_point(), crs = sf::st_crs(start))),
        outlet_comids = comid_val
      ))
    }

    if(!nrow(wb) == 1 | !inherits(wb, "sf"))
      stop("point needs to fall in a waterbody!")

    if(!is.null(vaa)) {
      all_wb <- vaa[vaa$wbareacomi == wb$comid, ]
      outlet_comids <- all_wb$comid[!all_wb$tonode %in% all_wb$fromnode]
    } else {
      message("Finding flowlines for waterbody COMID ", wb$comid, "...")
      wb_flowlines <- query_usgs_oafeat(
        AOI = wb,
        type = "nhd",
        filter = paste0("wbareacomi%20=%20", wb$comid),
        properties = c("comid", "tonode", "fromnode"),
        skip_geometry = TRUE
      )

      if(is.null(wb_flowlines) || nrow(wb_flowlines) == 0)
        stop("No flowlines found for waterbody COMID ", wb$comid)

      outlet_comids <- wb_flowlines$comid[
        !wb_flowlines$tonode %in% wb_flowlines$fromnode
      ]
    }

    message("Found ", length(outlet_comids), " outlets for waterbody")
    start_feature <- wb

  } else if(is.list(start) && !is.null(start$reachcode) &&
             !is.null(start$measure) &&
             is.null(start$featureSource) && is.null(start$featureID)) {
    message("Resolving start COMID from reachcode/measure ",
      "(bypassing NLDI)...")
    measure <- as.numeric(start$measure)
    comid <- resolve_comid_from_reachcode(start$reachcode, measure, vaa)
    message("  Resolved COMID = ", comid)

    fl <- get_nhdplus(comid = comid, realization = "flowline")
    if(is.null(fl) || nrow(fl) == 0)
      stop("Could not fetch flowline geometry for COMID ", comid)
    indexes <- data.frame(
      COMID = comid,
      REACHCODE = start$reachcode,
      REACHCODE_measure = measure,
      offset = 0
    )
    start_point <- get_hydro_location(indexes, dplyr::rename(fl, id = "comid"))

    start_feature <- sf::st_sf(
      comid = comid,
      reachcode = start$reachcode,
      measure = measure,
      geometry = sf::st_sfc(start_point, crs = sf::st_crs(fl))
    )
    outlet_comids <- comid

  } else {
    message("Resolving start feature via NLDI...")
    start_feature <- get_nldi_feature(start[c("featureSource", "featureID")])
    outlet_comids <- start_feature$comid

    if(!is.null(start$reachcode))
      start_feature$reachcode <- start$reachcode
    if(!is.null(start$measure))
      start_feature$measure <- as.numeric(start$measure)
  }

  if(length(outlet_comids) == 0 || all(is.na(outlet_comids)))
    stop("No outlet COMIDs found for the provided start location. ",
      "Check that the input resolves to a valid NHDPlus feature.")

  list(start_feature = start_feature, outlet_comids = outlet_comids)
}

#' Resolve a COMID from a reachcode + measure
#'
#' Used by \code{resolve_start_feature} when the caller bypasses NLDI by passing
#' a reachcode/measure directly in \code{start}. A reachcode can contain
#' multiple flowlines; the one whose \code{[frommeas, tomeas]} interval covers
#' \code{measure} is selected. If \code{measure} is outside every interval, the
#' flowline whose interval boundary is closest is chosen.
#'
#' @param reachcode character. NHDPlusV2 reachcode (14-digit string).
#' @param measure numeric. Reach measure 0-100.
#' @param vaa data.frame or NULL. NHDPlusV2 VAA table with \code{comid},
#'   \code{reachcode}, \code{frommeas}, \code{tomeas} columns. When NULL, the
#'   USGS NHD OGC API is queried by reachcode.
#' @return integer COMID.
#' @noRd
resolve_comid_from_reachcode <- function(reachcode, measure, vaa = NULL) {
  measure <- as.numeric(measure)

  if(!is.null(vaa)) {
    rows <- vaa[vaa$reachcode == reachcode,
      c("comid", "frommeas", "tomeas")]
    if(nrow(rows) == 0)
      stop("No flowlines found in VAA for reachcode ", reachcode)
  } else {
    base <- get_water_url()
    out <- get_oafeat(
      base = base, AOI = NULL, type = "nhdflowline_network",
      filter = paste0("reachcode%20=%20%27", reachcode, "%27"),
      properties = c("comid", "frommeas", "tomeas"),
      skip_geometry = TRUE, status = FALSE
    )
    if(is.null(out) || nrow(out) == 0)
      stop("No flowlines found via OGC API for reachcode ", reachcode)
    rows <- data.frame(
      comid = as.integer(out$comid),
      frommeas = as.numeric(out$frommeas),
      tomeas = as.numeric(out$tomeas)
    )
  }

  in_bounds <- measure >= rows$frommeas & measure <= rows$tomeas
  if(any(in_bounds))
    return(as.integer(rows$comid[which(in_bounds)[1]]))

  dist <- pmin(abs(measure - rows$frommeas),
    abs(measure - rows$tomeas))
  as.integer(rows$comid[which.min(dist)])
}

#' Find the HUC12 containing a point
#'
#' Used to detect closed-basin / endorheic point starts whose containing HUC12
#' has no flowline pour point on the NHD network. When \code{huc12_data} is
#' provided, the HUC12 is found by spatial filter; otherwise, \code{get_huc} is
#' used.
#'
#' @param start sfc_POINT. The start geometry.
#' @param huc12_data sf data.frame or NULL. Pre-prepared HUC12 polygon table
#'   (lowercase names, with a \code{huc_12} column) to spatially filter against.
#'   When NULL, falls back to \code{get_huc(AOI = start, type = "huc12")}.
#' @return character. The 12-digit HUC12 identifier, or NULL if not resolvable.
#' @noRd
find_start_huc12 <- function(start, huc12_data = NULL) {
  if(!inherits(start, "sfc")) return(NULL)

  if(!is.null(huc12_data)) {
    old_s2 <- sf::sf_use_s2()
    sf::sf_use_s2(FALSE)
    on.exit(sf::sf_use_s2(old_s2), add = TRUE)
    pt <- sf::st_transform(start, sf::st_crs(huc12_data))
    hit <- sf::st_filter(huc12_data, pt)
    if(nrow(hit) == 0) return(NULL)
    return(as.character(hit$huc_12[1]))
  }

  hu12 <- tryCatch(
    get_huc(AOI = start, type = "huc12"),
    error = function(e) NULL
  )
  if(is.null(hu12) || nrow(hu12) == 0) return(NULL)
  huc_col <- intersect(c("huc12", "huc_12"), names(hu12))[1]
  if(is.na(huc_col)) return(NULL)
  as.character(hu12[[huc_col]][1])
}

#' Augment HU_inclusion_override when a point start sits in a closed-basin HUC12
#'
#' Looks up the HUC12 containing the start point. If that HUC12 is not present
#' in \code{network_huc12_ids} (i.e., NLDI returned no pour point for it on
#' the upstream network), prepends the HUC12's parent HUC10 to
#' \code{HU_inclusion_override}. A 10-digit override cascades to also match
#' the parent HUC08 in \code{filter_disconnected_huc12s} because that function
#' truncates overrides to its \code{parent_nchar}.
#'
#' @param start sfc_POINT. The start geometry.
#' @param network_huc12_ids character. HUC12 identifiers in the on-network set
#'   (typically \code{huc12_outlets$identifier}).
#' @param huc12_data sf data.frame or NULL. Pre-prepared HUC12 table.
#' @param HU_inclusion_override character or NULL. Existing override codes.
#' @return character vector of override codes (unchanged when no augmentation
#'   is warranted).
#' @noRd
augment_override_for_closed_basin_start <- function(start, network_huc12_ids,
  huc12_data = NULL, HU_inclusion_override = NULL) {

  start_huc12 <- find_start_huc12(start, huc12_data)
  if(is.null(start_huc12)) return(HU_inclusion_override)

  if(start_huc12 %in% network_huc12_ids) return(HU_inclusion_override)

  start_huc10 <- substr(start_huc12, 1, 10)
  message("Start point HUC12 ", start_huc12,
    " has no flowline pour point (closed-basin-like); ",
    "adding HUC10 ", start_huc10, " to HU_inclusion_override.")
  unique(c(start_huc10, HU_inclusion_override))
}

#' Compute the flowline measure for a gage in its outlet catchment
#'
#' Given a resolved start feature, computes the position of the gage along its
#' outlet flowline as a 0--100 flowline measure. This measure is needed to
#' decide whether the outlet catchment should be split so that only the portion
#' upstream of the gage contributes to the drainage area estimate.
#'
#' When the start feature lacks \code{measure} or \code{reachcode} fields (e.g.
#' waterbody starts), returns NULL to signal that no outlet split is needed.
#'
#' @param start_info list. Output of \code{resolve_start_feature} with
#'   \code{start_feature} (sf data.frame) and \code{outlet_comids} (integer).
#' @param vaa data.frame or NULL. NHDPlusV2 VAA table (must include
#'   \code{comid}, \code{frommeas}, \code{tomeas}, \code{lengthkm} columns).
#'   When NULL, flowline attributes are fetched from the OGC API.
#' @param outlet_split_threshold_m numeric. Minimum distance (meters) from
#'   gage to flowline outlet to trigger a catchment split. Default 100.
#' @return list with \code{flowline_measure} (numeric 0--100),
#'   \code{gage_point} (\code{sfc_POINT} on the flowline), and
#'   \code{threshold_exceeded} (logical). Returns NULL when the start feature
#'   lacks measure/reachcode or the gage is at the outlet.
#' @noRd
negotiate_outlet_catchment <- function(start_info, vaa = NULL,
  outlet_split_threshold_m = 100) {

  sf <- start_info$start_feature

  if(!all(c("measure", "reachcode") %in% names(sf)))
    return(NULL)

  measure <- as.numeric(sf$measure)
  reachcode <- sf$reachcode
  comid <- as.integer(sf$comid)

  if(is.na(measure) || is.na(reachcode))
    return(NULL)

  # get frommeas/tomeas/lengthkm from VAA or OGC API
  if(!is.null(vaa)) {
    row <- vaa[vaa$comid == comid, ]
    if(nrow(row) == 0)
      stop("COMID ", comid, " not found in VAA")
    frommeas <- row$frommeas
    tomeas <- row$tomeas
    lengthkm <- row$lengthkm
  } else {
    fl_attrs <- get_nhdplus(comid = comid, realization = "flowline",
      skip_geometry = TRUE)
    if(is.null(fl_attrs) || nrow(fl_attrs) == 0)
      stop("Could not fetch flowline attributes for COMID ", comid)
    frommeas <- fl_attrs$frommeas
    tomeas <- fl_attrs$tomeas
    lengthkm <- fl_attrs$lengthkm
  }

  if(!dplyr::between(measure, frommeas, tomeas)) {
    diffs <- abs(c(frommeas, tomeas) - measure)
    flowline_measure <- c(0, 100)[which.min(diffs)]
    message("  Gage measure (", round(measure, 2),
      ") outside flowline bounds [", round(frommeas, 2), ", ",
      round(tomeas, 2), "]; snapped to ", flowline_measure)
  } else {
    flowline_measure <- rescale_measures(measure, frommeas, tomeas)
  }

  if(flowline_measure < 1) {
    message("  Gage is at the outlet of the flowline (measure ",
      round(flowline_measure, 2), "); no outlet split needed")
    return(NULL)
  }

  message("  Outlet catchment flowline measure: ",
    round(flowline_measure, 2))

  # check threshold: distance from gage to outlet = flowline_measure% of length
  dist_to_outlet_m <- (flowline_measure / 100) * lengthkm * 1000
  threshold_exceeded <- dist_to_outlet_m >= outlet_split_threshold_m

  message("  Distance to outlet: ", round(dist_to_outlet_m, 0),
    " m (threshold ", outlet_split_threshold_m, " m",
    if(threshold_exceeded) " => split needed)" else " => no split needed)")

  # compute gage point on the flowline (needs geometry)
  gage_point <- NULL
  if(threshold_exceeded) {
    fl <- get_nhdplus(comid = comid, realization = "flowline")
    if(!is.null(fl) && nrow(fl) > 0) {
      indexes <- data.frame(
        COMID = comid,
        REACHCODE = reachcode,
        REACHCODE_measure = measure,
        offset = 0
      )
      fl <- dplyr::rename(fl, id = "comid")
      gage_point <- get_hydro_location(indexes, fl)
    }
  }

  list(
    flowline_measure = flowline_measure,
    gage_point = gage_point,
    threshold_exceeded = threshold_exceeded
  )
}

#' Fetch upstream network and HUC12 pour points
#'
#' Retrieves the full upstream flowline network and HUC12 pour points for
#' the given outlet COMIDs. Two code paths: when \code{vaa} is provided,
#' uses local VAA for the network and NLDI only for HUC12 pour points;
#' otherwise fetches both from NLDI and the OGC API.
#'
#' @param outlet_comids integer. One or more outlet COMIDs.
#' @param vaa data.frame or NULL. NHDPlusV2 VAA table for local navigation.
#'   When NULL, uses the NLDI + OGC API path.
#' @param huc12_outlets sf data.frame or NULL. Pre-prepared HUC12 pour
#'   points (from \code{prepare_huc12_outlets}). When supplied, the NLDI
#'   \code{huc12pp} query is skipped and these are filtered to the
#'   upstream network COMID set.
#' @return list with \code{all_net} (data.frame with toid column) and
#'   \code{huc12_outlets} (sf data.frame with comid and identifier columns).
#' @noRd
fetch_upstream_network <- function(outlet_comids, vaa = NULL,
  huc12_outlets = NULL) {

  local_outlets <- !is.null(huc12_outlets)

  if(!local_outlets)
    message("Finding HUC12 pour points upstream via NLDI...")

  if(!is.null(vaa)) {
    if(!local_outlets) {
      huc12_outlets <- lapply(outlet_comids, \(x) {
        findNLDI(
          comid = x, nav = "UT",
          distance_km = 9999, find = "huc12pp"
        )$UT_huc12pp
      }) |> bind_rows()
    }

    message("Subsetting upstream network from VAA...")
    ut_comids <- lapply(outlet_comids, \(x) {
      navigate_hydro_network(vaa, x, mode = "UT")
    })
    all_net <- filter(vaa, .data$comid %in% do.call(c, ut_comids))

    if(local_outlets)
      huc12_outlets <- huc12_outlets[huc12_outlets$comid %in% all_net$comid, ]

  } else {
    nldi_find <- if(local_outlets) "flowline" else c("huc12pp", "flowline")
    nldi_results <- lapply(outlet_comids, \(x) {
      findNLDI(
        comid = x, nav = "UT",
        distance_km = 9999, find = nldi_find
      )
    })

    if(!local_outlets)
      huc12_outlets <- lapply(nldi_results, \(x) x$UT_huc12pp) |>
        bind_rows()

    upstream_comids <- unique(as.integer(unlist(
      lapply(nldi_results, \(x) x$UT_flowlines$nhdplus_comid)
    )))

    message("Fetching flowline attributes for ",
      length(upstream_comids), " upstream COMIDs...")
    all_net <- get_nhdplus(
      comid = upstream_comids, realization = "flowline",
      skip_geometry = TRUE
    )

    if(is.null(all_net) || nrow(all_net) == 0)
      stop("Could not fetch flowline attributes for upstream network ",
        "(NHDPlusV2 OGC API unavailable). Try again or supply local data.",
        call. = FALSE)

    if(local_outlets)
      huc12_outlets <- huc12_outlets[huc12_outlets$comid %in% upstream_comids, ]
  }

  all_net <- add_toids(all_net, return_dendritic = TRUE)

  list(all_net = all_net, huc12_outlets = huc12_outlets)
}

#' Find immediately-upstream HUC12 outlets
#'
#' Given the full upstream network with toids, identifies HUC12 outlets
#' reachable from the start COMID without passing through another HUC12 outlet.
#'
#' @param all_net data.frame with comid and toid columns (from add_toids)
#' @param start_comid integer. The outlet COMID.
#' @param huc12_comids integer vector. COMIDs of all upstream HUC12 pour points.
#' @return integer vector of immediately-upstream HUC12 outlet COMIDs
#' @noRd
find_immediate_huc12_outlets <- function(all_net, start_comid, huc12_comids) {

  message("Finding immediately-upstream HUC12 outlets...")

  trim_comids <- setdiff(huc12_comids, start_comid)
  trimmed_net <- all_net[!all_net$comid %in% trim_comids, ] |>
    select("comid", "toid")
  trimmed_net$toid[!trimmed_net$toid %in% c(trimmed_net$comid, 0)] <- 0

  reachable <- unlist(
    navigate_network_dfs(trimmed_net, start_comid, direction = "up")
  )

  # immediate outlets are h12 comids whose downstream neighbor is in the

  # reachable set (or is the start comid itself)
  huc12_comids[huc12_comids %in%
    all_net$comid[all_net$toid %in% reachable |
      all_net$toid %in% start_comid]] |>
    unique()
}

#' Plan upstream HUC12 fetch operations
#'
#' Given NLDI-identified HUC12 IDs and the outlet HUC12 ID, determines
#' which HUC IDs to fetch for each of three drainage-area estimates:
#' HUC12-only, HUC10-level, and HUC08-level. Pure logic -- no web calls.
#'
#' @param all_huc12_ids character. NLDI-identified upstream HUC12 identifiers
#'   (12-digit strings).
#' @param outlet_huc12_ids character. The immediately-upstream HUC12 outlet
#'   identifier(s) (12-digit strings). May be length > 1 when the network
#'   branches upstream of the start COMID.
#' @return list with elements \code{huc12_est}, \code{huc10_est},
#'   \code{huc08_est}. Each estimate contains the IDs needed for fetching
#'   and a flag indicating whether it is the same as a simpler estimate.
#' @noRd
plan_upstream_huc12_fetches <- function(all_huc12_ids, outlet_huc12_ids) {

  outlet_huc10s <- unique(substr(outlet_huc12_ids, 1, 10))
  outlet_huc08s <- unique(substr(outlet_huc12_ids, 1, 8))
  all_huc10s <- unique(substr(all_huc12_ids, 1, 10))
  all_huc08s <- sort(unique(substr(all_huc12_ids, 1, 8)))

  # Estimate 1: HUC12-only -- just the NLDI IDs
  huc12_est <- list(fetch_ids = all_huc12_ids)

  # Estimate 2: HUC10-level -- complete upstream HUC10s + huc12pp in outlet HUC10(s)
  if(!all(all_huc10s %in% outlet_huc10s)) {
    upstream_huc10s <- all_huc10s[!all_huc10s %in% outlet_huc10s]
    local_huc12_ids <- all_huc12_ids[
      substr(all_huc12_ids, 1, 10) %in% outlet_huc10s &
        !all_huc12_ids %in% outlet_huc12_ids
    ]
    # Outlet HUC10 backfill: some HUC12s within the outlet HUC10 have no
    # huc12pp pour point (no flowline exits the HUC12 on the NHD network).
    # These are missed by NLDI navigation. The fix: query ALL HUC12s in
    # the outlet HUC10(s), then filter to those upstream of the outlet by
    # sort order (huc_12 < outlet huc_12).
    huc10_est <- list(
      bulk_huc10s = upstream_huc10s,
      local_huc12_ids = local_huc12_ids,
      outlet_backfill = list(
        outlet_huc10s = outlet_huc10s,
        outlet_huc12_ids = outlet_huc12_ids
      ),
      same_as_huc12 = FALSE
    )
  } else {
    huc10_est <- list(
      bulk_huc10s = character(0),
      local_huc12_ids = character(0),
      same_as_huc12 = TRUE
    )
  }

  # Estimate 3: HUC08-level -- complete upstream HUC08s + huc12pp in outlet HUC08(s)
  if(!all(all_huc08s %in% outlet_huc08s)) {
    upstream_huc08s <- all_huc08s[!all_huc08s %in% outlet_huc08s]
    local_huc12_ids_huc08 <- all_huc12_ids[
      substr(all_huc12_ids, 1, 8) %in% outlet_huc08s &
        !all_huc12_ids %in% outlet_huc12_ids
    ]
    huc08_est <- list(
      bulk_huc08s = upstream_huc08s,
      local_huc12_ids = local_huc12_ids_huc08,
      outlet_backfill = list(
        outlet_huc10s = outlet_huc10s,
        outlet_huc12_ids = outlet_huc12_ids
      ),
      same_as_huc10 = FALSE
    )
  } else {
    huc08_est <- list(
      bulk_huc08s = character(0),
      local_huc12_ids = character(0),
      same_as_huc10 = TRUE
    )
  }

  list(huc12_est = huc12_est, huc10_est = huc10_est, huc08_est = huc08_est)
}

#' Filter outlet backfill HUC12s by sort order
#'
#' Filters HUC12 features in the outlet HUC10(s) to those upstream of the
#' outlet HUC12(s) using lexicographic sort order.
#'
#' @param outlet_hu12s sf data.frame. All HUC12s in the outlet HUC10(s).
#' @param outlet_huc12_ids character. The outlet HUC12 identifier(s).
#' @param bulk sf data.frame or NULL. Bulk HUC12s to dedup against.
#' @param label character. Label for progress message.
#' @return sf data.frame of filtered backfill HUC12s (may have 0 rows).
#' @noRd
filter_outlet_backfill <- function(outlet_hu12s, outlet_huc12_ids,
  bulk = NULL, label = "backfill") {

  if(nrow(outlet_hu12s) == 0) return(outlet_hu12s)

  max_per_h10 <- tapply(
    outlet_huc12_ids, substr(outlet_huc12_ids, 1, 10), max
  )
  keep <- vapply(outlet_hu12s$huc_12, function(h12) {
    h10 <- substr(h12, 1, 10)
    h10 %in% names(max_per_h10) && h12 < max_per_h10[[h10]]
  }, logical(1))
  local <- outlet_hu12s[keep, ]

  if(!is.null(bulk) && nrow(local) > 0)
    local <- local[!local$huc_12 %in% bulk$huc_12, ]

  message("  Backfill (", label, "): ", sum(keep), " HUC12s in outlet HUC10")
  local
}

#' Union two HUC12 sf sets by huc_12 identifier
#'
#' Combines two sf data.frames of HUC12 features, deduplicating by
#' \code{huc_12}. Features in \code{target} take precedence when the same
#' HUC12 appears in both sets. Used to guarantee the superset property:
#' HUC10 estimate >= HUC12 estimate, HUC08 estimate >= HUC10 estimate.
#'
#' @param target sf data.frame. The estimate being extended.
#' @param base sf data.frame. The lower-level estimate to include.
#' @return sf data.frame containing all features from both sets, deduplicated.
#' @noRd
union_huc12_sets <- function(target, base) {
  if(is.null(base) || nrow(base) == 0) return(target)
  if(is.null(target) || nrow(target) == 0) return(base)

  missing <- base[!base$huc_12 %in% target$huc_12, ]
  if(nrow(missing) == 0) return(target)

  message("  Superset union: adding ", nrow(missing),
    " HUC12s from base estimate")
  bind_rows(target, missing)
}

#' Prepare in-memory HUC12 data for local subsetting
#'
#' Lowercases column names, validates required columns, and transforms to
#' EPSG:5070.
#'
#' @param huc12_data sf data.frame. HUC12 polygon table with mixed-case names.
#' @return sf data.frame with lowercase names and EPSG:5070 CRS.
#' @noRd
prepare_huc12_data <- function(huc12_data) {
  huc12_data <- rename_geometry(huc12_data, "geom")
  names(huc12_data) <- tolower(names(huc12_data))
  required <- c("huc_12", "ncontrb_a")
  missing <- setdiff(required, names(huc12_data))
  if(length(missing) > 0)
    stop("huc12_data is missing required columns: ",
      paste(missing, collapse = ", "))
  st_transform(huc12_data, 5070)
}

#' Prepare HUC12 pour point outlets for local use
#'
#' Reads a GPKG path (if given), renames \code{COMID} -> \code{comid} and
#' \code{FinalWBD_HUC12} -> \code{identifier} to match the NLDI schema, and
#' validates the required columns. Other columns and geometry are preserved.
#'
#' @param huc12_outlets sf data.frame or character path to a GPKG.
#' @return sf data.frame with \code{comid} (integer) and \code{identifier}
#'   (character) columns.
#' @noRd
prepare_huc12_outlets <- function(huc12_outlets) {
  if(is.character(huc12_outlets)) {
    if(!file.exists(huc12_outlets))
      stop("huc12_outlets file not found: ", huc12_outlets)
    huc12_outlets <- sf::read_sf(huc12_outlets)
  }

  if(!inherits(huc12_outlets, "sf"))
    stop("huc12_outlets must be an sf data.frame or a path to a GPKG file")

  rename_map <- c(COMID = "comid", FinalWBD_HUC12 = "identifier")
  for(old in names(rename_map)) {
    new <- rename_map[[old]]
    if(old %in% names(huc12_outlets) && !new %in% names(huc12_outlets))
      names(huc12_outlets)[names(huc12_outlets) == old] <- new
  }

  required <- c("comid", "identifier")
  missing <- setdiff(required, names(huc12_outlets))
  if(length(missing) > 0)
    stop("huc12_outlets is missing required columns: ",
      paste(missing, collapse = ", "))

  huc12_outlets$comid <- as.integer(huc12_outlets$comid)
  huc12_outlets$identifier <- as.character(huc12_outlets$identifier)
  huc12_outlets
}

#' Prepare in-memory NHDWaterbody data for local waterbody lookup
#'
#' Lowercases column names and validates the required \code{comid} column.
#'
#' @param waterbody_data sf data.frame. NHDWaterbody polygon table.
#' @return sf data.frame with lowercase column names.
#' @noRd
prepare_waterbody_data <- function(waterbody_data) {
  waterbody_data <- rename_geometry(waterbody_data, "geom")
  names(waterbody_data) <- tolower(names(waterbody_data))
  if(!"comid" %in% names(waterbody_data))
    stop("waterbody_data is missing required column: comid")
  waterbody_data
}

#' Prepare in-memory CatchmentSP data for local catchment operations
#'
#' Lowercases column names and validates the required \code{featureid} column.
#'
#' @param catchment_data sf data.frame. CatchmentSP polygon table.
#' @return sf data.frame with lowercase column names.
#' @noRd
prepare_catchment_data <- function(catchment_data) {
  catchment_data <- rename_geometry(catchment_data, "geom")
  names(catchment_data) <- tolower(names(catchment_data))
  if(!"featureid" %in% names(catchment_data))
    stop("catchment_data is missing required column: featureid")
  if(!"comid" %in% names(catchment_data))
    catchment_data$comid <- catchment_data$featureid
  catchment_data
}

#' Subset in-memory HUC12 data to match a fetch plan
#'
#' Local equivalent of \code{fetch_hu12_polygons} that subsets a pre-loaded
#' HUC12 table instead of calling web services. Returns the same list
#' structure so \code{assemble_hu12_sets} works identically.
#'
#' @param plan list. Output of \code{plan_upstream_huc12_fetches}.
#' @param huc12_data sf data.frame. Pre-prepared HUC12 table (lowercase names,
#'   EPSG:5070).
#' @return list matching \code{fetch_hu12_polygons} output.
#' @noRd
subset_hu12_polygons <- function(plan, huc12_data) {

  empty_sf <- st_sf(geometry = st_sfc(crs = 5070))

  by_id <- if(length(plan$huc12_est$fetch_ids) > 0) {
    huc12_data[huc12_data$huc_12 %in% plan$huc12_est$fetch_ids, ]
  } else {
    empty_sf
  }

  huc10_active <- !plan$huc10_est$same_as_huc12
  huc08_active <- huc10_active && !plan$huc08_est$same_as_huc10

  backfill_raw <- if(huc10_active) {
    bf <- plan$huc10_est$outlet_backfill
    if(length(bf$outlet_huc10s) > 0) {
      huc12_data[substr(huc12_data$huc_12, 1, 10) %in% bf$outlet_huc10s, ]
    } else {
      empty_sf
    }
  } else {
    empty_sf
  }

  huc08_bulk <- NULL
  huc10_extra <- NULL
  huc10_bulk <- NULL

  if(huc10_active && huc08_active) {
    if(length(plan$huc08_est$bulk_huc08s) > 0) {
      huc08_bulk <- huc12_data[
        substr(huc12_data$huc_12, 1, 8) %in% plan$huc08_est$bulk_huc08s, ]
    }

    covered_huc10s <- plan$huc10_est$bulk_huc10s[
      substr(plan$huc10_est$bulk_huc10s, 1, 8) %in%
        plan$huc08_est$bulk_huc08s
    ]
    extra_huc10s <- setdiff(plan$huc10_est$bulk_huc10s, covered_huc10s)

    if(length(extra_huc10s) > 0) {
      huc10_extra <- huc12_data[
        substr(huc12_data$huc_12, 1, 10) %in% extra_huc10s, ]
    }

  } else if(huc10_active) {
    if(length(plan$huc10_est$bulk_huc10s) > 0) {
      huc10_bulk <- huc12_data[
        substr(huc12_data$huc_12, 1, 10) %in% plan$huc10_est$bulk_huc10s, ]
    }
  }

  list(
    by_id = by_id,
    backfill_raw = backfill_raw,
    huc08_bulk = huc08_bulk,
    huc10_extra = huc10_extra,
    huc10_bulk = huc10_bulk
  )
}

#' Fetch raw HUC12 polygons for a fetch plan
#'
#' Executes all web service calls for HUC12 polygons described by a plan from
#' \code{plan_upstream_huc12_fetches}. Returns raw results without filtering,
#' backfill, or area computation -- that assembly happens in
#' \code{fetch_upstream_huc12s}.
#'
#' @param plan list. Output of \code{plan_upstream_huc12_fetches}.
#' @return list with:
#'   \describe{
#'     \item{by_id}{sf data.frame. HUC12 polygons fetched by NLDI IDs.}
#'     \item{backfill_raw}{sf data.frame. All HUC12s in outlet HUC10(s).}
#'     \item{huc08_bulk}{sf data.frame or NULL. All HUC12s in upstream HUC08s.}
#'     \item{huc10_extra}{sf data.frame or NULL. HUC12s in HUC10s not covered
#'       by the HUC08 query.}
#'     \item{huc10_bulk}{sf data.frame or NULL. All HUC12s in bulk HUC10s
#'       (when HUC08 estimate is inactive).}
#'   }
#' @noRd
fetch_hu12_polygons <- function(plan) {

  empty_sf <- st_sf(geometry = st_sfc(crs = 5070))

  # HUC12s by NLDI-identified IDs
  by_id <- if(length(plan$huc12_est$fetch_ids) > 0) {
    get_huc(id = plan$huc12_est$fetch_ids, type = "_nhdplusv2") |>
      st_transform(5070)
  } else {
    empty_sf
  }

  huc10_active <- !plan$huc10_est$same_as_huc12
  huc08_active <- huc10_active && !plan$huc08_est$same_as_huc10

  # backfill: all HUC12s in outlet HUC10(s)
  backfill_raw <- if(huc10_active) {
    bf <- plan$huc10_est$outlet_backfill
    if(length(bf$outlet_huc10s) > 0) {
      get_huc12_by_huc(bf$outlet_huc10s)
    } else {
      empty_sf
    }
  } else {
    empty_sf
  }

  huc08_bulk <- NULL
  huc10_extra <- NULL
  huc10_bulk <- NULL

  if(huc10_active && huc08_active) {
    # broadest query first: all HUC12s in upstream HUC08s
    if(length(plan$huc08_est$bulk_huc08s) > 0) {
      huc08_bulk <- get_huc12_by_huc(plan$huc08_est$bulk_huc08s)
    }

    # HUC10s not covered by the HUC08 bulk query
    covered_huc10s <- plan$huc10_est$bulk_huc10s[
      substr(plan$huc10_est$bulk_huc10s, 1, 8) %in%
        plan$huc08_est$bulk_huc08s
    ]
    extra_huc10s <- setdiff(plan$huc10_est$bulk_huc10s, covered_huc10s)

    if(length(extra_huc10s) > 0)
      huc10_extra <- get_huc12_by_huc(extra_huc10s)

  } else if(huc10_active) {
    # only HUC10 active, no HUC08
    if(length(plan$huc10_est$bulk_huc10s) > 0)
      huc10_bulk <- get_huc12_by_huc(plan$huc10_est$bulk_huc10s)
  }

  list(
    by_id = by_id,
    backfill_raw = backfill_raw,
    huc08_bulk = huc08_bulk,
    huc10_extra = huc10_extra,
    huc10_bulk = huc10_bulk
  )
}

#' Assemble upstream HUC12 polygon sets from raw fetched data
#'
#' Takes the raw polygon results from \code{fetch_hu12_polygons} and the
#' fetch plan, then applies backfill filtering, disconnect filtering, superset
#' enforcement, and area computation to produce three HUC12 polygon sets.
#' No web service calls.
#'
#' @param plan list. Output of \code{plan_upstream_huc12_fetches}.
#' @param polygons list. Output of \code{fetch_hu12_polygons}.
#' @return list with \code{hu12_by_huc12}, \code{hu12_by_huc10}, and
#'   \code{hu12_by_huc08} sf objects, each with columns \code{dasqkm},
#'   \code{ncontrb_sqkm}, \code{contrib_sqkm}.
#' @noRd
assemble_hu12_sets <- function(plan, polygons,
  HU_inclusion_override = NULL) {

  empty_sf <- st_sf(geometry = st_sfc(crs = 5070))

  hu12_by_huc12 <- if(nrow(polygons$by_id) > 0) {
    polygons$by_id
  } else {
    empty_sf
  }

  huc10_active <- !plan$huc10_est$same_as_huc12
  huc08_active <- huc10_active && !plan$huc08_est$same_as_huc10

  if(!huc10_active) {
    hu12_by_huc10 <- NULL
    hu12_by_huc08 <- NULL
  } else {
    bf <- plan$huc10_est$outlet_backfill
    backfill_raw <- polygons$backfill_raw

    if(huc08_active) {
      huc08_bulk <- if(!is.null(polygons$huc08_bulk)) {
        polygons$huc08_bulk
      } else {
        empty_sf
      }
      huc10_extra <- if(!is.null(polygons$huc10_extra)) {
        polygons$huc10_extra
      } else {
        empty_sf
      }

      # derive HUC10 subset from HUC08 results
      covered_huc10s <- plan$huc10_est$bulk_huc10s[
        substr(plan$huc10_est$bulk_huc10s, 1, 8) %in%
          plan$huc08_est$bulk_huc08s
      ]
      huc10_from_huc08 <- if(nrow(huc08_bulk) > 0 &&
          length(covered_huc10s) > 0) {
        huc08_bulk[
          substr(huc08_bulk$huc_12, 1, 10) %in% covered_huc10s, ]
      } else {
        empty_sf
      }

      message("  HUC10 from HUC08: ", nrow(huc10_from_huc08),
        " HUC12s reused, ", nrow(huc10_extra), " fetched separately")

      # assemble HUC08 estimate
      huc08_parts <- list()
      if(nrow(huc08_bulk) > 0) huc08_parts$bulk <- huc08_bulk
      if(nrow(huc10_extra) > 0) huc08_parts$outlet_huc08_huc10s <- huc10_extra
      huc08_bulk_all <- if(length(huc08_parts) > 0) {
        bind_rows(huc08_parts)
      } else {
        NULL
      }
      backfill_huc08 <- filter_outlet_backfill(
        backfill_raw, bf$outlet_huc12_ids,
        bulk = huc08_bulk_all, label = "HUC08"
      )
      if(nrow(backfill_huc08) > 0) huc08_parts$backfill <- backfill_huc08

      hu12_by_huc08 <- if(length(huc08_parts) > 0) {
        st_sf(bind_rows(huc08_parts)) |> st_transform(5070)
      } else {
        empty_sf
      }

      # assemble HUC10 estimate
      huc10_parts <- list()
      if(nrow(huc10_from_huc08) > 0)
        huc10_parts$from_huc08 <- huc10_from_huc08
      if(nrow(huc10_extra) > 0) huc10_parts$extra <- huc10_extra
      all_huc10_bulk <- if(length(huc10_parts) > 0) {
        bind_rows(huc10_parts)
      } else {
        NULL
      }
      backfill_huc10 <- filter_outlet_backfill(
        backfill_raw, bf$outlet_huc12_ids,
        bulk = all_huc10_bulk, label = "HUC10"
      )
      if(nrow(backfill_huc10) > 0)
        huc10_parts$backfill <- backfill_huc10

      hu12_by_huc10 <- if(length(huc10_parts) > 0) {
        st_sf(bind_rows(huc10_parts)) |> st_transform(5070)
      } else {
        empty_sf
      }

      # filter disconnected HUC12s before superset union
      network_ids <- hu12_by_huc12$huc_12
      keep_10 <- filter_disconnected_huc12s(
        hu12_by_huc10$huc_12, network_ids,
        override_parents = HU_inclusion_override
      )
      hu12_by_huc10 <- hu12_by_huc10[hu12_by_huc10$huc_12 %in% keep_10, ]
      keep_08 <- filter_disconnected_huc12s(
        hu12_by_huc08$huc_12, network_ids, parent_nchar = 8L,
        override_parents = HU_inclusion_override
      )
      hu12_by_huc08 <- hu12_by_huc08[hu12_by_huc08$huc_12 %in% keep_08, ]

      # enforce superset property: HUC10 >= HUC12, HUC08 >= HUC10
      hu12_by_huc10 <- union_huc12_sets(hu12_by_huc10, hu12_by_huc12)
      hu12_by_huc08 <- union_huc12_sets(hu12_by_huc08, hu12_by_huc10)

    } else {
      # only HUC10 active, no HUC08
      hu12_by_huc08 <- NULL
      huc10_parts <- list()
      if(!is.null(polygons$huc10_bulk))
        huc10_parts$bulk <- polygons$huc10_bulk
      backfill_huc10 <- filter_outlet_backfill(
        backfill_raw, bf$outlet_huc12_ids,
        bulk = huc10_parts$bulk, label = "HUC10"
      )
      if(nrow(backfill_huc10) > 0)
        huc10_parts$backfill <- backfill_huc10

      hu12_by_huc10 <- if(length(huc10_parts) > 0) {
        st_sf(bind_rows(huc10_parts)) |> st_transform(5070)
      } else {
        empty_sf
      }

      # filter disconnected HUC12s before superset union
      network_ids <- hu12_by_huc12$huc_12
      keep_10 <- filter_disconnected_huc12s(
        hu12_by_huc10$huc_12, network_ids,
        override_parents = HU_inclusion_override
      )
      hu12_by_huc10 <- hu12_by_huc10[hu12_by_huc10$huc_12 %in% keep_10, ]

      # enforce superset property: HUC10 >= HUC12
      hu12_by_huc10 <- union_huc12_sets(hu12_by_huc10, hu12_by_huc12)
    }
  }

  hu12_by_huc12 <- add_huc12_area_columns(hu12_by_huc12)
  if(!is.null(hu12_by_huc10))
    hu12_by_huc10 <- add_huc12_area_columns(hu12_by_huc10)
  if(!is.null(hu12_by_huc08))
    hu12_by_huc08 <- add_huc12_area_columns(hu12_by_huc08)

  message("  HUC12 query: ", nrow(hu12_by_huc12), " HUC12s, ",
    "total = ", round(sum(hu12_by_huc12$dasqkm), 2), " sq km, ",
    "contributing = ", round(sum(hu12_by_huc12$contrib_sqkm), 2),
    " sq km")
  if(!is.null(hu12_by_huc10))
    message("  HUC10 query: ", nrow(hu12_by_huc10), " HUC12s, ",
      "total = ", round(sum(hu12_by_huc10$dasqkm), 2), " sq km, ",
      "contributing = ", round(sum(hu12_by_huc10$contrib_sqkm), 2),
      " sq km")
  if(!is.null(hu12_by_huc08))
    message("  HUC08 query: ", nrow(hu12_by_huc08), " HUC12s, ",
      "total = ", round(sum(hu12_by_huc08$dasqkm), 2), " sq km, ",
      "contributing = ", round(sum(hu12_by_huc08$contrib_sqkm), 2),
      " sq km")

  list(
    hu12_by_huc12 = hu12_by_huc12,
    hu12_by_huc10 = hu12_by_huc10,
    hu12_by_huc08 = hu12_by_huc08
  )
}

#' Fetch upstream HUC12 polygons according to a fetch plan
#'
#' Fetches raw HUC12 polygons via \code{fetch_hu12_polygons}, then assembles
#' and filters them via \code{assemble_hu12_sets}. When \code{polygons} is
#' provided, skips the web service calls and uses the pre-fetched data.
#'
#' @param plan list. Output of \code{plan_upstream_huc12_fetches}.
#' @param polygons list or NULL. Pre-fetched polygon data matching the
#'   structure returned by \code{fetch_hu12_polygons}. When NULL, polygons
#'   are fetched from web services.
#' @param huc12_data sf data.frame or NULL. Pre-prepared in-memory HUC12 table
#'   (lowercase names, EPSG:5070). When non-NULL, used to subset polygons
#'   locally instead of fetching from web services.
#' @return list with \code{hu12_by_huc12}, \code{hu12_by_huc10}, and
#'   \code{hu12_by_huc08} sf objects, each with columns \code{dasqkm},
#'   \code{ncontrb_sqkm}, \code{contrib_sqkm}.
#' @noRd
fetch_upstream_huc12s <- function(plan, polygons = NULL,
  huc12_data = NULL, HU_inclusion_override = NULL) {
  if(is.null(polygons)) {
    if(!is.null(huc12_data)) {
      polygons <- subset_hu12_polygons(plan, huc12_data)
    } else {
      polygons <- fetch_hu12_polygons(plan)
    }
  }
  assemble_hu12_sets(plan, polygons,
    HU_inclusion_override = HU_inclusion_override)
}

#' Fetch upstream HUC12s and compute area columns
#'
#' Derives HUC08/HUC10 groupings from HUC12 identifiers and fetches
#' HUC12 polygons via the OGC API. Computes total, non-contributing,
#' and contributing area columns on the result.
#'
#' @param huc12_outlets sf data.frame. HUC12 pour points with \code{comid}
#'   and \code{identifier} columns.
#' @param outlet_huc sf data.frame. The immediately-upstream HUC12 outlet row.
#' @param huc12_data sf data.frame or NULL. Pre-prepared in-memory HUC12 table
#'   (lowercase names, EPSG:5070). When non-NULL, passed to
#'   \code{fetch_upstream_huc12s} to avoid web service calls.
#' @return list with \code{hu12_by_huc12}, \code{hu12_by_huc10}, and
#'   \code{hu12_by_huc08} sf objects, each with columns \code{dasqkm},
#'   \code{ncontrb_sqkm}, \code{contrib_sqkm}.
#' @noRd
get_upstream_huc12s <- function(huc12_outlets, outlet_huc,
  huc12_data = NULL, HU_inclusion_override = NULL) {

  all_huc12_ids <- huc12_outlets$identifier
  outlet_huc12_ids <- outlet_huc$identifier

  plan <- plan_upstream_huc12_fetches(all_huc12_ids, outlet_huc12_ids)
  result <- fetch_upstream_huc12s(plan, huc12_data = huc12_data,
    HU_inclusion_override = HU_inclusion_override)

  # The network-driven plan may not fetch every HU12 under an
  # HU_inclusion_override parent. For closed-basin starts the outlet HU8 is
  # treated as an outlet HU8 by the plan (not bulk-fetched), so HU12s that
  # have no flowline pour point (e.g. lakes, frontal units) are missed.
  # Force-include all HU12s under override parents from huc12_data.
  if(!is.null(HU_inclusion_override) && !is.null(huc12_data)) {
    result <- force_include_override_huc12s(result, HU_inclusion_override,
      huc12_data)
  }

  result
}

#' Force-include all HU12s under override parents
#'
#' Adds HU12s from \code{huc12_data} whose parent HU10 (or HU8) matches any
#' code in \code{HU_inclusion_override}, into \code{hu12_by_huc10} and
#' \code{hu12_by_huc08}. Mirrors the cascade in
#' \code{filter_disconnected_huc12s}: a 10-digit override applies at the
#' HUC10 grouping and (via truncation) at the HUC08 grouping; an 8-digit
#' override applies at the HUC08 grouping only.
#'
#' @param hu12_result list. Output of \code{fetch_upstream_huc12s}.
#' @param HU_inclusion_override character. Override codes (8 or 10 digits).
#' @param huc12_data sf data.frame. Pre-prepared HUC12 table.
#' @return list with the same shape as \code{hu12_result}, with augmented
#'   \code{hu12_by_huc10} and \code{hu12_by_huc08}.
#' @noRd
force_include_override_huc12s <- function(hu12_result, HU_inclusion_override,
  huc12_data) {

  override_10 <- HU_inclusion_override[nchar(HU_inclusion_override) >= 10]
  override_10 <- unique(substr(override_10, 1, 10))
  override_8 <- unique(substr(HU_inclusion_override, 1, 8))

  added_h10 <- 0L
  added_h08 <- 0L

  if(length(override_10) > 0 && !is.null(hu12_result$hu12_by_huc10)) {
    must_have <- huc12_data[
      substr(huc12_data$huc_12, 1, 10) %in% override_10, ]
    if(nrow(must_have) > 0) {
      must_have <- add_huc12_area_columns(must_have)
      before_n <- nrow(hu12_result$hu12_by_huc10)
      hu12_result$hu12_by_huc10 <- union_huc12_sets(
        hu12_result$hu12_by_huc10, must_have)
      added_h10 <- nrow(hu12_result$hu12_by_huc10) - before_n
    }
  }

  if(length(override_8) > 0 && !is.null(hu12_result$hu12_by_huc08)) {
    must_have <- huc12_data[
      substr(huc12_data$huc_12, 1, 8) %in% override_8, ]
    if(nrow(must_have) > 0) {
      must_have <- add_huc12_area_columns(must_have)
      before_n <- nrow(hu12_result$hu12_by_huc08)
      hu12_result$hu12_by_huc08 <- union_huc12_sets(
        hu12_result$hu12_by_huc08, must_have)
      added_h08 <- nrow(hu12_result$hu12_by_huc08) - before_n
    }
  }

  if(added_h10 > 0 || added_h08 > 0)
    message("  Force-included from HU_inclusion_override: +",
      added_h10, " HU12s in HUC10 result, +", added_h08,
      " HU12s in HUC08 result")

  hu12_result
}

#' Filter disconnected HUC12s from broader HUC queries
#'
#' When we fetch HUC12s by a broader HUC level (HUC10 or HUC08), we get all
#' HUC12s in each parent HUC group. The purpose of fetching by group is to
#' capture disconnected HUC12s -- those that are upstream in a landscape
#' sense but have no flowline exiting on the NHD network, so they are not
#' discovered by network navigation alone.
#'
#' However, this can also pull in HUC12s from groups that are not actually
#' upstream of the original outlet. To distinguish, we check whether the
#' outlet HUC12 of each group (the maximum HUC12 by sort order within the
#' group) is present in the on-network set discovered by NLDI navigation.
#' If the group outlet is on-network, the entire group is upstream and all
#' its HUC12s are kept -- including disconnected ones. If the group outlet
#' is not on-network, the group as a whole is not upstream, so only HUC12s
#' that were individually discovered on-network are retained.
#'
#' Groups are defined by \code{parent_nchar}: 10 for HUC10-level grouping
#' (used with the HUC10 query), 8 for HUC08-level grouping (used with the
#' HUC08 query). The grouping level should match the query level so that
#' entire HUC groups are kept or dropped as a unit.
#'
#' Parent HUC codes listed in \code{override_parents} bypass this check:
#' all HUC12s in the matching group are kept regardless of whether the
#' outlet is on-network. This handles regions like the Prairie Potholes
#' (e.g. HUC08 10130106 in South Dakota) where the HUC drains into the
#' network but lacks a network-connected outlet flowline.
#'
#' @param broader_huc12s character. HUC12 IDs from a broader HUC query
#'   (e.g. hu12_by_huc10 or hu12_by_huc08).
#' @param network_huc12s character. HUC12 IDs from the on-network (NLDI)
#'   hu12_by_huc12 set.
#' @param parent_nchar integer. Number of characters defining the parent
#'   group: 10 for HUC10, 8 for HUC08.
#' @param override_parents character or NULL. Eight- or ten-digit HUC codes
#'   whose HUC12s are always kept.
#' @return character vector of HUC12 IDs to keep.
#' @noRd
filter_disconnected_huc12s <- function(broader_huc12s, network_huc12s,
  parent_nchar = 10L, override_parents = NULL) {

  if(length(broader_huc12s) == 0) return(broader_huc12s)

  # match override_parents at the current grouping level
  overrides <- if(!is.null(override_parents)) {
    substr(override_parents, 1, parent_nchar)
  }

  parents <- substr(broader_huc12s, 1, parent_nchar)
  unique_parents <- unique(parents)

  keep <- logical(length(broader_huc12s))

  for(p in unique_parents) {
    in_p <- parents == p
    h12s_in_p <- broader_huc12s[in_p]
    outlet_h12 <- max(h12s_in_p)

    if(p %in% overrides) {
      keep[in_p] <- TRUE
    } else if(outlet_h12 %in% network_huc12s) {
      keep[in_p] <- TRUE
    } else {
      keep[in_p] <- h12s_in_p %in% network_huc12s
    }
  }

  n_dropped <- sum(!keep)
  if(n_dropped > 0)
    message("  Filtered ", n_dropped,
      " disconnected HUC12s from broader query")

  broader_huc12s[keep]
}

#' Add area columns to HUC12 sf object
#'
#' Computes \code{dasqkm} (total area from geometry), \code{ncontrb_sqkm}
#' (non-contributing area converted from acres), and \code{contrib_sqkm}
#' (total minus non-contributing).
#'
#' @param hu12 sf data.frame of HUC12 features with \code{ncontrb_a} column.
#' @return sf data.frame with added area columns
#' @noRd
add_huc12_area_columns <- function(hu12) {
  acres_to_sqkm <- 0.00404686
  hu12$dasqkm <- as.numeric(set_units(st_area(hu12), "km^2"))
  hu12$ncontrb_sqkm <- hu12$ncontrb_a * acres_to_sqkm
  hu12$contrib_sqkm <- hu12$dasqkm - hu12$ncontrb_sqkm
  hu12
}

#' Assemble drainage area estimates from HUC12 results
#'
#' Pure logic -- computes the six DA estimate values from assembled HUC12
#' polygon sets and the gap area. No web service calls.
#'
#' @param hu12_result list. Output of \code{fetch_upstream_huc12s} with
#'   \code{hu12_by_huc12}, \code{hu12_by_huc10}, \code{hu12_by_huc08}.
#' @param extra_and_local numeric. Gap area in sq km from
#'   \code{compute_gap_area}.
#' @return list with \code{da_huc12_sqkm}, \code{da_huc10_sqkm},
#'   \code{da_huc08_sqkm}, \code{contrib_da_huc12_sqkm},
#'   \code{contrib_da_huc10_sqkm}, \code{contrib_da_huc08_sqkm}.
#' @noRd
assemble_da_estimates <- function(hu12_result, extra_and_local) {

  da_huc12_sqkm <- sum(hu12_result$hu12_by_huc12$dasqkm) +
    extra_and_local
  contrib_da_huc12_sqkm <- sum(hu12_result$hu12_by_huc12$contrib_sqkm) +
    extra_and_local

  if(!is.null(hu12_result$hu12_by_huc10)) {
    da_huc10_sqkm <- sum(hu12_result$hu12_by_huc10$dasqkm) +
      extra_and_local
    contrib_da_huc10_sqkm <- sum(hu12_result$hu12_by_huc10$contrib_sqkm) +
      extra_and_local
  } else {
    da_huc10_sqkm <- NA_real_
    contrib_da_huc10_sqkm <- NA_real_
  }

  if(!is.null(hu12_result$hu12_by_huc08)) {
    da_huc08_sqkm <- sum(hu12_result$hu12_by_huc08$dasqkm) +
      extra_and_local
    contrib_da_huc08_sqkm <- sum(hu12_result$hu12_by_huc08$contrib_sqkm) +
      extra_and_local
  } else {
    da_huc08_sqkm <- NA_real_
    contrib_da_huc08_sqkm <- NA_real_
  }

  list(
    da_huc12_sqkm = da_huc12_sqkm,
    da_huc10_sqkm = da_huc10_sqkm,
    da_huc08_sqkm = da_huc08_sqkm,
    contrib_da_huc12_sqkm = contrib_da_huc12_sqkm,
    contrib_da_huc10_sqkm = contrib_da_huc10_sqkm,
    contrib_da_huc08_sqkm = contrib_da_huc08_sqkm
  )
}

#' Compute gap area between outlet and HUC12 outlets
#'
#' Splits the catchment at each HUC12 outlet, navigates upstream from each
#' outlet, and identifies catchments in the gap between the gage
#' and the HUC12 outlets.
#'
#' @param outlet_huc sf data.frame. The immediately-upstream HUC12 outlet(s).
#'   May have multiple rows when the network branches.
#' @param all_net data.frame. Full upstream network with comid, toid, areasqkm.
#' @param nav_net data.frame. Network used for upstream navigation. Defaults
#'   to \code{all_net}; pass the full VAA when using local navigation.
#' @param split_catchments list or NULL. Pre-computed split catchments keyed by
#'   COMID. Each element is an sf data.frame as returned by
#'   \code{get_split_catchment} (projected to EPSG:5070). When NULL, split
#'   catchments are fetched from the web service.
#' @param gap_catchments sf data.frame or NULL. Pre-fetched NHDPlusV2 catchment
#'   geometries for the gap flowlines. When NULL, fetched via
#'   \code{get_nhdplus}.
#' @param outlet_info list or NULL. Output of \code{negotiate_outlet_catchment}
#'   with \code{gage_point}, \code{threshold_exceeded}, and
#'   \code{flowline_measure}. When non-NULL and \code{threshold_exceeded} is
#'   TRUE, the outlet catchment is split at the gage point and only the
#'   upstream portion contributes to gap area.
#' @return list with \code{extra_net} (data.frame), \code{extra_catchments}
#'   (sf data.frame), \code{split_catchment} (sf data.frame),
#'   \code{extra_and_local} (numeric area in sq km),
#'   \code{outlet_split_catchment} (sf data.frame or NULL).
#' @noRd
compute_gap_area <- function(outlet_huc, all_net, nav_net = all_net,
  split_catchments = NULL, gap_catchments = NULL, outlet_info = NULL) {

  n_outlets <- nrow(outlet_huc)
  message("Splitting catchments at ", n_outlets, " HUC12 outlet(s)...")

  all_hu12_outlet_ut <- integer(0)
  total_local_dasqkm <- 0
  split_catches <- vector("list", n_outlets)

  for(i in seq_len(n_outlets)) {
    oh <- outlet_huc[i, ]

    if(is.null(split_catchments)) {
      split_catch <- get_split_catchment(
        st_geometry(oh),
        upstream = FALSE
      ) |> st_transform(5070)
    } else {
      split_catch <- split_catchments[[as.character(oh$comid)]]
    }

    split_catch$dasqkm <- as.numeric(set_units(st_area(split_catch), "km^2"))
    split_catches[[i]] <- split_catch

    local_dasqkm <-
      split_catch$dasqkm[split_catch$id == "catchment"] -
      split_catch$dasqkm[split_catch$id == "splitCatchment"]
    total_local_dasqkm <- total_local_dasqkm + local_dasqkm

    ut_comids <- navigate_hydro_network(nav_net, oh$comid, mode = "UT")
    all_hu12_outlet_ut <- union(all_hu12_outlet_ut, ut_comids)
  }

  split_catch <- do.call(rbind, split_catches)

  # catchments between the gage and the HUC12 outlets
  message("Navigating network upstream of HUC12 outlets...")
  extra_net <- all_net[!all_net$comid %in% all_hu12_outlet_ut, ]
  message("  ", nrow(extra_net),
    " extra catchments between outlet and HUC12 outlets")

  extra_and_local <- sum(c(extra_net$areasqkm, total_local_dasqkm))

  # split outlet catchment at the gage point if threshold exceeded
  outlet_split_catchment <- NULL
  if(!is.null(outlet_info) && isTRUE(outlet_info$threshold_exceeded)) {
    message("Splitting outlet catchment at gage point...")
    outlet_split_catchment <- tryCatch({
      sc <- get_split_catchment(
        outlet_info$gage_point, upstream = FALSE
      ) |> st_transform(5070)
      sc$dasqkm <- as.numeric(set_units(st_area(sc), "km^2"))
      sc
    }, error = function(e) {
      warning("Failed to split outlet catchment at gage: ", e$message,
        call. = FALSE)
      NULL
    })

    if(!is.null(outlet_split_catchment)) {
      full_area <- outlet_split_catchment$dasqkm[
        outlet_split_catchment$id == "catchment"]
      split_area <- outlet_split_catchment$dasqkm[
        outlet_split_catchment$id == "splitCatchment"]
      downstream_area <- full_area - split_area
      extra_and_local <- extra_and_local - downstream_area
      message("  Outlet split: full=", round(full_area, 2),
        " km2, upstream=", round(split_area, 2),
        " km2, removed=", round(downstream_area, 2), " km2")
    }
  }

  # fetch catchment geometries for the extra portion
  if(nrow(extra_net) > 0) {
    if(is.null(gap_catchments)) {
      message("Fetching extra catchment geometries...")
      extra_cat <- get_nhdplus(
        comid = extra_net$comid, realization = "catchment"
      )
      if(is.null(extra_cat)) {
        warning("Could not fetch extra catchment geometries ",
          "(NHDPlusV2 OGC API unavailable); scalar estimates still ",
          "valid but catchment polygons unavailable.", call. = FALSE)
        extra_cat <- st_sf(geometry = st_sfc(crs = 5070))
      }
    } else {
      extra_cat <- gap_catchments[
        gap_catchments$comid %in% extra_net$comid, ]
    }
  } else {
    extra_cat <- st_sf(geometry = st_sfc(crs = 5070))
  }

  list(
    extra_net = extra_net,
    extra_catchments = extra_cat,
    split_catchment = split_catch,
    extra_and_local = extra_and_local,
    outlet_split_catchment = outlet_split_catchment
  )
}

#' Estimate drainage area from NHDPlusHR catchments
#'
#' Fetches NHDPlusHR flowlines, indexes the start point to the HR network
#' using drainage area to disambiguate, navigates upstream, and sums
#' catchment areas. Returns NA with a warning on any failure.
#'
#' @param start_feature sf data.frame. Resolved start feature with geometry.
#' @param network_da numeric. NHDPlusV2 network total drainage area (sq km)
#'   used to match the best HR flowline.
#' @param hu12_polys sf data.frame. Largest available HUC12 polygon set used
#'   to define the bounding box for fetching the full HR network.
#' @param hr_network sf data.frame or NULL. Pre-fetched NHDPlusHR flowlines
#'   (type \code{"networknhdflowline"}). When NULL, fetched via
#'   \code{get_nhdphr}.
#' @param hr_catchments sf data.frame or NULL. Pre-fetched NHDPlusHR
#'   catchments with \code{nhdplusid} and optionally \code{areasqkm} columns.
#'   When NULL, fetched via \code{get_nhdphr}.
#' @return list with \code{nhdplushr_network_dasqkm} (numeric or NA) and
#'   \code{nhdplushr_boundary} (sfc_GEOMETRY or NULL).
#' @noRd
get_nhdplushr_da_estimate <- function(start_feature, network_da, hu12_polys,
  hr_network = NULL, hr_catchments = NULL) {

  fail <- list(nhdplushr_network_dasqkm = NA_real_,
    nhdplushr_boundary = NULL)

  tryCatch({

    # 1. extract start point (and full geometry for proximity filtering)
    start_geom <- st_geometry(start_feature)
    is_point <- inherits(start_geom, "sfc_POINT")
    start_point <- if(is_point) start_geom else st_centroid(start_geom)

    # 2. fetch HR network if not provided
    if(is.null(hr_network)) {
      start_geom_5070 <- st_transform(start_geom, 5070)
      start_buf <- st_buffer(start_geom_5070, 500)
      aoi <- st_union(
        st_transform(st_as_sfc(st_bbox(hu12_polys)), 5070),
        start_buf
      )
      aoi <- st_as_sfc(st_bbox(st_transform(aoi, 4326)))

      message("  Fetching NHDPlusHR network for full AOI...")
      hr_network <- get_nhdphr(AOI = aoi, type = "networknhdflowline")
    }

    if(is.null(hr_network) || nrow(hr_network) == 0)
      stop("no HR flowlines in AOI")

    message("  Fetched ", nrow(hr_network), " HR flowlines")

    # 3. find HR outlet flowline(s) to start navigation from
    if(!is_point) {
      # waterbody: find HR flowlines within 500m of the lake polygon,
      # then identify outlets where tonode is not in fromnode
      wb_5070 <- st_transform(start_geom, 5070)
      wb_buf <- st_buffer(wb_5070, 500)
      hr_5070 <- st_transform(hr_network, 5070)
      dists <- as.numeric(st_distance(hr_5070, wb_buf))
      hr_wb <- hr_network[dists <= 0, ]

      if(nrow(hr_wb) == 0)
        stop("no HR flowlines within 500m of waterbody")

      message("  ", nrow(hr_wb),
        " HR flowlines within 500m of waterbody polygon")

      hr_wb_outlets <- hr_wb[
        !hr_wb$tonode %in% hr_wb$fromnode, ]

      if(nrow(hr_wb_outlets) == 0) hr_wb_outlets <- hr_wb

      hr_start_ids <- hr_wb_outlets$nhdplusid

      message("  Found ", length(hr_start_ids), " HR outlet flowline(s)")

    } else {
      # point: index to nearby HR flowlines, disambiguate by DA
      start_5070 <- st_transform(start_point, 5070)
      hr_5070 <- st_transform(hr_network, 5070)
      start_buf <- st_buffer(start_5070, 500)
      dists <- as.numeric(st_distance(hr_5070, start_buf))
      hr_local <- hr_network[dists <= 0, ]

      if(nrow(hr_local) == 0)
        stop("no HR flowlines within 500m of start feature")

      indexes <- index_points_to_lines(
        st_transform(hr_local, 5070), start_5070,
        search_radius = set_units(500, "m"),
        max_matches = 10
      )

      if(is.null(indexes) || nrow(indexes) == 0)
        stop("no HR index matches found")

      best <- disambiguate_indexes(
        indexes,
        st_drop_geometry(hr_local[, c("nhdplusid", "totdasqkm")]),
        data.frame(point_id = 1, totdasqkm = network_da)
      )
      hr_start_ids <- best$nhdplusid
    }

    for(sid in hr_start_ids) {
      message("  HR outlet NHDPlusID = ",
        format(sid, scientific = FALSE),
        " (TotDASqKM = ",
        hr_network$totdasqkm[hr_network$nhdplusid == sid], ")")
    }

    # 4. navigate upstream from each outlet on HR network
    message("  Navigating upstream on HR network...")
    ut_ids <- unique(unlist(lapply(hr_start_ids, \(sid) {
      navigate_hydro_network(hr_network, sid, mode = "UT")
    })))

    message("  Found ", length(ut_ids), " HR flowlines upstream")

    # 5. fetch HR catchments if not provided
    if(is.null(hr_catchments)) {
      message("  Fetching NHDPlusHR catchments...")
      hr_catchments <- get_nhdphr(
        ids = as.character(ut_ids), type = "nhdpluscatchment"
      )
    } else {
      hr_catchments <- hr_catchments[
        hr_catchments$nhdplusid %in% ut_ids, ]
    }

    if(is.null(hr_catchments) || nrow(hr_catchments) == 0)
      stop("no HR catchments returned")

    # 6. sum catchment areas
    if("areasqkm" %in% names(hr_catchments)) {
      da <- sum(hr_catchments$areasqkm, na.rm = TRUE)
    } else {
      hr_catch_5070 <- st_transform(hr_catchments, 5070)
      da <- sum(as.numeric(st_area(hr_catch_5070))) / 1e6
    }

    list(nhdplushr_network_dasqkm = da,
      nhdplushr_boundary = st_union(st_geometry(hr_catchments)))

  }, error = function(e) {
    warning("NHDPlusHR estimate failed: ", conditionMessage(e),
      call. = FALSE)
    fail
  })
}
