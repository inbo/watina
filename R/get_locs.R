# DOCUMENTATION GET LOCS -------------------------------------------------------
#' Get locations from the data warehouse
#'
#' Returns locations (and optionally, observation wells) from the \emph{Watina}
#' data warehouse that meet several criteria, either as a lazy object or as a
#' local tibble. Criteria refer to spatial or non-spatial physical attributes of
#' the location or the location's observation wells. Essential metadata are
#' included in the result.
#'
#' (TO BE ADDED: Explanation on the different available values of loc_type and
#' loc_validity)
#'
#' The lazy object returns a \code{loc_wid} variable, for further use in
#' \emph{remote} queries. However, don't use it in local objects: \code{loc_wid}
#' is not to be regarded as stable. Therefore, \code{collect = TRUE} does not
#' return \code{loc_wid}.
#'
#' The result also provides metadata at the level of the observation well, even
#' when \code{obswells = FALSE}. In the latter case, this refers to the
#' variables \code{soilsurf_ost}, \code{measuringref_ost}, \code{tubelength},
#' \code{filterlength}, \code{filterdepth}. See the argument \code{obswell_aggr}
#' for options of how to aggregate this information at the location level; by
#' default the latest observation well is used (per location) that meets the
#' criteria on filterdepth. Mind that \code{obswells = FALSE} and
#' \code{filterdepth_na = TRUE} may lead to missing filterdepth values at
#' locations which do have a value for an older observation well, but not for
#' the most recent one.
#'
#' Please note the meaning of observation well in Watina: if there are multiple
#' observation wells attached to one location, these belong to \emph{other
#' timeframes}! So one location always coincides with exactly one observation
#' well at one moment in time. Multiple observation wells can succeed one
#' another because of physical alterations (e.g. damage of a piezometer). Here,
#' the term 'observation well' is used to refer to a fixed installed device in
#' the field (groundwater piezometer, surface water level measurement device).
#'
#'
#' @param con A \code{DBIConnection} object to Watina. See
#'   \code{\link{connect_watina}} to generate one.
#' @param filterdepth_range Numeric vector of length 2. Specifies the allowed
#'   range of the depth of the filter below soil surface, as meters (minimum and
#'   maximum allowed filterdepth, respectively). This condition is only applied
#'   to groundwater piezometers. The second vector element cannot be smaller
#'   than the first. Note that 'filterdepth' takes into account \emph{half} the
#'   length of the filter. It is always assumed that filters are at the bottom
#'   of the tube. Hence \code{filterdepth = tubelength - filterlength / 2 -
#'   [tubelength part above soil surface]}.
#'   If filterlength is missing, it is assumed to be 0.3 m. With \code{obswells
#'   = FALSE}, a location is kept whenever one observation well fulfills the
#'   condition.
#' @param filterdepth_guess Logical. Only relevant for groundwater piezometers.
#'   Defaults to \code{FALSE}. For observation wells of which tubelength is
#'   known, but not the part of the tubelength above soil surface (height of
#'   measuring point), filterdepth cannot be calculated and is missing. However,
#'   filterdepth will never be larger than tubelength minus half the
#'   filterlength; hence a maximum possible (i.e. conservative) value for
#'   filterdepth is given by \code{tubelength - filterlength / 2}. With
#'   \code{filterdepth_guess = TRUE}, filterdepth is replaced by this value when
#'   it cannot be calculated and tubelength is available. This is done before
#'   applying the \code{filterdepth_range} condition. To mark these cases, a
#'   logical variable \code{filterdepth_guessed} is added to the result:
#'   \code{TRUE} for wells where filterdepth was replaced; \code{FALSE} in all
#'   other rows.
#' @param filterdepth_na Logical. Are observation wells with missing filterdepth
#'   value to be included? Defaults to \code{FALSE}. With
#'   \code{filterdepth_guess = TRUE}, this has only effect on the
#'   \emph{remaining} observation wells with missing filterdepth value.
#' @param obswells Logical. If \code{TRUE}, the returned object distinguishes
#'   all observation wells (see \emph{Details}) that meet the
#'   \code{filterdepth_range} condition (or have missing filterdepth, if
#'   \code{filterdepth_na = TRUE}). If \code{FALSE} (the default), the returned
#'   object just distinguishes locations. In the latter case, the variables
#'   \code{obswell_installdate} and \code{obswell_stopdate} are not returned.
#' @param obswell_aggr String. Defines how the attributes of multiple
#'   observation wells per location that fulfill the \code{filterdepth_range}
#'   and \code{filterdepth_na} criteria (after filterdepth adjustment if
#'   \code{filterdepth_guess = TRUE}), are aggregated into one record
#'   \strong{per location}:
#'   \itemize{
#'   \item \code{"latest"}: return attributes of the most recent observation
#'      well that fulfills the \code{filterdepth_range} and
#'      \code{filterdepth_na} criteria;
#'   \item \code{"latest_fd"}: return attributes of the most recent observation
#'      well that fulfills the \code{filterdepth_range} condition, i.e.
#'      filterdepth will not be missing unless \emph{all} retained wells have
#'      missing filterdepth \emph{and} \code{filterdepth_na = TRUE};
#'   \item \code{"latest_sso"}: return attributes of the most recent observation
#'      well that fulfills the \code{filterdepth_range} and
#'      \code{filterdepth_na} criteria \emph{and} for which \code{soilsurf_ost}
#'      (soil surface level in the
#'      \href{http://crs.bkg.bund.de/crseu/crs/eu-description.php?crs_id=Y0JFX09PU1QrJTJGK1VOQ09S}{Ostend
#'      height} CRS (EPSG \href{https://epsg.io/5710}{5710}) is not missing
#'      (unless \emph{all} retained wells have missing \code{soilsurf_ost});
#'   \item \code{"mean"}: aggregation not by selecting an individual observation
#'      well, but by averaging the values of the associated variables
#'      \code{soilsurf_ost}, \code{measuringref_ost}, \code{tubelength},
#'      \code{filterlength}, \code{filterdepth} for the observation wells with
#'      non-missing values (different wells may be involved for each variable,
#'      depending on the distribution of missing values). With
#'      \code{filterdepth_guess = TRUE}, the extra variabele
#'      \code{filterdepth_guessed} is summarised as \code{TRUE} for a location
#'      if at least one of the location's observation wells has
#'      \code{filterdepth_guessed = TRUE}.
#'   }
#'   \strong{In all cases} the returned value of \code{obswell_statecode} and
#'   \code{obswell_state} corresponds to the \code{"latest"} approach. The
#'   \code{obswell_aggr} argument has no effect on locations with a single
#'   retained observation well. It is ignored if \code{obswells = TRUE}.
#' @param mask An optional geospatial filter of class \code{sf}. If provided,
#'   only locations that intersect with \code{mask} will be returned, with the
#'   value of \code{buffer} taken into account. The CRS must be Belgian Lambert
#'   72 (EPSG-code \href{https://epsg.io/31370}{31370}).
#' @param join_mask Logical. Do you want to spatially join the attribute columns
#'   of \code{mask} to the resulting tibble? The spatial join is executed with
#'   \code{\link[sf:geos_binary_pred]{st_intersects()}} as the topological
#'   operator. Beware: if the same location intersects with more than one
#'   element of \code{mask} (taking into account the value of \code{buffer}),
#'   that location will occur multiple times in the result. \code{join_mask} is
#'   ignored if \code{mask} is not provided.
#' @param buffer Number of meters taken as a buffer to enlarge \code{mask} (or
#'   shrink it, if \code{buffer < 0}) if \code{mask} is provided.
#' @param bbox Optional geospatial fiter (rectangle). A bounding box (class
#'   \code{bbox}), or a vector of four named elements \code{xmin}, \code{xmax},
#'   \code{ymin}, \code{ymax} defining the boundary coordinates of a bounding
#'   box. If provided, only locations within this rectangular area will be
#'   returned. The CRS must be Belgian Lambert 72 (EPSG-code
#'   \href{https://epsg.io/31370}{31370}).
#' @param area_codes An optional vector with area codes. If provided, only
#'   locations within the areas will be returned.
#' @param loc_type Type of the location (mainly: the type of measurement
#'   device). Defaults to \code{"P"}, i.e. only groundwater piezometers are
#'   returned by default. Can be a vector with multiple selected values.
#' @param loc_validity Validation status of the location. Can be a vector with
#'   multiple selected values, which must belong to \code{"VLD"}, \code{"ENT"},
#'   \code{"DEL"} or \code{"CLD"}. Defaults to \code{c("VLD", "ENT")}.
#' @param loc_vec An optional vector with location codes. If provided, only
#'   locations are returned that are present in this vector.
#' @param collect Should the data be retrieved as a local tibble? If
#'   \code{FALSE} (the default), a \code{tbl_lazy} object is returned (lazy
#'   query). Hence the result can be further built upon before retrieving data
#'   with \code{\link[dplyr:compute]{collect()}}.
#'
#' @return By default, a \code{tbl_lazy} object. With \code{collect = TRUE} or
#' with a specified \code{mask}, a local \code{\link[tibble]{tibble}} is
#' returned.
#'
#' (TO BE ADDED: Explanation on the variable names of the returned object)
#'
#' @family functions to query the data warehouse
#'
#' @md
#' @note Up to and including `watina 0.3.0`, the result was sorted according to
#' `area_code` and `loc_code`, both for the lazy query and the collected result.
#' Later versions avoid sorting in case of a lazy result, because otherwise,
#' when using the result inside another lazy query, this led to 'ORDER BY'
#' constructs in SQL subqueries, which must be avoided. If you like to print the
#' lazy object in a sorted manner, you must add `%>% arrange(...)` yourself.
#'
#' @examples
#' \dontrun{
#' watina <- connect_watina()
#'
#' library(dplyr)
#'
#' get_locs(watina,
#'   bbox = c(
#'     xmin = 1.4e+5,
#'     xmax = 1.7e+5,
#'     ymin = 1.6e+5,
#'     ymax = 1.9e+5
#'   )
#' ) %>%
#'   arrange(area_code, loc_code)
#'
#' get_locs(
#'   watina,
#'   area_codes = c("KAL", "KBR"),
#'   collect = TRUE
#' )
#'
#' get_locs(
#'   watina,
#'   area_codes = c("KAL", "KBR"),
#'   loc_type = c("P", "S"),
#'   collect = TRUE
#' )
#'
#' get_locs(
#'   watina,
#'   area_codes = "WES"
#' ) %>%
#'   count()
#'
#' get_locs(
#'   watina,
#'   area_codes = "WES",
#'   filterdepth_guess = TRUE
#' ) %>%
#'   count()
#'
#' get_locs(
#'   watina,
#'   area_codes = c("KAL", "KBR"),
#'   loc_type = c("P", "S"),
#'   filterdepth_na = TRUE,
#'   collect = TRUE
#' )
#'
#' # Mark the different output of:
#' get_locs(
#'   watina,
#'   loc_vec = c("KBRP081", "KBRP090", "KBRP095", "KBRS001"),
#'   loc_type = c("P", "S"),
#'   collect = TRUE
#' )
#' # versus:
#' get_locs(
#'   watina,
#'   loc_vec = c("KBRP081", "KBRP090", "KBRP095", "KBRS001"),
#'   collect = TRUE
#' )
#'
#' # Returning all individual observation wells:
#' get_locs(
#'   watina,
#'   obswells = TRUE,
#'   area_codes = c("KAL", "KBR"),
#'   loc_type = c("P", "S"),
#'   collect = TRUE
#' )
#'
#' # Different examples of aggregating observation wells at location level:
#' get_locs(
#'   watina,
#'   area_codes = "WES",
#'   filterdepth_na = TRUE,
#'   filterdepth_guess = TRUE,
#'   obswell_aggr = "latest",
#'   collect = TRUE
#' ) %>%
#'   select(loc_code, contains("ost"), contains("filterdepth")) %>%
#'   head(12)
#'
#' get_locs(
#'   watina,
#'   area_codes = "WES",
#'   filterdepth_na = TRUE,
#'   filterdepth_guess = TRUE,
#'   obswell_aggr = "mean",
#'   collect = TRUE
#' ) %>%
#'   select(loc_code, contains("ost"), contains("filterdepth")) %>%
#'   head(12)
#'
#' # Selecting all piezometers with status VLD of the
#' # province "West-Vlaanderen" (current polygon taken
#' # from the official WFS service):
#' library(sf)
#' library(purrr)
#' library(httr)
#' mymask <-
#'   "https://geo.api.vlaanderen.be/VRBG/wfs" %>%
#'   parse_url() %>%
#'   list_merge(query = list(
#'     request = "GetFeature",
#'     typeName = "VRBG:Refprv",
#'     cql_filter = "NAAM='West-Vlaanderen'",
#'     srsName = "EPSG:31370",
#'     outputFormat = "text/xml; subtype=gml/3.1.1"
#'   )) %>%
#'   build_url() %>%
#'   read_sf(crs = 31370) %>%
#'   st_cast("GEOMETRYCOLLECTION")
#' get_locs(
#'   watina,
#'   loc_validity = "VLD",
#'   mask = mymask,
#'   buffer = 0
#' )
#'
#' # Disconnect:
#' dbDisconnect(watina)
#' }
#'
#' @export
#' @importFrom rlang .data
#' @importFrom assertthat assert_that is.number is.flag noNA
#' @importFrom dplyr %>% tbl filter left_join select distinct arrange group_by
#'   ungroup sql
# MAIN FUNCTION GET LOCS -------------------------------------------------------
get_locs <- function(con,
                     filterdepth_range = c(0, 3),
                     filterdepth_guess = FALSE,
                     filterdepth_na = FALSE,
                     obswells = FALSE,
                     obswell_aggr = c(
                       "latest",
                       "latest_fd",
                       "latest_sso",
                       "mean"
                     ),
                     mask = NULL,
                     join_mask = FALSE,
                     buffer = 10,
                     bbox = NULL,
                     area_codes = NULL,
                     loc_type = c("P", "S", "R", "N", "W", "D", "L", "B"),
                     loc_validity = c("VLD", "ENT"),
                     loc_vec = NULL,
                     collect = FALSE) {
  if (missing(obswell_aggr)) obswell_aggr <- match.arg(obswell_aggr)
  if (missing(loc_type)) loc_type <- match.arg(loc_type)

  validate_input(
    filterdepth_range,
    filterdepth_guess,
    filterdepth_na,
    obswells,
    obswell_aggr,
    mask,
    join_mask,
    buffer,
    bbox,
    area_codes,
    loc_type,
    loc_validity,
    loc_vec,
    collect
  )

  meetpunt <- tbl(con, "vwDimMeetpunt")
  gebied <- tbl(con, "vwDimGebied")
  peilpunt <- tbl(con, "vwDimPeilpunt")

  locations <- meetpunt %>%
    join_area_metadata(gebied) %>%
    filter_locations(
      bbox,
      area_codes,
      loc_type,
      loc_validity,
      loc_vec
    )
  observation_wells <- peilpunt %>%
    process_observation_wells()

  locs <- locations %>%
    left_join(observation_wells, by = "MeetpuntWID") %>%
    compute_observation_metrics() %>%
    select(
      loc_wid = .data$MeetpuntWID,
      loc_code = .data$MeetpuntCode,
      area_code = .data$GebiedCode,
      area_name = .data$GebiedNaam,
      x = .data$MeetpuntXCoordinaat,
      y = .data$MeetpuntYCoordinaat,
      loc_validitycode = .data$MeetpuntStatusCode,
      loc_validity = .data$MeetpuntStatus,
      loc_typecode = .data$MeetpuntTypeCode,
      loc_typename = .data$MeetpuntType,
      obswell_code = .data$PeilpuntCode,
      obswell_rank = .data$PeilpuntVersie,
      obswell_statecode = .data$PeilpuntToestandCode,
      obswell_state = .data$PeilpuntToestandNaam,
      obswell_installdate = .data$PeilpuntPlaatsing,
      obswell_stopdate = .data$PeilpuntStopzetting,
      .data$soilsurf_ost,
      measuringref_ost = .data$ReferentieNiveauTAW,
      .data$tubelength,
      .data$filterlength,
      .data$filterdepth
    ) %>%
    estimate_filterdepth(
      filterdepth_range,
      filterdepth_guess,
      filterdepth_na
    )

  if (!obswells) {
    locs <-
      locs %>%
      aggregate_observations(obswell_aggr)
  }

  if (!is.null(mask)) {
    locs <-
      locs %>%
      filter_by_spatial_mask(mask, join_mask, buffer)
  }

  if (collect & is.null(mask)) {
    locs <-
      locs %>%
      select(-.data$loc_wid) %>%
      collect() %>%
      arrange(
        .data$area_code,
        .data$loc_code
      )
  }

  if (inherits(locs, "data.frame")) {
    warn_xy_duplicates(locs$x, locs$y)
  }

  return(locs)
}

# HELPER FUNCTIONS GET LOCS ----------------------------------------------------
validate_input <- function(
    filterdepth_range,
    filterdepth_guess,
    filterdepth_na,
    obswells,
    obswell_aggr,
    mask,
    join_mask,
    buffer,
    bbox,
    area_codes,
    loc_type,
    loc_validity,
    loc_vec,
    collect
) {
  assert_that(
    is.numeric(filterdepth_range),
    length(filterdepth_range) == 2,
    filterdepth_range[1] <= filterdepth_range[2]
  )

  assert_that(is.number(buffer))

  assert_that(
    is.null(bbox) | all(sort(names(bbox)) == c("xmax", "xmin", "ymax", "ymin")),
    msg = "You did not correctly specify bbox."
  )
  assert_that(is.null(area_codes) | all(is.character(area_codes)))
  assert_that(
    is.null(loc_vec) | all(is.character(loc_vec)),
    msg = "loc_vec must be a character vector."
  )
  assert_that(is.flag(join_mask), assertthat::noNA(join_mask))
  assert_that(is.flag(collect), assertthat::noNA(collect))
  assert_that(is.flag(obswells), assertthat::noNA(obswells))
  assert_that(is.flag(filterdepth_guess), assertthat::noNA(filterdepth_guess))
  assert_that(is.flag(filterdepth_na), assertthat::noNA(filterdepth_na))

  if (!is.null(mask) & !collect) {
    message("As a mask always invokes a collect(), the argument 'collect = FALSE' will be ignored.")
  }

  if (!is.null(mask)) {
    assert_that(
      inherits(mask, "sf"),
      msg = "mask must be an sf object."
    )
    require_pkgs("sf")
    assert_that(
      sf::st_crs(mask) == sf::st_crs(31370),
      msg = "The CRS of mask must be Belgian Lambert 72 (EPSG-code 31370)."
    )
  }

  if (!is.null(bbox)) {
    assert_that(
      bbox["xmax"] >= bbox["xmin"],
      bbox["ymax"] >= bbox["ymin"]
    )
  }

  if (missing(loc_type)) {
    loc_type <- match.arg(loc_type)
  } else {
    assert_that(
      all(loc_type %in% c("P", "S", "R", "N", "W", "D", "L", "B")),
      msg = "You specified at least one unknown loc_type."
    )
  }

  assert_that(
    all(loc_validity %in% c("VLD", "ENT", "DEL", "CLD")),
    msg = "You specified at least one unknown loc_validity."
  )
}

join_area_metadata <- function(meetpunt, gebied) {
  gebied_filtered <- gebied %>%
    select(
      .data$GebiedWID,
      .data$GebiedCode,
      .data$GebiedNaam
    )

  locs <- meetpunt %>%
    left_join(
      gebied_filtered,
      by = "GebiedWID"
    )

  return(locs)
}

filter_by_location_code <- function(locs, loc_vec) {
  locs %>%
    filter(.data$MeetpuntCode %in% loc_vec)
}

filter_by_area_code <- function(locs, area_codes) {
  locs %>%
    filter(.data$GebiedCode %in% area_codes)
}

filter_by_bbox <- function(locs, bbox) {
  bbox_xmin <- unname(bbox["xmin"])
  bbox_xmax <- unname(bbox["xmax"])
  bbox_ymin <- unname(bbox["ymin"])
  bbox_ymax <- unname(bbox["ymax"])

  locs %>%
    filter(
      .data$MeetpuntXCoordinaat >= bbox_xmin,
      .data$MeetpuntXCoordinaat <= bbox_xmax,
      .data$MeetpuntYCoordinaat >= bbox_ymin,
      .data$MeetpuntYCoordinaat <= bbox_ymax
    )
}

filter_locations <- function(
    locs,
    bbox,
    area_codes,
    loc_type,
    loc_validity,
    loc_vec
) {
  locs <- locs %>%
    filter(
      .data$MeetpuntTypeCode %in% loc_type,
      .data$MeetpuntStatusCode %in% loc_validity
    )

  if (!is.null(loc_vec)) locs <- locs %>% filter_by_location_code(loc_vec)
  if (!is.null(area_codes)) locs <- locs %>% filter_by_area_code(area_codes)
  if (!is.null(bbox)) locs <- locs %>% filter_by_bbox(bbox)

  return(locs)
}

process_observation_wells <- function(observations) {
  observations %>%
    mutate(
      PeilpuntPlaatsing =
        sql("CAST(PeilpuntPlaatsing AS date)"),
      PeilpuntStopzetting =
        sql("CAST(PeilpuntStopzetting AS date)")
    ) %>%
    filter(
      .data$PeilpuntStatusCode %in% c(
        "VLD",
        "ENT",
        "CLD"
      ),
      .data$PeilpuntOpenbaarheidTypeCode == "PLME",
      .data$PeilpuntOpenbaarheidCode == "UNKWN"
    )
}

compute_observation_metrics <- function(locs) {
  locs <- locs %>%
    mutate(
      tubelength = ifelse(
        .data$PeilbuisLengte <= 0,
        NA,
        .data$PeilbuisLengte
      ),
      filterlength = ifelse(
        is.na(.data$FilterLengte) | .data$FilterLengte == 0,
        0.3,
        .data$FilterLengte
      ),
      filterdepth = .data$tubelength -
        .data$ReferentieNiveauMaaiveld -
        .data$filterlength / 2,
      soilsurf_ost =
        .data$ReferentieNiveauTAW -
        .data$ReferentieNiveauMaaiveld
    )
}

add_filterdepth_estimation_flag <- function(locs) {
  locs <-
    locs %>%
    mutate(
      filterdepth_guessed =
        is.na(.data$filterdepth) & !is.na(.data$tubelength),
      filterdepth = ifelse(
        .data$filterdepth_guessed == 1,
        # (sql: logical stored as bit)
        .data$tubelength - .data$filterlength / 2,
        .data$filterdepth
      )
    )

  return(locs)
}

estimate_filterdepth <- function(
    locs,
    filterdepth_range,
    filterdepth_guess,
    filterdepth_na
) {
  if (filterdepth_guess) locs <- locs %>% add_filterdepth_estimation_flag()

  min_filterdepth <- filterdepth_range[1]
  max_filterdepth <- filterdepth_range[2]

  if (filterdepth_na) {
    locs <-
      locs %>%
      filter(
        (.data$loc_typecode == "P" &
           (.data$filterdepth <= max_filterdepth &
              .data$filterdepth >= min_filterdepth |
              is.na(.data$filterdepth))) |
          .data$loc_typecode != "P"
      )
  } else {
    locs <-
      locs %>%
      filter(
        .data$loc_typecode == "P" &
          .data$filterdepth <= max_filterdepth &
          .data$filterdepth >= min_filterdepth |
          .data$loc_typecode != "P"
      )
  }

  return(locs)
}

compute_observation_aggregations <- function(locs) {
  locs %>%
    group_by(.data$loc_code) %>%
    mutate(
      obswell_count = n(),
      obswell_maxrank = max(.data$obswell_rank, na.rm = TRUE),
      obswell_maxrank_fd =
        max(
          ifelse(is.na(.data$filterdepth), NA, .data$obswell_rank),
          na.rm = TRUE
        ),
      obswell_maxrank_sso =
        max(
          ifelse(is.na(.data$soilsurf_ost), NA, .data$obswell_rank),
          na.rm = TRUE
        ),
      obswell_statecode =
        max(
          ifelse(.data$obswell_rank == .data$obswell_maxrank, .data$obswell_statecode, NA),
          na.rm = TRUE
        ),
      obswell_state =
        max(
          ifelse(.data$obswell_rank == .data$obswell_maxrank, .data$obswell_state, NA),
          na.rm = TRUE
        )
    )
}

aggregate_guessed_flags <- function(locs) {
  # If the column doesn't exist, just pass the data through untouched
  if (!("filterdepth_guessed" %in% colnames(locs))) {
    return(locs)
  }

  locs %>%
    mutate(
      filterdepth_guessed =
        max(
          # (sql: logical stored as bit)
          ifelse(.data$filterdepth_guessed == 1, 1, 0),
          na.rm = TRUE
        )
    ) %>%
    mutate(
      filterdepth_guessed = sql("CAST(filterdepth_guessed AS bit)")
    )
}

aggregate_by_strategy <- function(locs, obswell_aggr) {
  switch(obswell_aggr,
         "latest" =
           locs %>%
           ungroup() %>%
           filter(
             .data$obswell_count == 1 |
               .data$obswell_rank == .data$obswell_maxrank
           ),
         "latest_fd" =
           locs %>%
           ungroup() %>%
           filter(
             .data$obswell_count == 1 |
               (.data$obswell_rank ==
                  .data$obswell_maxrank_fd) |
               (is.na(.data$obswell_maxrank_fd) &
                  (.data$obswell_rank ==
                     .data$obswell_maxrank))
           ),
         "latest_sso" =
           locs %>%
           ungroup() %>%
           filter(
             .data$obswell_count == 1 |
               (.data$obswell_rank ==
                  .data$obswell_maxrank_sso) |
               (is.na(.data$obswell_maxrank_sso) &
                  (.data$obswell_rank ==
                     .data$obswell_maxrank))
           ),
         "mean" =
           locs %>%
           mutate(
             soilsurf_ost = mean(.data$soilsurf_ost, na.rm = TRUE),
             measuringref_ost = mean(.data$measuringref_ost, na.rm = TRUE),
             filterdepth = mean(.data$filterdepth, na.rm = TRUE),
             filterlength = mean(.data$filterlength, na.rm = TRUE),
             tubelength = mean(.data$tubelength, na.rm = TRUE)
           ) %>%
           aggregate_guessed_flags() %>%
           ungroup() %>%
           filter(
             .data$obswell_count == 1 |
               .data$obswell_rank == .data$obswell_maxrank
           )
  )
}

drop_observation_metadata <- function(locs) {
  locs %>%
    select(
      -.data$obswell_code,
      -.data$obswell_rank,
      -.data$obswell_installdate,
      -.data$obswell_stopdate,
      -.data$obswell_count,
      -.data$obswell_maxrank,
      -.data$obswell_maxrank_fd,
      -.data$obswell_maxrank_sso
    )
}

aggregate_observations <- function(locs, obswell_aggr) {
  locs %>%
    compute_observation_aggregations() %>%
    aggregate_by_strategy(obswell_aggr) %>%
    drop_observation_metadata()
}

expand_mask <- function(mask, buffer) {
  if (buffer == 0) return(mask)
  sf::st_buffer(mask, dist = buffer)
}

execute_spatial_filter <- function(locs, mask, join_mask) {
  if (join_mask) {
    locs <- locs %>%
      sf::st_join(mask, left = FALSE)
  } else {
    locs <- locs %>%
      .[mask, ]
  }
  return(sf::st_drop_geometry(locs))
}

filter_by_spatial_mask <- function(locs, mask, join_mask, buffer) {
  locs <-
    locs %>%
    select(-.data$loc_wid) %>%
    collect() # SF (Spatial Features) filters not possible on database engines

  nr_dropped_locs <-
    locs %>%
    filter(is.na(.data$x) | is.na(.data$y)) %>%
    count() %>%
    .$n

  if (nr_dropped_locs > 0) {
    warning(
      "Dropped ",
      nr_dropped_locs,
      " locations from which x or y coordinates were missing.\n"
    )
  }

  locs <-
    locs %>%
    filter(!is.na(.data$x), !is.na(.data$y)) %>%
    arrange(.data$area_code, .data$loc_code) %>%
    as_points(warn_dupl = FALSE)

  mask_expand <- expand_mask(mask, buffer)

  locs <- locs %>% execute_spatial_filter(mask_expand, join_mask)

  return(locs)
}
