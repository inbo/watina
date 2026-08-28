# DOOCUMENTATION GET LOCS ------------------------------------------------------
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
#' @param loc_validity Validation status of the location. The new DWH only
#'   contains records with status \code{"VLD"}. This argument is deprecated and
#'   will be removed in a future version.
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
#' @importFrom lifecycle deprecated
# FUNCTION GET LOCS ------------------------------------------------------------
get_locs <- function(
  con,
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
  loc_validity = "VLD",
  loc_vec = NULL,
  collect = FALSE
) {
  if (!missing(loc_validity) && !identical(loc_validity, "VLD")) {
    lifecycle::deprecate_warn(
      when = "1.0.0",
      what = "get_locs(loc_validity)",
      details = paste(
        "The new DWH only contains records with validity status 'VLD'. Your",
        "input will be ignored. This argument will be removed in a future",
        "version."
      )
    )
  }

  loc_validity <- "VLD"

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

  obswell_aggr <- match.arg(obswell_aggr)

  if (!is.null(mask) & !collect) {
    message(
      "As a mask always invokes a collect(), the argument 'collect = FALSE' will be ignored."
    )
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

  locs <- build_locs_query(
    con = con,
    bbox = bbox,
    area_codes = area_codes,
    loc_type = loc_type,
    loc_validity = loc_validity,
    loc_vec = loc_vec
  )

  min_filterdepth <- filterdepth_range[1]
  max_filterdepth <- filterdepth_range[2]

  if (filterdepth_guess) {
    locs <-
      locs %>%
      mutate(
        filterdepth_guessed = is.na(.data$filterdepth) &
          !is.na(.data$tubelength),
        filterdepth = ifelse(
          .data$filterdepth_guessed == 1,
          # (sql: logical stored as bit)
          .data$tubelength - .data$filterlength / 2,
          .data$filterdepth
        )
      )
  }

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

  if (!obswells) {
    locs <-
      locs %>%
      group_by(.data$loc_code) %>%
      mutate(
        obswell_count = n(),
        obswell_maxrank = max(.data$obswell_rank, na.rm = TRUE),
        obswell_maxrank_fd = max(
          ifelse(
            is.na(.data$filterdepth),
            NA,
            .data$obswell_rank
          ),
          na.rm = TRUE
        ),
        obswell_maxrank_sso = max(
          ifelse(
            is.na(.data$soilsurf_ost),
            NA,
            .data$obswell_rank
          ),
          na.rm = TRUE
        ),
        obswell_statecode = max(
          ifelse(
            .data$obswell_rank == .data$obswell_maxrank,
            .data$obswell_statecode,
            NA
          ),
          na.rm = TRUE
        ),
        obswell_state = max(
          ifelse(
            .data$obswell_rank == .data$obswell_maxrank,
            .data$obswell_state,
            NA
          ),
          na.rm = TRUE
        )
      )

    locs <-
      switch(
        obswell_aggr,
        "latest" = locs %>%
          ungroup() %>%
          filter(
            .data$obswell_count == 1 |
              .data$obswell_rank == .data$obswell_maxrank
          ),
        "latest_fd" = locs %>%
          ungroup() %>%
          filter(
            .data$obswell_count == 1 |
              (.data$obswell_rank == .data$obswell_maxrank_fd) |
              (is.na(.data$obswell_maxrank_fd) &
                (.data$obswell_rank == .data$obswell_maxrank))
          ),
        "latest_sso" = locs %>%
          ungroup() %>%
          filter(
            .data$obswell_count == 1 |
              (.data$obswell_rank == .data$obswell_maxrank_sso) |
              (is.na(.data$obswell_maxrank_sso) &
                (.data$obswell_rank == .data$obswell_maxrank))
          ),
        "mean" = locs %>%
          mutate(
            soilsurf_ost = mean(.data$soilsurf_ost, na.rm = TRUE),
            measuringref_ost = mean(.data$measuringref_ost, na.rm = TRUE),
            filterdepth = mean(.data$filterdepth, na.rm = TRUE),
            filterlength = mean(.data$filterlength, na.rm = TRUE),
            tubelength = mean(.data$tubelength, na.rm = TRUE)
          ) %>%
          {
            if ("filterdepth_guessed" %in% colnames(.)) {
              mutate(
                .,
                filterdepth_guessed = max(
                  ifelse(
                    .data$filterdepth_guessed == 1,
                    # (sql: logical stored as bit)
                    1,
                    0
                  ),
                  na.rm = TRUE
                )
              ) %>%
                mutate(
                  filterdepth_guessed = sql("CAST(filterdepth_guessed AS bit)")
                )
            } else {
              .
            }
          } %>%
          ungroup() %>%
          filter(
            .data$obswell_count == 1 |
              .data$obswell_rank == .data$obswell_maxrank
          )
      ) %>%
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

  if (!is.null(mask)) {
    locs <-
      locs %>%
      select(-.data$loc_wid) %>%
      collect()

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
      arrange(
        .data$area_code,
        .data$loc_code
      ) %>%
      as_points(warn_dupl = FALSE)

    if (buffer != 0) {
      mask_expand <-
        mask %>%
        sf::st_buffer(dist = buffer)
    } else {
      mask_expand <-
        mask
    }

    if (join_mask) {
      locs <-
        locs %>%
        sf::st_join(mask_expand, left = FALSE) %>%
        sf::st_drop_geometry()
    } else {
      locs <-
        locs %>%
        .[mask_expand, ] %>%
        sf::st_drop_geometry()
    }
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

build_locs_query <- function(
  con,
  bbox,
  area_codes,
  loc_type,
  loc_validity,
  loc_vec
) {
  locs <-
    tbl(con, "DimMeetpunt") %>%
    filter(
      .data$MeetpuntTypeCode %in% loc_type,
      .data$MeetpuntStatusCode %in% loc_validity
    ) %>%
    left_join(
      tbl(con, "DimGebied") %>%
        select(
          .data$GebiedWID,
          .data$GebiedCode,
          .data$GebiedNaam
        ),
      by = "GebiedWID"
    )

  if (!is.null(loc_vec)) {
    locs <-
      locs %>%
      filter(.data$MeetpuntCode %in% loc_vec)
  }

  if (!is.null(area_codes)) {
    locs <-
      locs %>%
      filter(.data$GebiedCode %in% area_codes)
  }

  if (!is.null(bbox)) {
    bbox_xmin <- unname(bbox["xmin"])
    bbox_xmax <- unname(bbox["xmax"])
    bbox_ymin <- unname(bbox["ymin"])
    bbox_ymax <- unname(bbox["ymax"])
    locs <-
      locs %>%
      filter(
        .data$MeetpuntXCoordinaat >= bbox_xmin,
        .data$MeetpuntXCoordinaat <= bbox_xmax,
        .data$MeetpuntYCoordinaat >= bbox_ymin,
        .data$MeetpuntYCoordinaat <= bbox_ymax
      )
  }

  locs <-
    locs %>%
    left_join(
      tbl(con, "DimPeilpunt") %>%
        mutate(
          PeilpuntPlaatsing = sql("CAST(PeilpuntPlaatsing AS date)"),
          PeilpuntStopzetting = sql("CAST(PeilpuntStopzetting AS date)")
        ),
      by = "MeetpuntWID"
    ) %>%
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
      soilsurf_ost = .data$ReferentieNiveauTAW -
        .data$ReferentieNiveauMaaiveld
    ) %>%
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
    )

  return(locs)
}

# DOOCUMENTATION GET XG3 -------------------------------------------------------
#' Get XG3 values from the data warehouse
#'
#' Returns XG3 values from the \emph{Watina} data warehouse, either as a lazy
#' object or as a local tibble. The values must belong to selected locations and
#' to a specified timeframe.
#'
#' The timeframe is a selection interval between a given first and last
#' hydroyear.
#'
#' (TO BE ADDED: What are XG3 values? What is a hydroyear? Why truncate, and why
#' truncate by default? When to choose which \code{vert_crs}?)
#'
#' @param locs A \code{tbl_lazy} object or a data frame, with at least a column
#'   \code{loc_code} that defines the locations for which values are to be
#'   returned. Typically, this will be the object returned by
#'   \code{\link{get_locs}}.
#' @param con A database connection object to the Watina data warehouse.
#' @param startyear First hydroyear of the timeframe.
#' @param endyear Last hydroyear of the timeframe. Defaults to the previous calendar year.
#' @param vert_crs A string, defining the 1-dimensional vertical coordinate
#'   reference system (CRS) of the XG3 water levels. Either \code{"local"} (the
#'   default, i.e. returned values are relative to soil surface level, with
#'   positive values = above soil surface), or \code{"ostend"} (values are from
#'   the CRS
#'   \href{http://crs.bkg.bund.de/crseu/crs/eu-description.php?crs_id=Y0JFX09PU1QrJTJGK1VOQ09S}{Ostend
#'   height} (EPSG \href{https://epsg.io/5710}{5710}), also known as 'TAW' or
#'   'DNG'), or \code{"both"}, where the values for both CRS options are
#'   returned. The units are always meters.
#' @param truncated Logical. If \code{TRUE}, the XG3 values are calculated
#'   after having capped the underlying water level measurements that are above
#'   soil surface level to the soil surface level itself (zero in the local CRS).
#'   Defaults to \code{FALSE}.
#' @param with_estimated Logical. If \code{TRUE}, the XG3 calculation includes
#'   estimated or simulated (e.g. Menyanthes model) water level data alongside
#'   raw measurements. Defaults to \code{FALSE}.
#' @param collect Logical. If \code{TRUE}, the lazy query is executed and
#'   brought into local R memory as a tibble. Defaults to \code{FALSE}.
#'
#' @inheritParams get_locs
#'
#' @return By default, a \code{tbl_lazy} object. With \code{collect = TRUE}, a
#' local \code{\link[tibble]{tibble}} is returned.
#'
#' (TO BE ADDED: Explanation on the variable names of the returned object)
#'
#' The suffix of the XG3 variables is either "\code{_lcl}" for \code{vert_crs =
#' "local"} or "\code{_ost}" for \code{vert_crs = "ostend"}.
#'
#' @family functions to query the data warehouse
#'
#' @md
#' @note Up to and including `watina 0.3.0`, the result was sorted according to
#' `loc_code` and `hydroyear`, both for the lazy query and the collected result.
#' Later versions avoid sorting in case of a lazy result, because otherwise,
#' when using the result inside another lazy query, this led to 'ORDER BY'
#' constructs in SQL subqueries, which must be avoided. If you like to print the
#' lazy object in a sorted manner, you must add `%>% arrange(...)` yourself.
#'
#' @examples
#' \dontrun{
#' watina <- connect_watina()
#' library(dplyr)
#' mylocs <- get_locs(watina, area_codes = "KAL")
#' mylocs %>%
#'   get_xg3(watina, 2010) %>%
#'   arrange(loc_code, hydroyear)
#' mylocs %>% get_xg3(watina, 2010, collect = TRUE)
#' mylocs %>%
#'   get_xg3(watina, 2010, vert_crs = "ostend") %>%
#'   arrange(loc_code, hydroyear)
#'
#' # joining results to mylocs:
#' mylocs %>%
#'   get_xg3(watina, 2010) %>%
#'   left_join(
#'     mylocs %>%
#'       select(-loc_wid),
#'     .
#'   ) %>%
#'   collect() %>%
#'   arrange(loc_code, hydroyear)
#'
#' # Disconnect:
#' dbDisconnect(watina)
#' }
#'
#' @export
#' @importFrom assertthat assert_that is.number is.flag noNA
#' @importFrom rlang .data
#' @importFrom lubridate year now
#' @importFrom dplyr %>% copy_to filter left_join inner_join select contains
#'   arrange distinct
# FUNCTION GET XG3 -------------------------------------------------------------
get_xg3 <- function(
  locs,
  con,
  startyear,
  endyear = year(now()) - 1,
  vert_crs = c("local", "ostend", "both"),
  truncated = FALSE,
  with_estimated = FALSE,
  collect = FALSE
) {
  vert_crs <- match.arg(vert_crs)
  assert_that(is.number(startyear))
  assert_that(is.number(endyear))
  assert_that(
    endyear >= startyear,
    msg = "startyear must not be larger than endyear."
  )
  assert_that(
    "loc_code" %in% colnames(locs),
    msg = "locs does not have a column name 'loc_code'."
  )
  assert_that(is.flag(truncated), assertthat::noNA(truncated))
  assert_that(is.flag(collect), assertthat::noNA(collect))

  xg3 <- build_xg3_query(
    locs,
    con,
    startyear,
    endyear,
    truncated,
    with_estimated
  )

  xg3 <-
    switch(
      vert_crs,
      local = xg3 %>% select(-contains("ost")),
      ostend = xg3 %>% select(-contains("lcl")),
      both = xg3
    )

  if (collect) {
    xg3 <-
      xg3 %>%
      arrange(
        .data$loc_code,
        .data$hydroyear
      ) %>%
      collect()
  }

  return(xg3)
}

build_xg3_query <- function(
  locs,
  con,
  startyear,
  endyear,
  truncated,
  with_estimated
) {
  if (inherits(locs, "data.frame")) {
    locs <- stage_locs(locs, con)
  }

  param_query <- tbl(con, "DimParameterSet") %>%
    filter(
      .data$MinMetingen > 0,
      .data$IsAfgetopt == truncated,
      .data$IsEstimated == with_estimated
    )

  matching_params <- param_query %>% collect()
  param_count <- nrow(matching_params)

  str_settings <- sprintf(
    "settings truncated = '%s' and with_estimated = '%s'",
    truncated,
    with_estimated
  )

  if (param_count == 0) {
    stop(
      sprintf(
        "No parameters found with %s",
        str_settings
      ),
      call. = FALSE
    )
  }
  if (param_count > 1) {
    str_param_options <- paste0(matching_params$Omschrijving, collapse = ", ")

    selected_id <- max((matching_params$ParameterSetWID))
    selected_option <- matching_params$Omschrijving[
      matching_params$ParameterSetWID == selected_id
    ]

    message(sprintf(
      "Multiple parameter options found with %s. The options are: %s. Option '%s' selected. Contact package maintainer if another parameter should be selected and extra filters are needed. Maybe create your first issue?",
      str_settings,
      str_param_options,
      selected_option
    ))

    param_query <- param_query %>%
      filter(.data$ParameterSetWID == selected_id)
  }

  xg3 <-
    tbl(con, "FactBRPeilMetingJaar") %>%
    inner_join(
      param_query,
      by = "ParameterSetWID"
    ) %>%
    filter(
      # CRITICAL: Keep only Hydroyears
      .data$IsHydroJaar == 1,
      .data$Jaar >= startyear,
      .data$Jaar <= endyear
    ) %>%
    select(
      loc_wid = .data$MeetpuntWID,
      hydroyear = .data$Jaar,
      lg3_lcl = .data$LGmMaaiVeldValue,
      hg3_lcl = .data$HGmMaaiVeldValue,
      vg3_lcl = .data$VGmMaaiVeldValue,
      lg3_ost = .data$LGmTAWValue,
      hg3_ost = .data$HGmTAWValue,
      vg3_ost = .data$VGmTAWValue
    ) %>%
    inner_join(
      locs %>%
        select(
          .data$loc_wid,
          .data$loc_code
        ) %>%
        distinct(),
      .,
      by = "loc_wid"
    ) %>%
    select(-.data$loc_wid)

  return(xg3)
}

# DOOCUMENTATION GET CHEM ------------------------------------------------------
#' Get hydrochemical data from the data warehouse
#'
#' Returns hydrochemical data from the \emph{Watina} data warehouse, either as a
#' lazy object or as a local tibble. The values must belong to selected
#' locations and to a specified timeframe.
#'
#' The timeframe is a selection interval between a given \code{startdate} and
#' \code{enddate}.
#'
#' The water samples must meet a specified electroneutrality condition, set by
#' \code{en_range}.
#'
#' \itemize{
#' \item This condition is however ignored when the sample's iron (meq/l) /
#'    conductivity (µS/cm) ratio exceeds \code{en_fecond_threshold} (use
#'    \code{en_fecond_threshold = NA} if you don't want this to happen).
#' \item Further, water samples are included by default if their
#'    electroneutrality is \code{NA} (this is controlled by the
#'    \code{en_exclude_na} argument).
#' \item Finally, please note that measurements of non-ion variables are
#'    \emph{always} returned!
#' }
#' To retrieve all data from all water samples, use \code{en_range = c(-1, 1)}.
#'
#' **More information about the electroneutrality**
#'
#' We expect groundwater samples to have no net charge, i.e. the total positive
#' charge from cations must equal the total negative charge from anions. To
#' ensure this is true (if we ignore the inevitable margin of error in the
#' laboratory), we calculate:
#' \itemize{
#' \item the sum of charges from anions (AN) in the sample as
#'    *HCO3 + SO4 + PO4 + Cl + NO3 + NO2*
#' \item the sum of charges from cations (CAT) in the sample as
#'    *Ca + Mg + Na + K + Fe + NH4*
#' }
#'
#' Then we derive the electroneutrality as *(CAT - AN)/(CAT + AN)*
#'
#' If significant deviation from zero occurs, there must be analytical errors in
#' the concentration determinations or ions at significant concentration levels
#' that were not included in the analysis.
#'
#' The \code{get_chem()} function allows for a standard tolerance of +/-0.1 for
#' the electroneutrality. This value can be adapted using the \code{en_range}
#' argument.
#'
#' @param startdate First date of the timeframe, as a string. The string must
#'   use a formatting of the order 'day month year', i.e. a format which can be
#'   interpreted by \code{\link[lubridate:ymd]{dmy}}.
#'
#'   Examples: \code{"16-1-2005"}, \code{"16-01-2005"}, \code{"1-01-2005"},
#'   \code{"16/1/2005"}, \code{"16/1/05"}, \code{"16/1/88"} (years 69 and higher
#'   are regarded as 19xy), \code{"16/1-2005"}, \code{"23 Oct 99"}, \code{"23
#'   Okt 99"} (supposing this notation follows your system locale), \code{"16
#'   1-!!-2005"}, ......
#' @param enddate Last date of the timeframe, as a string. The same formatting
#'   rule must be applied as in \code{startdate}. Defaults to a string
#'   representation of the current system date.
#' @param conc_type A string defining the type of concentration in \emph{ionic
#'   concentration variables}. Either:
#' \itemize{
#' \item{\code{"mass"}:} mass concentration (the default);
#' \item{\code{"eq"}:} equivalent concentration (= normality), referring to the
#'    electrical charge of the dissolved ion's main natural form.
#' }
#'   Note that the argument has no effect on the value of non-ion-variables.
#' @param en_range Numeric vector of length 2. Specifies the allowed range of
#'   water sample electroneutrality for ion-variable measurements (see Details).
#'   Both vector elements must be within the range \code{c(-1, 1)}, with the
#'   second element not being smaller than the first. Note that this argument
#'   only affects the selection of water samples for ionic concentration
#'   variables, not for non-ion variables such as pH and electrical
#'   conductivity. Measurements of non-ion variables are always returned.
#' @param en_exclude_na Logical. Should ion-variable measurements of water
#'   samples with missing electroneutrality value be omitted? Defaults to FALSE.
#'   A missing electroneutrality value is the consequence of one or more missing
#'   values of ionic concentration variables that are needed for
#'   electroneutrality calculation of the water sample. Note that this argument
#'   has no effect on the selection of non-ion variable measurements, which are
#'   always returned.
#' @param en_fecond_threshold A number (with a sensible default). May be set to
#'   \code{NA} or \code{NULL} by the user.
#'   \itemize{
#'   \item If \code{en_fecond_threshold} is a number (numeric scalar), all
#'      measurements from water samples with an iron (meq/l) / conductivity
#'      (µS/cm) ratio (\code{Fe/CondL}) equal to or larger than
#'      \code{en_fecond_threshold} are returned, regardless of the
#'      \code{en_range} and \code{en_exclude_na} arguments.
#'   \item If \code{en_fecond_threshold} is set to \code{NA} or \code{NULL}, the
#'      iron / conductivity ratio is ignored. Hence, no exceptions are made to
#'      the conditions imposed by \code{en_range} and \code{en_exclude_na}
#'      (except for measurements of non-ion variables, which are always
#'      returned).
#'   }
#'
#' @inheritParams get_xg3
#'
#' @return By default, a \code{tbl_lazy} object. With \code{collect = TRUE}, a
#' local \code{\link[tibble]{tibble}} is returned.
#'
#' **Returned Fields**
#'
#' - `loc_code` (chr): location code such as KESP001, ABES001, ...
#' - `date` (Date): sampling date
#' - `lab_project_id` (chr): code or identifier of the project as used by the
#' laboratory
#' - `lab_sample_id` (chr): code or identifier of the sample as used by the
#' laboratory
#' - `chem_variable` (chr): abbreviation for the chemical variable (detailed below)
#' - `value` (num): measurement value
#' - `unit` (chr): unit
#' - `below_loq` (logi): is the value below the limit of quantitation for this
#' analysis in the laboratory?
#' - `loq` (num): limit of quantitation for this analysis in the laboratory
#' - `elneutr` (num): value of the calculated electroneutrality (not in %)
#'
#' **Possible values for \code{chem_variable}:**
#'
#' - Al: aluminium concentration
#' - Ca: calcium concentration
#' - Cl: chloride concentration
#' - CondF: electrical conductivity at 25°C (measured in the field)
#' - CondL: electrical conductivity at 25°C (measured in the laboratory)
#' - Fe: iron concentration
#' - HCO3: bicarbonate concentration
#' - K: potassium concentration
#' - Mg: magnesium concentration
#' - Mn: manganese concentration
#' - N-NH4: ammonium (expressed as ammonium-nitrogen)
#' - N-NO2: nitrite (expressed as nitrite-nitrogen)
#' - N-NO3: nitrate (expressed as nitrate-nitrogen)
#' - Na: sodium concentration
#' - P-PO4: orthophosphate concentration (expressed as orthophosphate-phosphorus)
#' - pHF: pH (measured in the field)
#' - pHL: pH (measured in the laboratory)
#' - Si: silicon concentration
#' - SO4: sulphate concentration
#'
#' @family functions to query the data warehouse
#'
#' @md
#' @note Up to and including `watina 0.3.0`, the result was sorted according to
#' `loc_code`, `date` and `chem_variable`, both for the lazy query and the
#' collected result. Later versions avoid sorting in case of a lazy result,
#' because otherwise, when using the result inside another lazy query, this led
#' to 'ORDER BY' constructs in SQL subqueries, which must be avoided. If you
#' like to print the lazy object in a sorted manner, you must add `%>%
#' arrange(...)` yourself.
#'
#' @examples
#' \dontrun{
#' watina <- connect_watina()
#' library(dplyr)
#' mylocs <- get_locs(watina, area_codes = "ZWA")
#' mylocs %>%
#'   get_chem(watina, "1/1/2017") %>%
#'   arrange(loc_code, date, chem_variable)
#' mylocs %>%
#'   get_chem(watina, "1/1/2017", collect = TRUE)
#' mylocs %>%
#'   get_chem(watina, "1/1/2017", conc_type = "eq") %>%
#'   arrange(loc_code, date, chem_variable)
#'
#' # compare the number of returned rows:
#' mylocs %>%
#'   get_chem(watina, "1/1/2017") %>%
#'   count()
#' mylocs %>%
#'   get_chem(watina, "1/1/2017", en_fecond_threshold = NA) %>%
#'   count()
#' mylocs %>%
#'   get_chem(watina, "1/1/2017", en_exclude_na = TRUE) %>%
#'   count()
#' mylocs %>%
#'   get_chem(
#'     watina,
#'     "1/1/2017",
#'     en_exclude_na = TRUE,
#'     en_fecond_threshold = NA
#'   ) %>%
#'   count()
#' mylocs %>%
#'   get_chem(watina, "1/1/2017", en_range = c(-1, 1)) %>%
#'   count()
#'
#' # joining results to mylocs:
#' mylocs %>%
#'   get_chem(watina, "1/1/2017") %>%
#'   left_join(
#'     mylocs %>%
#'       select(-loc_wid),
#'     .
#'   ) %>%
#'   collect() %>%
#'   arrange(loc_code, date, chem_variable)
#'
#' # Disconnect:
#' dbDisconnect(watina)
#' }
#'
#' @export
#' @importFrom assertthat assert_that is.number is.flag noNA is.date
#' @importFrom rlang .data
#' @importFrom lubridate dmy today day month year
#' @importFrom dplyr %>% copy_to filter left_join inner_join select contains
#'   arrange distinct sql rename between
# FUNCTION GET CHEM ------------------------------------------------------------
get_chem <- function(
  locs,
  con,
  startdate,
  enddate = paste(
    day(today()),
    month(today()),
    year(today())
  ),
  conc_type = c("mass", "eq"),
  en_range = c(-0.1, 0.1),
  en_exclude_na = FALSE,
  en_fecond_threshold = 0.0023,
  collect = FALSE
) {
  conc_type <- match.arg(conc_type)

  assert_that(
    is.string(startdate),
    is.date(dmy(startdate))
  )
  assert_that(
    is.string(enddate),
    is.date(dmy(enddate))
  )
  startdate <- dmy(startdate)
  enddate <- dmy(enddate)
  assert_that(
    enddate >= startdate,
    msg = "startdate must not be larger than enddate."
  )

  assert_that(
    "loc_code" %in% colnames(locs),
    msg = "locs does not have a column name 'loc_code'."
  )
  assert_that(
    is.numeric(en_range),
    length(en_range) == 2,
    en_range[1] <= en_range[2],
    en_range[1] >= -1,
    en_range[2] <= 1
  )
  assert_that(is.flag(en_exclude_na), assertthat::noNA(en_exclude_na))
  assert_that(is.flag(collect), assertthat::noNA(collect))

  if (!is.na(en_fecond_threshold) & !is.null(en_fecond_threshold)) {
    assert_that(
      is.number(en_fecond_threshold),
      en_fecond_threshold > 0
    )
  }

  if (inherits(locs, "data.frame")) {
    locs <- stage_locs(locs, con)
  }

  # filter chemistry data for dates and locations
  chemdata <-
    tbl(con, "FactChemischeMeting") %>%
    select(
      .data$StaalID,
      .data$DatumWID,
      .data$ChemVarWID,
      .data$MeetpuntWID,
      .data$Meetwaarde,
      .data$MeetwaardeMEQ,
      .data$IsBelowLOQ
    ) %>%
    inner_join(
      tbl(con, "DimChemVar") %>%
        select(
          .data$ChemVarWID,
          .data$ChemVarCode,
          .data$ChemVarEenheid
        ),
      by = "ChemVarWID"
    ) %>%
    inner_join(
      tbl(con, "DimTijd") %>%
        select(
          .data$DatumWID,
          .data$Datum
        ),
      by = "DatumWID"
    ) %>%
    mutate(Datum = sql("CAST(Datum AS date)")) %>%
    filter(
      .data$Datum >= startdate,
      .data$Datum <= enddate
    ) %>%
    rename(loc_wid = .data$MeetpuntWID) %>%
    inner_join(
      locs %>%
        select(
          .data$loc_wid,
          .data$loc_code
        ) %>%
        distinct(),
      .,
      by = "loc_wid"
    ) %>%
    select(-.data$loc_wid)

  # add relevant further attributes and rename
  chem <-
    chemdata %>%
    left_join(
      tbl(con, "ssrs_StaalEN") %>%
        select(
          .data$StaalID,
          .data$StaalEN
        ),
      by = "StaalID"
    ) %>%
    # temporary values:
    mutate(
      lab_project_id = "0",
      lab_sample_id = sql("CAST(StaalID AS varchar)"),
      loq = -99
    ) %>%
    select(
      .data$loc_code,
      date = .data$Datum,
      .data$lab_project_id,
      .data$lab_sample_id,
      chem_variable = .data$ChemVarCode,
      value_mass = .data$Meetwaarde,
      value_eq = .data$MeetwaardeMEQ,
      unit = .data$ChemVarEenheid,
      below_loq = .data$IsBelowLOQ,
      .data$loq,
      elneutr = .data$StaalEN
    ) %>%
    filter(!is.na(.data$value_mass)) %>% # empty rows occur in the DWH!
    mutate(
      # when are value_eq units effectively meq/l ?
      provide_eq_unit = sql(
        "CAST((CASE
                 WHEN chem_variable IN
                 ('P-PO4', 'N-NO3', 'N-NO2', 'N-NH4', 'HCO3',
                 'SO4', 'Cl', 'Na', 'K', 'Ca', 'Mg',
                 'Fe', 'Mn', 'Si', 'Al') THEN 1
                 ELSE 0
                 END) AS bit)"
      )
    )

  # preparing for the application of the en_fecond_threshold:
  if (!is.na(en_fecond_threshold) & !is.null(en_fecond_threshold)) {
    if (
      any(
        chemdata %>%
          filter(
            .data$ChemVarCode == "CondL",
            !is.na(.data$MeetwaardeMEQ)
          ) %>%
          pull(.data$MeetwaardeMEQ) ==
          0
      )
    ) {
      warning(
        "Zeroes for 'CondL' (lab conductivity) detected. ",
        "These rows will be ignored in calculating the iron / conductivity ",
        "ratio for the `en_fecond_threshold` condition."
      )
    }
    samples_fecond <-
      chemdata %>%
      # temporary value:
      mutate(lab_sample_id = sql("CAST(StaalID AS varchar)")) %>%
      select(
        .data$lab_sample_id,
        chem_variable = .data$ChemVarCode,
        value_eq = .data$MeetwaardeMEQ
      ) %>%
      filter(
        !is.na(.data$value_eq),
        .data$chem_variable %in% c("Fe", "CondL")
      ) %>%
      db_pivot_wider(
        names_from = .data$chem_variable,
        values_from = .data$value_eq
      ) %>%
      mutate(
        fecond = .data$Fe / ifelse(.data$CondL == 0, NA_real_, .data$CondL)
      ) %>%
      select(
        .data$lab_sample_id,
        .data$fecond
      ) %>%
      filter(!is.na(.data$fecond))
  }

  # filtering chem according to sample characteristics
  chem <-
    # all cases return all non-ion measurements, regardless of settings

    # I. don't allow samples with elneutr = NA, except when
    # en_fecond_threshold is exceeded:
    if (en_exclude_na) {
      if (is.na(en_fecond_threshold) | is.null(en_fecond_threshold)) {
        # I.1 applying the en_range condition:
        chem %>%
          filter(
            (!is.na(.data$elneutr) &
              between(.data$elneutr, !!en_range[1], !!en_range[2])) |
              .data$provide_eq_unit == "FALSE"
          )
      } else {
        # I.2 applying the en_fecond_threshold OR the en_range condition:
        chem %>%
          left_join(samples_fecond, by = "lab_sample_id") %>%
          filter(
            (!is.na(.data$elneutr) &
              between(.data$elneutr, !!en_range[1], !!en_range[2])) |
              .data$fecond >= en_fecond_threshold |
              .data$provide_eq_unit == "FALSE"
          ) %>%
          select(-.data$fecond)
      }
    } else {
      # II. here, all samples with elneutr = NA are kept as well:
      if (is.na(en_fecond_threshold) | is.null(en_fecond_threshold)) {
        # II.1 applying the en_range condition:
        chem %>%
          filter(
            is.na(.data$elneutr) |
              between(.data$elneutr, !!en_range[1], !!en_range[2]) |
              .data$provide_eq_unit == "FALSE"
          )
      } else {
        # II.2 applying the en_fecond_threshold OR the en_range condition:
        chem %>%
          left_join(samples_fecond, by = "lab_sample_id") %>%
          filter(
            is.na(.data$elneutr) |
              between(.data$elneutr, !!en_range[1], !!en_range[2]) |
              .data$fecond >= en_fecond_threshold |
              .data$provide_eq_unit == "FALSE"
          ) %>%
          select(-.data$fecond)
      }
    }

  chem <-
    switch(
      conc_type,
      mass = chem %>%
        rename(value = .data$value_mass),
      eq = chem %>%
        rename(value = .data$value_eq) %>%
        mutate(
          unit = ifelse(
            .data$provide_eq_unit == "TRUE",
            "meq/l",
            .data$unit
          )
        )
    ) %>%
    select(-contains("value_"), -.data$provide_eq_unit) %>%
    mutate(unit = ifelse(.data$unit == "/", NA, .data$unit))

  if (collect) {
    chem <-
      chem %>%
      arrange(
        .data$loc_code,
        .data$date,
        .data$chem_variable
      ) %>%
      collect()
  }

  return(chem)
}

# HELPER FUNCTIONS -------------------------------------------------------------
stage_locs <- function(locs, con) {
  locs <-
    locs %>%
    distinct(.data$loc_code)

  require_pkgs("DBI")

  try(
    DBI::dbRemoveTable(con, "#locs"),
    silent = TRUE
  )

  locs <-
    copy_to(
      con,
      locs,
      "#locs"
    ) %>%
    inner_join(
      tbl(con, "DimMeetpunt") %>%
        select(
          loc_wid = .data$MeetpuntWID,
          loc_code = .data$MeetpuntCode
        ),
      .,
      by = "loc_code"
    )

  return(locs)
}
