#' Get locations from the database
#'
#' Returns locations (and optionally, observation wells) from the \emph{Watina}
#' database that meet
#' several criteria, either as a lazy object or as a
#' local tibble.
#' Criteria refer to spatial or non-spatial physical attributes of the
#' location or the location's observation wells.
#' Essential metadata are included in the result.
#'
#' (TO BE ADDED: Explanation on the different available values of loc_type
#' and loc_validity)
#'
#' The lazy object returns a \code{loc_wid} variable, for further use in
#' \emph{remote} queries.
#' However, don't use it in local objects: \code{loc_wid} is not to be
#' regarded as stable.
#' Therefore, \code{collect = TRUE} does not return \code{loc_wid}.
#'
#' The result also provides metadata at the level of the observation
#' well, even when \code{obswells = FALSE}.
#' In the latter case, this refers to the variables
#' \code{soilsurf_ost},
#' \code{measuringref_ost},
#' \code{tubelength},
#' \code{filterlength},
#' \code{filterdepth}.
#' See the argument \code{obswell_aggr} for options of how to aggregate this
#' information at the location level;
#' by default the latest observation well is used
#' (per location) that meets the criteria on filterdepth.
#' Mind that \code{obswells = FALSE} and \code{filterdepth_na = TRUE} may lead
#' to missing filterdepth values at locations which do have a
#' value for an older observation well, but not for the most recent one.
#'
#' Please note the meaning of observation well in Watina: if there are multiple
#' observation wells attached to one location, these belong to
#' \emph{other timeframes}!
#' So one location always coincides with exactly one observation well at
#' one moment in time.
#' Multiple observation wells can succeed one another because of physical
#' alterations (e.g. damage of a piezometer).
#' Here, the term 'observation well' is used to refer to a fixed installed
#' device in the field (groundwater piezometer, surface water level
#' measurement device).

#'
#'
#' @param con A \code{DBIConnection} object to Watina.
#' See \code{\link{connect_watina}} to generate one.
#' @param filterdepth_range Numeric vector of length 2.
#' Specifies the allowed range of the depth of the filter below soil
#' surface, as meters (minimum and maximum allowed filterdepth, respectively).
#' This condition is only applied to groundwater piezometers.
#' The second vector element cannot be smaller than the first.
#' Note that 'filterdepth' takes into account \emph{half} the length of the
#' filter.
#' It is always assumed that filters are at the bottom of the tube.
#' Hence
#' \code{filterdepth = tubelength - filterlength / 2 -
#' [tubelength part above soil surface]}.
#' If filterlength is missing, it is assumed to be 0.3 m.
#' With \code{obswells = FALSE}, a location is kept whenever one observation
#' well fulfills the condition.
#' @param filterdepth_guess Logical.
#' Only relevant for groundwater piezometers.
#' Defaults to \code{FALSE}.
#' For observation wells of which tubelength is known, but not
#' the part of the tubelength above soil surface (height of measuring point),
#' filterdepth cannot be calculated and is missing.
#' However, filterdepth will never be larger than tubelength minus half the
#' filterlength; hence a maximum
#' possible (i.e. conservative) value for filterdepth is given by
#' \code{tubelength - filterlength / 2}.
#' With \code{filterdepth_guess = TRUE}, filterdepth is replaced by this value
#' when it cannot be calculated and tubelength is available.
#' This is done before applying the \code{filterdepth_range} condition.
#' To mark these cases, a logical variable \code{filterdepth_guessed} is added
#' to the result: \code{TRUE} for wells where filterdepth was replaced;
#' \code{FALSE} in all other rows.
#' @param filterdepth_na Logical.
#' Are observation wells with missing filterdepth value to be included?
#' Defaults to \code{FALSE}.
#' With \code{filterdepth_guess = TRUE}, this has only effect on the
#' \emph{remaining} observation wells with missing filterdepth value.
#' @param obswells Logical.
#' If \code{TRUE}, the returned object distinguishes all observation wells
#' (see \emph{Details}) that
#' meet the \code{filterdepth_range} condition (or have missing filterdepth, if
#' \code{filterdepth_na = TRUE}).
#' If \code{FALSE} (the default), the returned object just distinguishes
#' locations.
#' In the latter case, the variables \code{obswell_installdate} and
#' \code{obswell_stopdate} are not returned.
#'
#' @param obswell_aggr String.
#' Defines how the attributes of multiple observation wells per location that
#' fulfill the \code{filterdepth_range} and
#' \code{filterdepth_na} criteria (after filterdepth adjustment if
#' \code{filterdepth_guess = TRUE}), are
#' aggregated into one record \strong{per location}:
#' \itemize{
#'
#' \item \code{"latest"}: return attributes of the most recent observation well
#' that fulfills the \code{filterdepth_range} and
#' \code{filterdepth_na} criteria;
#'
#' \item \code{"latest_fd"}: return attributes of the most recent observation well
#' that fulfills the \code{filterdepth_range} condition, i.e.
#' filterdepth will not be missing unless \emph{all} retained wells have missing
#' filterdepth \emph{and} \code{filterdepth_na = TRUE};
#'
#' \item \code{"latest_sso"}: return attributes of the most recent observation well
#' that fulfills the \code{filterdepth_range} and
#' \code{filterdepth_na} criteria \emph{and} for which \code{soilsurf_ost}
#' (soil surface level in the
#' \href{http://crs.bkg.bund.de/crseu/crs/eu-description.php?crs_id=Y0JFX09PU1QrJTJGK1VOQ09S}{Ostend height}
#' CRS (EPSG \href{https://epsg.io/5710}{5710}) is not missing (unless
#' \emph{all} retained wells have missing \code{soilsurf_ost});
#'
#' \item \code{"mean"}: aggregation not by selecting an individual observation
#' well, but by averaging the values of the associated variables
#' \code{soilsurf_ost},
#' \code{measuringref_ost},
#' \code{tubelength},
#' \code{filterlength},
#' \code{filterdepth}
#' for the observation wells with non-missing values (different
#' wells may be involved for each variable, depending on the distribution of
#' missing values).
#' With \code{filterdepth_guess = TRUE}, the extra variabele
#' \code{filterdepth_guessed} is summarised as \code{TRUE} for a location
#' if at least one of the location's observation wells has
#' \code{filterdepth_guessed = TRUE}.
#' }
#' \strong{In all cases} the returned value of \code{obswell_statecode} and
#' \code{obswell_state} corresponds to the \code{"latest"} approach.
#' The \code{obswell_aggr} argument has no effect on locations with a single
#' retained observation well.
#' It is ignored if \code{obswells = TRUE}.
#'
#' @param mask An optional geospatial filter of class \code{sf}.
#' If provided, only locations that intersect with \code{mask} will be returned,
#' with the value of \code{buffer} taken into account.
#' The CRS must be Belgian Lambert 72 (EPSG-code
#' \href{https://epsg.io/31370}{31370}).
#' @param join_mask Logical.
#' Do you want to spatially join the attribute columns of \code{mask} to the
#' resulting tibble?
#' The spatial join is executed with
#' \code{\link[sf:geos_binary_pred]{st_intersects()}} as the topological operator.
#' Beware: if the same location intersects with more than one element of
#' \code{mask} (taking into account the value of \code{buffer}), that location
#' will occur multiple times in the result.
#' \code{join_mask} is ignored if \code{mask} is not provided.
#' @param buffer Number of meters taken as a buffer to enlarge
#' \code{mask} (or shrink it, if \code{buffer < 0}) if \code{mask} is provided.
#' @param bbox Optional geospatial fiter (rectangle).
#' A bounding box (class \code{bbox}), or a vector of four named elements
#' \code{xmin}, \code{xmax}, \code{ymin}, \code{ymax} defining the
#' boundary coordinates of a bounding box.
#' If provided, only locations within this rectangular area will be returned.
#' The CRS must be Belgian Lambert 72 (EPSG-code
#' \href{https://epsg.io/31370}{31370}).
#' @param area_codes An optional vector with area codes.
#' If provided, only locations within the areas will be returned.
#' @param loc_type Type of the location (mainly: the type of measurement device).
#' Defaults to \code{"P"}, i.e. only groundwater piezometers are returned by
#' default.
#' Can be a vector with multiple selected values.
#' @param loc_validity Validation status of the location.
#' Can be a vector with multiple selected values, which must belong to
#' \code{"VLD"}, \code{"ENT"}, \code{"DEL"} or \code{"CLD"}.
#' Defaults to \code{c("VLD", "ENT")}.
#' @param loc_vec An optional vector with location codes.
#' If provided, only locations are returned that are present in this vector.
#' @param collect Should the data be retrieved as a local tibble?
#' If \code{FALSE} (the default), a \code{tbl_lazy} object is returned
#' (lazy query).
#' Hence the result can be further built upon before retrieving data with
#' \code{\link[dplyr:compute]{collect()}}.
#'
#' @return
#' By default, a \code{tbl_lazy} object.
#' With \code{collect = TRUE} or with a specified \code{mask},
#' a local \code{\link[tibble]{tibble}} is returned.
#'
#' (TO BE ADDED: Explanation on the variable names of the returned object)
#'
#' @family functions to query the database
#'
#' @examples
#' \dontrun{
#' watina <- connect_watina()
#'
#' library(dplyr)
#'
#' get_locs(watina,
#'          bbox = c(xmin = 1.4e+5,
#'                   xmax = 1.7e+5,
#'                   ymin = 1.6e+5,
#'                   ymax = 1.9e+5))
#'
#' get_locs(watina,
#'          area_codes = c("KAL", "KBR"),
#'          collect = TRUE)
#'
#' get_locs(watina,
#'          area_codes = c("KAL", "KBR"),
#'          loc_type = c("P", "S"),
#'          collect = TRUE)
#'
#' get_locs(watina,
#'          area_codes = "WES") %>%
#'     count()
#'
#' get_locs(watina,
#'          area_codes = "WES",
#'          filterdepth_guess = TRUE) %>%
#'     count()
#'
#' get_locs(watina,
#'          area_codes = c("KAL", "KBR"),
#'          loc_type = c("P", "S"),
#'          filterdepth_na = TRUE,
#'          collect = TRUE)
#'
#' # Mark the different output of:
#'   get_locs(watina,
#'            loc_vec = c("KBRP081", "KBRP090", "KBRP095", "KBRS001"),
#'            loc_type = c("P", "S"),
#'            collect = TRUE)
#'   # versus:
#'   get_locs(watina,
#'            loc_vec = c("KBRP081", "KBRP090", "KBRP095", "KBRS001"),
#'            collect = TRUE)
#'
#' # Returning all individual observation wells:
#' get_locs(watina,
#'          obswells = TRUE,
#'          area_codes = c("KAL", "KBR"),
#'          loc_type = c("P", "S"),
#'          collect = TRUE)
#'
#' # Different examples of aggregating observation wells at location level:
#' get_locs(watina,
#'          area_codes = "WES",
#'          filterdepth_na = TRUE,
#'          filterdepth_guess = TRUE,
#'          obswell_aggr = "latest",
#'          collect = TRUE) %>%
#'     select(loc_code, contains("ost"), contains("filterdepth")) %>%
#'     head(12)
#'
#' get_locs(watina,
#'          area_codes = "WES",
#'          filterdepth_na = TRUE,
#'          filterdepth_guess = TRUE,
#'          obswell_aggr = "mean",
#'          collect = TRUE) %>%
#'     select(loc_code, contains("ost"), contains("filterdepth")) %>%
#'     head(12)
#'
#' # Selecting all piezometers with status VLD of the
#' # province "West-Vlaanderen":
#' data(BE_ADMIN_PROVINCE,
#'      package = "BelgiumMaps.StatBel")
#' library(sf)
#' library(stringr)
#' mymask <-
#'     st_as_sf(BE_ADMIN_PROVINCE) %>%
#'     filter(str_detect(TX_PROV_DESCR_NL, "West")) %>%
#'     st_transform(crs = 31370)
#' get_locs(watina,
#'          loc_validity = "VLD",
#'          mask = mymask,
#'          buffer = 0)
#'
#' # Disconnect:
#' DBI::dbDisconnect(watina)
#' }
#'
#' @export
#' @importFrom inborutils connect_inbo_dbase
#' @importFrom rlang .data
#' @importFrom assertthat
#' assert_that
#' is.number
#' is.flag
#' noNA
#' @importFrom sf
#' st_drop_geometry
#' st_intersects
#' st_crs
#' st_join
#' st_buffer
#' @importFrom dplyr
#' %>%
#' tbl
#' filter
#' left_join
#' select
#' distinct
#' arrange
#' group_by
#' row_number
#' ungroup
#' sql
get_locs <- function(con,
                     filterdepth_range = c(0, 3),
                     filterdepth_guess = FALSE,
                     filterdepth_na = FALSE,
                     obswells = FALSE,
                     obswell_aggr = c("latest",
                                      "latest_fd",
                                      "latest_sso",
                                      "mean"),
                     mask = NULL,
                     join_mask = FALSE,
                     buffer = 10,
                     bbox = NULL,
                     area_codes = NULL,
                     loc_type = c("P", "S", "R", "N", "W", "D", "L", "B"),
                     loc_validity = c("VLD", "ENT"),
                     loc_vec = NULL,
                     collect = FALSE) {

    assert_that(is.numeric(filterdepth_range),
                length(filterdepth_range) == 2,
                filterdepth_range[1] <= filterdepth_range[2])

    assert_that(is.number(buffer))
    assert_that(is.null(bbox) | all(sort(names(bbox)) ==
                                        c("xmax", "xmin", "ymax", "ymin")),
                msg = "You did not correctly specify bbox.")
    assert_that(is.null(area_codes) | all(is.character(area_codes)))
    assert_that(is.null(loc_vec) | all(is.character(loc_vec)),
                msg = "loc_vec must be a character vector.")
    assert_that(is.flag(join_mask), noNA(join_mask))
    assert_that(is.flag(collect), noNA(collect))
    assert_that(is.flag(obswells), noNA(obswells))
    assert_that(is.flag(filterdepth_guess), noNA(filterdepth_guess))
    assert_that(is.flag(filterdepth_na), noNA(filterdepth_na))

    obswell_aggr <- match.arg(obswell_aggr)

    if (!is.null(mask) & !collect) {
        message("As a mask always invokes a collect(), the argument 'collect = FALSE' will be ignored.")
    }

    if (!is.null(mask)) {
        assert_that(inherits(mask, "sf"),
                    msg = "mask must be an sf object.")
        assert_that(st_crs(mask) == st_crs(31370),
                    msg = "The CRS of mask must be Belgian Lambert 72 (EPSG-code 31370).")
    }

    if (!is.null(bbox)) {
        assert_that(bbox["xmax"] >= bbox["xmin"],
                    bbox["ymax"] >= bbox["ymin"])
    }

    if (missing(loc_type)) {
        loc_type <- match.arg(loc_type)} else {
            assert_that(all(loc_type %in%
                                c("P", "S", "R", "N", "W", "D", "L", "B")),
                        msg = "You specified at least one unknown loc_type.")
        }

    assert_that(all(loc_validity %in%
                        c("VLD", "ENT", "DEL", "CLD")),
                msg = "You specified at least one unknown loc_validity.")

    min_filterdepth <- filterdepth_range[1]
    max_filterdepth <- filterdepth_range[2]

    locs <-
        tbl(con, "vwDimMeetpunt") %>%
        filter(.data$MeetpuntTypeCode %in% loc_type,
               .data$MeetpuntStatusCode %in% loc_validity
               ) %>%
        left_join(tbl(con, "vwDimGebied") %>%
                      select(.data$GebiedWID,
                             .data$GebiedCode,
                             .data$GebiedNaam),
                  by = "GebiedWID")

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
            filter(.data$MeetpuntXCoordinaat >= bbox_xmin,
                   .data$MeetpuntXCoordinaat <= bbox_xmax,
                   .data$MeetpuntYCoordinaat >= bbox_ymin,
                   .data$MeetpuntYCoordinaat <= bbox_ymax)
    }

    locs <-
        locs %>%
        left_join(tbl(con, "vwDimPeilpunt") %>%
                      filter(.data$PeilpuntStatusCode %in% c("VLD",
                                                       "ENT",
                                                       "CLD"),
                             .data$PeilpuntOpenbaarheidTypeCode == "PLME",
                             .data$PeilpuntOpenbaarheidCode == "UNKWN") %>%
                      mutate(PeilpuntPlaatsing =
                                 sql("CAST(PeilpuntPlaatsing AS date)"),
                             PeilpuntStopzetting =
                                 sql("CAST(PeilpuntStopzetting AS date)")
                             ),
                  by = "MeetpuntWID") %>%
        mutate(tubelength = ifelse(.data$PeilbuisLengte <= 0,
                                   NA,
                                   .data$PeilbuisLengte),
               filterlength = ifelse(is.na(.data$FilterLengte),
                                     0.3,
                                     .data$FilterLengte),
               filterdepth = .data$tubelength -
                                .data$ReferentieNiveauMaaiveld -
                                .data$filterlength / 2,
               soilsurf_ost =
                   .data$ReferentieNiveauTAW -
                   .data$ReferentieNiveauMaaiveld) %>%
        select(loc_wid = .data$MeetpuntWID,
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
               .data$filterdepth) %>%
        arrange(.data$area_code,
                .data$loc_code,
                .data$obswell_rank)

    if (filterdepth_guess) {
        locs <-
            locs %>%
            mutate(filterdepth_guessed =
                       is.na(.data$filterdepth) &
                       !is.na(.data$tubelength),
                   filterdepth = ifelse(.data$filterdepth_guessed == 1,
                                                # (sql: logical stored as bit)
                                        .data$tubelength -
                                            .data$filterlength / 2,
                                        .data$filterdepth))
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
            filter(.data$loc_typecode == "P" &
                       .data$filterdepth <= max_filterdepth &
                       .data$filterdepth >= min_filterdepth |
                       .data$loc_typecode != "P"
            )
    }

    locs <-
        locs %>%
        arrange(.data$area_code,
                .data$loc_code,
                .data$obswell_rank)

    if (!obswells) {

        locs <-
            locs %>%
            group_by(.data$loc_code) %>%
            mutate(obswell_count = n(),
                   obswell_maxrank = max(.data$obswell_rank,
                                         na.rm = TRUE),
                   obswell_maxrank_fd =
                       max(ifelse(is.na(.data$filterdepth),
                                         NA,
                                         .data$obswell_rank),
                                  na.rm = TRUE),
                   obswell_maxrank_sso =
                       max(ifelse(is.na(.data$soilsurf_ost),
                                         NA,
                                         .data$obswell_rank),
                                  na.rm = TRUE),
                   obswell_statecode =
                       max(ifelse(.data$obswell_rank ==
                                             .data$obswell_maxrank,
                                         .data$obswell_statecode,
                                         NA),
                                  na.rm = TRUE),
                   obswell_state =
                       max(ifelse(.data$obswell_rank ==
                                             .data$obswell_maxrank,
                                         .data$obswell_state,
                                         NA),
                                  na.rm = TRUE)
                   )

        locs <-
            switch(obswell_aggr,

                   "latest" =
                       locs %>%
                       ungroup() %>%
                       filter(.data$obswell_count == 1 |
                                  .data$obswell_rank == .data$obswell_maxrank),

                   "latest_fd" =
                       locs %>%
                       ungroup() %>%
                       filter(.data$obswell_count == 1 |
                                  (.data$obswell_rank ==
                                       .data$obswell_maxrank_fd) |
                                  (is.na(.data$obswell_maxrank_fd) &
                                       (.data$obswell_rank ==
                                            .data$obswell_maxrank))
                       ),

                   "latest_sso" =
                       locs %>%
                       ungroup() %>%
                       filter(.data$obswell_count == 1 |
                                  (.data$obswell_rank ==
                                       .data$obswell_maxrank_sso) |
                                  (is.na(.data$obswell_maxrank_sso) &
                                       (.data$obswell_rank ==
                                            .data$obswell_maxrank))
                       ),

                   "mean" =
                       locs %>%
                       mutate(soilsurf_ost = mean(.data$soilsurf_ost,
                                                  na.rm = TRUE),
                              measuringref_ost = mean(.data$measuringref_ost,
                                                  na.rm = TRUE),
                              filterdepth = mean(.data$filterdepth,
                                                  na.rm = TRUE),
                              filterlength = mean(.data$filterlength,
                                                 na.rm = TRUE),
                              tubelength = mean(.data$tubelength,
                                                  na.rm = TRUE)) %>%
                       {if ("filterdepth_guessed" %in% colnames(.)) {
                           mutate(.,
                                  filterdepth_guessed =
                                      max(ifelse(.data$filterdepth_guessed == 1,
                                                 # (sql: logical stored as bit)
                                                 1,
                                                 0),
                                          na.rm = TRUE)) %>%
                               mutate(filterdepth_guessed =
                                          sql("CAST(
                                              filterdepth_guessed AS bit)"))
                       } else .} %>%
                       filter(row_number() == 1L) %>%
                       ungroup()

                   ) %>%
            select(-.data$obswell_code,
                   -.data$obswell_rank,
                   -.data$obswell_installdate,
                   -.data$obswell_stopdate,
                   -.data$obswell_count,
                   -.data$obswell_maxrank,
                   -.data$obswell_maxrank_fd,
                   -.data$obswell_maxrank_sso) %>%
            arrange(.data$area_code,
                    .data$loc_code)
    }

    if (!is.null(mask)) {

        locs <-
            locs %>%
            select(-.data$loc_wid) %>%
            collect

        nr_dropped_locs <-
            locs %>%
            filter(is.na(.data$x) | is.na(.data$y)) %>%
            count %>%
            .$n

        if (nr_dropped_locs > 0) {
            warning("Dropped ",
                    nr_dropped_locs,
                    " locations from which x or y coordinates were missing.\n")
        }

        locs <-
            locs %>%
            filter(!is.na(.data$x), !is.na(.data$y)) %>%
            arrange(.data$area_code,
                    .data$loc_code) %>%
            as_points(warn_dupl = FALSE)

        if (buffer != 0) {
            mask_expand <-
                mask %>%
                st_buffer(dist = buffer)
        } else {
            mask_expand <-
                mask
        }

        if (join_mask) {

            locs <-
                locs %>%
                st_join(mask_expand,
                        left = FALSE) %>%
                st_drop_geometry

        } else {

            locs <-
                locs %>%
                .[mask_expand, ] %>%
                st_drop_geometry

        }

    }

    if (collect & is.null(mask)) {
        locs <-
            locs %>%
            select(-.data$loc_wid) %>%
            collect %>%
            arrange(.data$area_code,
                    .data$loc_code)
    }

    if (inherits(locs, "data.frame")) {
        warn_xy_duplicates(locs$x, locs$y)
    }

    return(locs)

}









#' Get XG3 values from the database
#'
#' Returns XG3 values from the \emph{Watina} database,
#' either as a lazy object or as a
#' local tibble.
#' The values must belong to selected locations
#' and
#' to a specified timeframe.
#'
#' The timeframe is a selection interval between
#' a given first and last hydroyear.
#'
#' Note: the arguments \code{truncated} and \code{with_estimated} are currently
#' not used.
#' Currently, non-truncated values are returned, with usage of estimated values.
#'
#' (TO BE ADDED: What are XG3 values? What is a hydroyear?
#' Why truncate, and why truncate by default?
#' When to choose which \code{vert_crs}?)
#'
#' @param locs A \code{tbl_lazy} object or a dataframe, with at least a column
#' \code{loc_code} that defines the locations for which values are to be
#' returned.
#' Typically, this will be the object returned by \code{\link{get_locs}}.
#' @param startyear First hydroyear of the timeframe.
#' @param endyear Last hydroyear of the timeframe.
#' @param vert_crs A string, defining the 1-dimensional vertical coordinate
#' reference system (CRS) of the XG3 water levels.
#' Either \code{"local"} (the default, i.e. returned values are relative to
#' soil surface level, with positive values = above soil surface),
#' or \code{"ostend"} (values are from the CRS
#' \href{http://crs.bkg.bund.de/crseu/crs/eu-description.php?crs_id=Y0JFX09PU1QrJTJGK1VOQ09S}{Ostend height}
#' (EPSG \href{https://epsg.io/5710}{5710}),
#' also known as 'TAW' or 'DNG'),
#' or \code{"both"}, where the values for both CRS options are returned.
#' The units are always meters.
#' @param truncated Logical.
#' If \code{TRUE} (the default), the XG3 values are calculated after having set
#' the underlying water level measurements that are above soil surface level
#' to the soil surface level itself
#' (which is zero in the case of the local CRS).
#' @param with_estimated Logical.
#' If \code{TRUE} (the default), the XG3 values calculations also use estimated
#' (i.e. non-measured) water level data that are available in the database.
#'
#' @inheritParams get_locs
#'
#' @return
#' By default, a \code{tbl_lazy} object.
#' With \code{collect = TRUE},
#' a local \code{\link[tibble]{tibble}} is returned.
#'
#' (TO BE ADDED: Explanation on the variable names of the returned object)
#'
#' The suffix of the XG3 variables is either "\code{_lcl}" for
#' \code{vert_crs = "local"} or
#' "\code{_ost}" for \code{vert_crs = "ostend"}.
#'
#' @family functions to query the database
#'
#' @examples
#' \dontrun{
#' watina <- connect_watina()
#' library(dplyr)
#' mylocs <- get_locs(watina, area_codes = "KAL")
#' mylocs %>% get_xg3(watina, 2010)
#' mylocs %>% get_xg3(watina, 2010, collect = TRUE)
#' mylocs %>% get_xg3(watina, 2010, vert_crs = "ostend")
#'
#' # joining results to mylocs:
#' mylocs %>%
#'   get_xg3(watina, 2010) %>%
#'   left_join(mylocs %>%
#'             select(-loc_wid),
#'             .) %>%
#'   collect
#'
#' # Disconnect:
#' DBI::dbDisconnect(watina)
#' }
#'
#' @export
#' @importFrom assertthat
#' assert_that
#' is.number
#' is.flag
#' noNA
#' @importFrom rlang .data
#' @importFrom lubridate
#' year
#' now
#' @importFrom dplyr
#' %>%
#' copy_to
#' db_drop_table
#' filter
#' left_join
#' inner_join
#' select
#' contains
#' arrange
#' distinct
get_xg3 <- function(locs,
                    con,
                    startyear,
                    endyear = year(now()) - 1,
                    vert_crs = c("local",
                                 "ostend",
                                 "both"),
                    truncated = TRUE,
                    with_estimated = TRUE,
                    collect = FALSE) {

    vert_crs <- match.arg(vert_crs)
    assert_that(is.number(startyear))
    assert_that(is.number(endyear))
    assert_that(endyear >= startyear,
                msg = "startyear must not be larger than endyear.")
    assert_that("loc_code" %in% colnames(locs),
                msg = "locs does not have a column name 'loc_code'.")
    assert_that(is.flag(truncated), noNA(truncated))
    assert_that(is.flag(collect), noNA(collect))

    if (inherits(locs, "data.frame")) {
        locs <-
            locs %>%
            distinct(.data$loc_code)

        try(db_drop_table(con, "##locs"),
            silent = TRUE)

        locs <-
            copy_to(con,
                    locs,
                    "##locs") %>%
            inner_join(tbl(con, "vwDimMeetpunt") %>%
                          select(loc_wid = .data$MeetpuntWID,
                                 loc_code = .data$MeetpuntCode),
                      .,
                      by = "loc_code")
    }

    xg3 <-
        tbl(con, "ssrs_Precalc") %>%
        # left_join(tbl(con, "DimMetingType"),
        #           by = "MetingTypeWID") %>%
        select(loc_wid = .data$MeetpuntWID,
               hydroyear = .data$HydroJaar,
               # method_code = .data$MetingTypeCode,
               # method_name = .data$MetingTypeNaam,
               lg3_lcl = .data$GLG_2,
               hg3_lcl = .data$GHG_2,
               vg3_lcl = .data$GVG_2,
               lg3_ost = .data$GLG_1,
               hg3_ost = .data$GHG_1,
               vg3_ost = .data$GVG_1
               ) %>%
        filter(.data$hydroyear >= startyear,
               .data$hydroyear <= endyear) %>%
        inner_join(locs %>%
                       select(.data$loc_wid,
                              .data$loc_code) %>%
                       distinct,
                   .,
                   by = "loc_wid") %>%
        select(-.data$loc_wid)

    xg3 <-
        switch(vert_crs,
               local = xg3 %>% select(-contains("ost")),
               ostend = xg3 %>% select(-contains("lcl")),
               both = xg3
               ) %>%
        arrange(.data$loc_code,
                .data$hydroyear)

    if (collect) {
        xg3 <-
            xg3 %>%
            collect
    }

    return(xg3)

}












#' Get hydrochemical data from the database
#'
#' Returns hydrochemical data from the \emph{Watina} database,
#' either as a lazy object or as a
#' local tibble.
#' The values must belong to selected locations
#' and
#' to a specified timeframe.
#'
#' The timeframe is a selection interval between
#' a given \code{startdate} and \code{enddate}.
#'
#' The water samples must meet a specified electroneutrality
#' condition, set by \code{en_range}.
#' \itemize{
#' \item This condition is however ignored when the sample's iron (meq/l) /
#' conductivity (µS/cm) ratio exceeds \code{en_fecond_threshold} (use
#' \code{en_fecond_threshold = NA} if you don't want this to happen).
#' \item Further, water samples are included by default if their
#' electroneutrality is \code{NA} (this is controlled by the
#' \code{en_exclude_na} argument).
#' \item Finally, please note that measurements of non-ion variables are
#' \emph{always} returned!
#' }
#' To retrieve all data from all water samples, use \code{en_range = c(-1, 1)}.
#'
#' TO BE ADDED: What is electroneutrality and why is it used as a criterion?
#'
#' @param startdate First date of the timeframe, as a string.
#' The string must use a formatting of the order 'day month year',
#' i.e. a format which can be interpreted by \code{\link[lubridate:ymd]{dmy}}.
#'
#' Examples:
#' \code{"16-1-2005"},
#' \code{"16-01-2005"},
#' \code{"1-01-2005"},
#' \code{"16/1/2005"},
#' \code{"16/1/05"},
#' \code{"16/1/88"} (years 69 and higher are regarded as 19xy),
#' \code{"16/1-2005"},
#' \code{"23 Oct 99"},
#' \code{"23 Okt 99"} (supposing this notation follows your system locale),
#' \code{"16 1-!!-2005"},
#' ......
#' @param enddate Last date of the timeframe, as a string.
#' The same formatting rule must be applied as in \code{startdate}.
#' Defaults to a string representation of the current system date.
#' @param conc_type A string defining the type of concentration in
#' \emph{ionic concentration variables}.
#' Either:
#' \itemize{
#' \item{\code{"mass"}:} mass concentration (the default);
#' \item{\code{"eq"}:} equivalent concentration (= normality), referring to the
#' electrical charge of the dissolved ion's main natural form.
#' }
#' Note that the argument has no effect on the value of non-ion-variables.
#' @param en_range Numeric vector of length 2.
#' Specifies the allowed range of
#' water sample electroneutrality for ion-variable measurements (see Details).
#' Both vector elements must be within the range \code{c(-1, 1)}, with the
#' second element not being smaller than the first.
#' Note that this argument only affects the selection of water samples for
#' ionic concentration variables, not for non-ion variables such as pH and
#' electrical conductivity.
#' Measurements of non-ion variables are always returned.
#' @param en_exclude_na Logical.
#' Should ion-variable measurements of water samples with missing
#' electroneutrality value be omitted?
#' Defaults to FALSE.
#' A missing electroneutrality value is the consequence of one or more missing
#' values of ionic concentration variables that are needed for
#' electroneutrality calculation of the water sample.
#' Note that this argument has no effect on the selection of non-ion variable
#' measurements, which are always returned.
#' @param en_fecond_threshold A number (with a sensible default).
#' May be set to \code{NA} or \code{NULL} by the user.
#' \itemize{
#' \item If \code{en_fecond_threshold} is a number (numeric scalar), all
#' measurements from water samples with an iron (meq/l) /
#' conductivity (µS/cm) ratio
#' (\code{Fe/CondL}) equal to or larger than \code{en_fecond_threshold} are
#' returned, regardless of the \code{en_range} and \code{en_exclude_na}
#' arguments.
#' \item If \code{en_fecond_threshold} is set to \code{NA} or \code{NULL},
#' the iron / conductivity ratio is ignored.
#' Hence, no exceptions are made to
#' the conditions imposed by \code{en_range} and \code{en_exclude_na}
#' (except for measurements of non-ion variables, which are always returned).
#' }
#'
#' @inheritParams get_xg3
#'
#' @return
#' By default, a \code{tbl_lazy} object.
#' With \code{collect = TRUE},
#' a local \code{\link[tibble]{tibble}} is returned.
#'
#' (TO BE ADDED: Explanation on the variable names of the returned object)
#'
#' (TO BE ADDED: Explanation on the different abbreviations in the column
#' 'chem_variable')
#'
#' @family functions to query the database
#'
#' @examples
#' \dontrun{
#' watina <- connect_watina()
#' library(dplyr)
#' mylocs <- get_locs(watina, area_codes = "ZWA")
#' mylocs %>% get_chem(watina, "1/1/2017")
#' mylocs %>% get_chem(watina, "1/1/2017", collect = TRUE)
#' mylocs %>% get_chem(watina, "1/1/2017", conc_type = "eq")
#'
#' # compare the number of returned rows:
#' mylocs %>% get_chem(watina, "1/1/2017") %>% count
#' mylocs %>% get_chem(watina, "1/1/2017",
#'                     en_fecond_threshold = NA) %>% count
#' mylocs %>% get_chem(watina, "1/1/2017",
#'                     en_exclude_na = TRUE) %>% count
#' mylocs %>% get_chem(watina, "1/1/2017",
#'                     en_exclude_na = TRUE,
#'                     en_fecond_threshold = NA) %>% count
#' mylocs %>% get_chem(watina, "1/1/2017",
#'                     en_range = c(-1, 1)) %>% count
#'
#' # joining results to mylocs:
#' mylocs %>%
#' get_chem(watina, "1/1/2017") %>%
#'     left_join(mylocs %>%
#'                   select(-loc_wid),
#'               .) %>%
#'     collect
#'
#' # Disconnect:
#' DBI::dbDisconnect(watina)
#' }
#'
#' @export
#' @importFrom assertthat
#' assert_that
#' is.number
#' is.flag
#' noNA
#' is.date
#' @importFrom rlang .data
#' @importFrom lubridate
#' dmy
#' today
#' day
#' month
#' year
#' @importFrom dbplyr
#' db_pivot_wider
#' @importFrom dplyr
#' %>%
#' copy_to
#' db_drop_table
#' filter
#' left_join
#' inner_join
#' select
#' contains
#' arrange
#' distinct
#' sql
get_chem <- function(locs,
                     con,
                     startdate,
                     enddate = paste(day(today()),
                                     month(today()),
                                     year(today())),
                     conc_type = c("mass", "eq"),
                     en_range = c(-0.1, 0.1),
                     en_exclude_na = FALSE,
                     en_fecond_threshold = 0.0023,
                     collect = FALSE) {

    conc_type <- match.arg(conc_type)

    assert_that(is.string(startdate),
                is.date(dmy(startdate)))
    assert_that(is.string(enddate),
                is.date(dmy(enddate)))
    startdate <- dmy(startdate)
    enddate <- dmy(enddate)
    assert_that(enddate >= startdate,
                msg = "startdate must not be larger than enddate.")

    assert_that("loc_code" %in% colnames(locs),
                msg = "locs does not have a column name 'loc_code'.")
    assert_that(is.numeric(en_range),
                length(en_range) == 2,
                en_range[1] <= en_range[2],
                en_range[1] >= -1,
                en_range[2] <= 1
                )
    assert_that(is.flag(en_exclude_na), noNA(en_exclude_na))
    assert_that(is.flag(collect), noNA(collect))

    if (!is.na(en_fecond_threshold) & !is.null(en_fecond_threshold)) {
        assert_that(is.number(en_fecond_threshold),
                    en_fecond_threshold > 0)
    }

    if (inherits(locs, "data.frame")) {
        locs <-
            locs %>%
            distinct(.data$loc_code)

        try(db_drop_table(con, "##locs"),
            silent = TRUE)

        locs <-
            copy_to(con,
                    locs,
                    "##locs") %>%
            inner_join(tbl(con, "vwDimMeetpunt") %>%
                           select(loc_wid = .data$MeetpuntWID,
                                  loc_code = .data$MeetpuntCode),
                       .,
                       by = "loc_code")
    }


    chem <-
        tbl(con, "FactChemischeMeting") %>%
        select(.data$StaalID,
               .data$DatumWID,
               .data$ChemVarWID,
               .data$MeetpuntWID,
               .data$Meetwaarde,
               .data$MeetwaardeMEQ,
               .data$IsBelowLOQ) %>%
        inner_join(tbl(con, "DimChemVar") %>%
                       select(.data$ChemVarWID,
                              .data$ChemVarCode,
                              .data$ChemVarEenheid),
                   by = "ChemVarWID") %>%
        inner_join(tbl(con, "DimTijd") %>%
                       select(.data$DatumWID,
                              .data$Datum),
                   by = "DatumWID") %>%
        mutate(Datum = sql("CAST(Datum AS date)")) %>%
        left_join(tbl(con, "ssrs_StaalEN") %>%
                      select(.data$StaalID,
                             .data$StaalEN),
                  by = "StaalID") %>%
        filter(.data$Datum >= startdate,
               .data$Datum <= enddate) %>%
        # temporary values:
        mutate(lab_project_id = "0",
               lab_sample_id = sql("CAST(StaalID AS varchar)"),
               loq = -99) %>%
        select(loc_wid = .data$MeetpuntWID,
               date = .data$Datum,
               .data$lab_project_id,
               .data$lab_sample_id,
               chem_variable = .data$ChemVarCode,
               value_mass = .data$Meetwaarde,
               value_eq = .data$MeetwaardeMEQ,
               units = .data$ChemVarEenheid,
               below_loq = .data$IsBelowLOQ,
               .data$loq,
               elneutr = .data$StaalEN
        ) %>%
        filter(!is.na(.data$value_mass)) %>%  # empty rows occur in the DWH!
        mutate(
            provide_eq_unit = # when are value_eq units effectively meq/l ?
                sql(
                 "CAST((CASE
                 WHEN chem_variable IN
                 ('P-PO4', 'N-NO3', 'N-NO2', 'N-NH4', 'HCO3',
                 'SO4', 'Cl', 'Na', 'K', 'Ca', 'Mg',
                 'Fe', 'Mn', 'Si', 'Al') THEN 1
                 ELSE 0
                 END) AS bit)"
                )
        ) %>%
        inner_join(locs %>%
                       select(.data$loc_wid,
                              .data$loc_code) %>%
                       distinct,
                   .,
                   by = "loc_wid") %>%
        select(-.data$loc_wid)

    sqlstring_en <-
        paste0("elneutr BETWEEN ",
               en_range[1],
               " AND ",
               en_range[2])

    # preparing for the application of the en_fecond_threshold:
    if (!is.na(en_fecond_threshold) & !is.null(en_fecond_threshold)) {
        samples_fecond <-
            tbl(con, "FactChemischeMeting") %>%
            select(.data$StaalID,
                   .data$DatumWID,
                   .data$ChemVarWID,
                   .data$MeetwaardeMEQ) %>%
            inner_join(tbl(con, "DimChemVar") %>%
                           select(.data$ChemVarWID,
                                  .data$ChemVarCode),
                       by = "ChemVarWID") %>%
            inner_join(tbl(con, "DimTijd") %>%
                           select(.data$DatumWID,
                                  .data$Datum),
                       by = "DatumWID") %>%
            mutate(Datum = sql("CAST(Datum AS date)")) %>%
            filter(.data$Datum >= startdate,
                   .data$Datum <= enddate) %>%
            # temporary value:
            mutate(lab_sample_id = sql("CAST(StaalID AS varchar)")) %>%
            select(.data$lab_sample_id,
                   chem_variable = .data$ChemVarCode,
                   value_eq = .data$MeetwaardeMEQ) %>%
            filter(!is.na(.data$value_eq),
                   .data$chem_variable %in% c("Fe", "CondL")) %>%
            db_pivot_wider(names_from = .data$chem_variable,
                           values_from = .data$value_eq) %>%
            mutate(fecond = .data$Fe / .data$CondL) %>%
            select(.data$lab_sample_id,
                   .data$fecond) %>%
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
                    filter((!is.na(.data$elneutr) & sql(sqlstring_en)) |
                               .data$provide_eq_unit == "FALSE")
            } else {
                # I.2 applying the en_fecond_threshold OR the en_range condition:
                chem %>%
                    left_join(samples_fecond, by = "lab_sample_id") %>%
                    filter((!is.na(.data$elneutr) & sql(sqlstring_en)) |
                               .data$fecond >= en_fecond_threshold |
                               .data$provide_eq_unit == "FALSE") %>%
                    select(-.data$fecond)
            }

        } else {

            # II. here, all samples with elneutr = NA are kept as well:
            if (is.na(en_fecond_threshold) | is.null(en_fecond_threshold)) {
                # II.1 applying the en_range condition:
                chem %>%
                    filter(is.na(.data$elneutr) |
                               sql(sqlstring_en) |
                               .data$provide_eq_unit == "FALSE")
            } else {
                # II.2 applying the en_fecond_threshold OR the en_range condition:
                chem %>%
                    left_join(samples_fecond, by = "lab_sample_id") %>%
                    filter(is.na(.data$elneutr) |
                               sql(sqlstring_en) |
                               .data$fecond >= en_fecond_threshold |
                               .data$provide_eq_unit == "FALSE") %>%
                    select(-.data$fecond)
            }

        }

    chem <-
        switch(conc_type,
               mass = chem %>%
                        rename(value = .data$value_mass),
               eq = chem %>%
                        rename(value = .data$value_eq) %>%
                        mutate(units = ifelse(.data$provide_eq_unit == "TRUE",
                                              "meq/l",
                                              units))
        ) %>%
        select(-contains("value_"), -.data$provide_eq_unit) %>%
        mutate(units = ifelse(.data$units == "/", NA, units)) %>%
        arrange(.data$loc_code,
                .data$date,
                .data$chem_variable)

    if (collect) {
        chem <-
            chem %>%
            collect
    }

    return(chem)

}












