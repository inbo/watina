#' Get locations from the database
#'
#' Returns locations from the \emph{Watina} database that meet
#' several criteria (spatial or non-spatial), either as a lazy object or as a
#' local tibble.
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
#' @param con A \code{DBIConnection} object to Watina.
#' See \code{\link{connect_watina}} to generate one.
#' @param max_filterdepth Numeric.
#' Maximum depth of the filter bottom below soil surface, as meters.
#' This condition is only applied to piezometers.
#' @param mask An optional geospatial filter of class \code{sf}.
#' If provided, only locations that intersect with \code{mask} will be returned.
#' The CRS must be Belgian Lambert 72 (EPSG-code
#' \href{https://epsg.io/31370}{31370}).
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
#' Defaults to \code{"VLD"}, i.e. only validated locations are returned by
#' default.
#' Can be a vector with multiple selected values.
#' @param loc_vec An optional vector with location codes.
#' If provided, only locations are returned that are present in this vector.
#' @param collect Should the data be retrieved as a local tibble?
#' If \code{FALSE} (the default), a \code{tbl_lazy} object is returned
#' (lazy query).
#' Hence the result can be further built upon before retrieving data with
#' \code{\link[dplyr:collect]{collect()}}.
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
#' # Selecting all piezometers with status VLD or ENT of the
#' # province "West-Vlaanderen":
#' data(BE_ADMIN_PROVINCE,
#'      package = "BelgiumMaps.StatBel")
#' library(dplyr)
#' library(sf)
#' library(stringr)
#' mymask <-
#'     st_as_sf(BE_ADMIN_PROVINCE) %>%
#'     filter(str_detect(TX_PROV_DESCR_NL, "West")) %>%
#'     st_transform(crs = 31370)
#' get_locs(watina,
#'          loc_validity = c("VLD", "ENT"),
#'          mask = mymask)
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
#' @importFrom sf
#' st_drop_geometry
#' st_intersects
#' @importFrom dplyr
#' %>%
#' tbl
#' filter
#' left_join
#' select
#' collect
#' distinct
#' arrange
get_locs <- function(con,
                     max_filterdepth = 3,
                     mask = NULL,
                     bbox = NULL,
                     area_codes = NULL,
                     loc_type = c("P", "S", "R", "N", "W", "D", "L", "B"),
                     loc_validity = c("VLD", "ENT", "DEL", "CLD"),
                     loc_vec = NULL,
                     collect = FALSE) {

    assert_that(is.number(max_filterdepth))
    assert_that(is.null(bbox) | all(sort(names(bbox)) ==
                                        c("xmax", "xmin", "ymax", "ymin")),
                msg = "You did not correctly specify bbox.")
    assert_that(is.null(area_codes) | all(is.character(area_codes)))
    assert_that(is.null(loc_vec) | all(is.character(loc_vec)),
                msg = "loc_vec must be a character vector.")
    assert_that(is.logical(collect))

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

    if (missing(loc_validity)) {
        loc_validity <- match.arg(loc_validity)} else {
            assert_that(all(loc_validity %in%
                                c("VLD", "ENT", "DEL", "CLD")),
                        msg = "You specified at least one unknown loc_validity.")
        }

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
                   .data$MeetpuntYCoordinaat >= bbox_ymax)
    }

    locs <-
        locs %>%
        left_join(tbl(con, "vwDimPeilpunt") %>%
                      select(.data$MeetpuntWID,
                             .data$PeilpuntStatusCode,
                             .data$PeilbuisLengte,
                             .data$ReferentieNiveauMaaiveld) %>%
                      filter(.data$PeilpuntStatusCode %in% c("VLD",
                                                       "ENT",
                                                       "CLD")),
                  by = "MeetpuntWID") %>%
        filter(.data$MeetpuntTypeCode == "P" &
                   (.data$PeilbuisLengte - .data$ReferentieNiveauMaaiveld) <=
                     max_filterdepth |
                   .data$MeetpuntTypeCode != "P"
               ) %>%
        select(loc_wid = .data$MeetpuntWID,
               loc_code = .data$MeetpuntCode,
               area_code = .data$GebiedCode,
               area_name = .data$GebiedNaam,
               x = .data$MeetpuntXCoordinaat,
               y = .data$MeetpuntYCoordinaat,
               loc_validitycode = .data$MeetpuntStatusCode,
               loc_validity = .data$MeetpuntStatus,
               loc_typecode = .data$MeetpuntTypeCode,
               loc_typename = .data$MeetpuntType) %>%
        distinct %>%
        arrange(.data$area_code,
                .data$loc_code)

    if (!is.null(mask)) {

        nr_dropped_locs <-
            locs %>%
            filter(is.na(.data$x) | is.na(.data$y)) %>%
            collect() %>%
            nrow

        if (nr_dropped_locs > 0) {
            warning("Dropped ",
                    nr_dropped_locs,
                    " locations from which x or y coordinates were missing.")
        }

        locs <-
            locs %>%
            select(-.data$loc_wid) %>%
            collect %>%
            filter(!is.na(.data$x), !is.na(.data$y)) %>%
            as_points %>%
            filter(st_intersects(x = .,
                                 y = mask,
                                 sparse = FALSE)) %>%
            st_drop_geometry

    }

    if (collect) {
        locs <-
            locs %>%
            select(-.data$loc_wid) %>%
            collect
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
#' Note: the argument \code{truncated} is currently not used.
#' Currently, non-truncated values are returned!
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
#' (which is zero in the case of the relative CRS).
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
#' mylocs %>%
#'   get_xg3(watina, 2010) %>%
#'   left_join(mylocs %>%
#'             select(-loc_wid),
#'             .) %>%
#'   collect
#' mylocs %>% get_xg3(watina, 2010, vert_crs = "ostend")
#' # Disconnect:
#' DBI::dbDisconnect(watina)
#' }
#'
#' @export
#' @importFrom assertthat
#' assert_that
#' has_name
#' is.number
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
#' collect
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
                    collect = FALSE) {

    vert_crs <- match.arg(vert_crs)
    assert_that(is.number(startyear))
    assert_that(is.number(endyear))
    assert_that(endyear >= startyear,
                msg = "startyear must not be larger than endyear.")
    assert_that("loc_code" %in% colnames(locs),
                msg = "locs does not have a column name 'loc_code'.")
    assert_that(is.logical(truncated))
    assert_that(is.logical(collect))

    if (inherits(locs, "data.frame")) {
        locs <-
            locs %>%
            distinct(.data$loc_code)

        try(db_drop_table(con, "##locs"),
            silent = TRUE)

        locs <-
            copy_to(con,
                    locs) %>%
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
                              .data$loc_code),
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












