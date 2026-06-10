# DOCUMENTATION GET XG3 --------------------------------------------------------
#' Get XG3 values from the data warehouse
#'
#' Returns XG3 values from the \emph{Watina} data warehouse,
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
#' @md
#'
#' @note
#' Up to and including `watina 0.3.0`, the result was sorted according to
#' `loc_code` and `hydroyear`, both for the lazy query and the
#' collected result.
#' Later versions avoid sorting in case of a lazy result, because
#' otherwise, when using the result inside another lazy query, this led to
#' 'ORDER BY' constructs in SQL subqueries, which must be avoided.
#' If you like to print the lazy object in a sorted manner, you must add
#' `%>% arrange(...)` yourself.
#'
#' @param locs A \code{tbl_lazy} object or a data frame, with at least a column
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
#' (i.e. non-measured) water level data that are available in the data warehouse.
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
#' @family functions to query the data warehouse
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
#' filter
#' left_join
#' inner_join
#' select
#' contains
#' arrange
#' distinct
# FUNCTION GET XG3 -------------------------------------------------------------
get_xg3 <- function(locs,
                    con,
                    startyear,
                    endyear = year(now()) - 1,
                    vert_crs = c("local", "ostend", "both"),
                    truncated = TRUE,
                    with_estimated = TRUE,
                    collect = FALSE) {
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

  if (inherits(locs, "data.frame")) {
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
        tbl(con, "vwDimMeetpunt") %>%
          select(
            loc_wid = .data$MeetpuntWID,
            loc_code = .data$MeetpuntCode
          ),
        .,
        by = "loc_code"
      )
  }

  xg3 <-
    tbl(con, "ssrs_Precalc") %>%
    # left_join(tbl(con, "DimMetingType"),
    #           by = "MetingTypeWID") %>%
    select(
      loc_wid = .data$MeetpuntWID,
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
    filter(
      .data$hydroyear >= startyear,
      .data$hydroyear <= endyear
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

  xg3 <-
    switch(vert_crs,
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

# DOCUMENTATION GET CHEM -------------------------------------------------------
#' Get hydrochemical data from the data warehouse
#'
#' Returns hydrochemical data from the \emph{Watina} data warehouse,
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
#'
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
#' **More information about the electroneutrality**
#'
#' We expect groundwater samples to have no net charge, i.e. the total
#' positive charge from cations must equal the total negative charge
#' from anions.
#' To ensure this is true (if we ignore the inevitable margin of error
#' in the laboratory),
#' we calculate:
#' \itemize{
#' \item the sum of charges from anions (AN) in the sample as
#'      *HCO3 + SO4 + PO4 + Cl + NO3 + NO2*
#' \item the sum of charges from cations (CAT) in the sample as
#'      *Ca + Mg + Na + K + Fe + NH4*
#' }
#'
#' Then we derive the electroneutrality as *(CAT - AN)/(CAT + AN)*
#'
#' If significant deviation from zero occurs, there must be analytical errors in
#' the concentration determinations or ions at significant concentration levels
#' that were not included in the analysis.
#'
#' The \code{get_chem()} function allows for a standard tolerance of +/-0.1 for
#' the electroneutrality.
#' This value can be adapted using the \code{en_range} argument.
#'
#' @md
#'
#' @note
#' Up to and including `watina 0.3.0`, the result was sorted according to
#' `loc_code`, `date` and `chem_variable`, both for the lazy query and the
#' collected result.
#' Later versions avoid sorting in case of a lazy result, because
#' otherwise, when using the result inside another lazy query, this led to
#' 'ORDER BY' constructs in SQL subqueries, which must be avoided.
#' If you like to print the lazy object in a sorted manner, you must add
#' `%>% arrange(...)` yourself.
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
#' **Returned Fields**
#'
#' - `loc_code` (chr): location code such as KESP001, ABES001, ...
#' - `date` (Date): sampling date
#' - `lab_project_id` (chr): code or identifier of the project as used
#' by the laboratory
#' - `lab_sample_id` (chr): code or identifier of the sample as used
#' by the laboratory
#' - `chem_variable` (chr): abbreviation for the chemical variable (detailed below)
#' - `value` (num): measurement value
#' - `unit` (chr): unit
#' - `below_loq` (logi): is the value below the limit of quantitation
#' for this analysis in the laboratory?
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
#' @importFrom dplyr
#' %>%
#' copy_to
#' filter
#' left_join
#' inner_join
#' select
#' contains
#' arrange
#' distinct
#' sql
#' rename
# FUNTION GET CHEM -------------------------------------------------------------
get_chem <- function(locs,
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
                     collect = FALSE) {
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
        tbl(con, "vwDimMeetpunt") %>%
          select(
            loc_wid = .data$MeetpuntWID,
            loc_code = .data$MeetpuntCode
          ),
        .,
        by = "loc_code"
      )
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
    )

  sqlstring_en <-
    paste0(
      "elneutr BETWEEN ",
      en_range[1],
      " AND ",
      en_range[2]
    )

  # preparing for the application of the en_fecond_threshold:
  if (!is.na(en_fecond_threshold) & !is.null(en_fecond_threshold)) {
    if (any(
      chemdata %>%
        filter(
          .data$ChemVarCode == "CondL",
          !is.na(.data$MeetwaardeMEQ)
        ) %>%
        pull(.data$MeetwaardeMEQ) == 0
    )) {
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
            (!is.na(.data$elneutr) & sql(sqlstring_en)) |
              .data$provide_eq_unit == "FALSE"
          )
      } else {
        # I.2 applying the en_fecond_threshold OR the en_range condition:
        chem %>%
          left_join(samples_fecond, by = "lab_sample_id") %>%
          filter(
            (!is.na(.data$elneutr) & sql(sqlstring_en)) |
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
              sql(sqlstring_en) |
              .data$provide_eq_unit == "FALSE"
          )
      } else {
        # II.2 applying the en_fecond_threshold OR the en_range condition:
        chem %>%
          left_join(samples_fecond, by = "lab_sample_id") %>%
          filter(
            is.na(.data$elneutr) |
              sql(sqlstring_en) |
              .data$fecond >= en_fecond_threshold |
              .data$provide_eq_unit == "FALSE"
          ) %>%
          select(-.data$fecond)
      }
    }

  chem <-
    switch(conc_type,
      mass = chem %>%
        rename(value = .data$value_mass),
      eq = chem %>%
        rename(value = .data$value_eq) %>%
        mutate(unit = ifelse(
          .data$provide_eq_unit == "TRUE",
          "meq/l",
          .data$unit
        ))
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
