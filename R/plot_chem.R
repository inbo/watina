#' Return the 'vanwirdum_data' data source as a tibble
#'
#' Returns the included data source \code{\link{vanwirdum_data}} as a
#' \code{\link[tibble:tbl_df-class]{tibble}}
#'
#' This datasets gives the curved contour which encloses the plotting area of
#' all possible, simple mixtures of the reference water samples LI-ANG (a
#' relatively calcium-rich groundwater sample ), AT-WTV (a precipitation sample
#' caught in a relatively unpolluted inland area of The Netherlands) and TH-N70
#' (a representative analysis from the North Sea monitoring program, 70 km from
#' the coast).
#' Source: Van Wirdum, Geert (1991). Vegetation and hydrology of floating
#' rich-fens. Datawyse, Maastricht. 316 p. ISBN 90-5291-045-6. (Appendix D)
#' \href{https://publicwiki.deltares.nl/display/VWD/Home}{dataset available here}
#'
#' \code{read_vanwirdum_data()}  returns it as a
#' \code{\link[tibble:tbl_df-class]{tibble}} with 42 rows and 2 variables.
#' A tibble is a dataframe that makes working in the tidyverse a little
#' \href{https://r4ds.had.co.nz/tibbles.html}{easier}.
#'
#' Variables:
#' \describe{
#'   \item{ec25}{electric conductivity at 25°C in mS/cm}
#'   \item{ir}{ionic ratio in %}}
#'
#' @return
#' The \code{vanwirdum_data} dataframe as a \code{\link[tibble:tbl_df-class]{tibble}}
#
#' @examples
#' read_vanwirdum_data()
#'
#' @export
#' @importFrom dplyr
#' tribble
#'
#'@keywords internal
#'

read_vanwirdum_data <-
    function() {
        vanwirdum_data <- tribble(
            ~ec25, ~ir,
            63.30956284,94.31498538,
            35.93464093,93.32760382,
            24.90267631,92.05872255,
            15.56978112,88.81507815,
            12.42913352,86.41593288,
            9.30501041,80.77314322,
            7.771007662,74.42660428,
            6.921570383,68.64306476,
            6.448364484,63.0002751,
            5.969064634,55.66422203,
            5.63284352,48.1874191,
            5.525390616,43.46376864,
            5.364388374,38.42662987,
            5.161676837,31.3166296,
            5.161676837,24.20876189,
            5.161676837,18.62141914,
            5.575060883,13.79753773,
            7.361877404,8.380800867,
            12.63883835,5.503959123,
            25.32283525,4.232945276,
            73.49784815,3.895998652,
            228.6654432,3.810695709,
            695.1394102,3.725392767,
            3157.436507,3.725392767,
            4609.686896,3.556919455,
            4609.686896,3.471616512,
            3206.339376,3.810695709,
            2001.956002,4.825800728,
            1087.858305,6.858143339,
            679.2308009,9.903458395,
            420.8067858,14.55886649,
            292.7556511,20.0609063,
            213.3226248,27.08560364,
            166.6219332,34.36407722,
            127.1665473,44.85847176,
            102.4477316,55.77724843,
            90.12020389,63.18367643,
            79.64706759,72.66296594,
            71.85774104,81.57712346,
            66.85394731,87.89807151,
            64.83019531,91.50851857,
            62.85547508,94.3299134,
        )
        return(vanwirdum_data)
    }


#' Plots hydrochemistry: Van Wirdum diagram
#'
#' Create a Van Wirdum diagram for water samples (ionic ratio - log electric
#' conductivity) in ggplot
#'
#' Source: Van Wirdum, Geert (1991). Vegetation and hydrology of floating
#' rich-fens. Datawyse, Maastricht. 316 p. ISBN 90-5291-045-6. (Appendix D)
#'
#' Creates a ggplot object of the ionic ratio (IR) as Y axis against the
#' electric conductivity (EC 25°C) as X axis and allows to plot your own
#' water samples alongside reference data.
#'
#' @section Reference data:
#'
#' #' Reference points (Van Wirdum 1991): benchmark samples for:
#' 1) lithotrophic water LI: a calcium-bicarbonate type of water, usually
#' owing its characteristic composition to a contact with soil;
#' 2) atmotrophic water AT: a type of water with low concentrations of most
#' constituents, usually owing its characteristic composition to atmospheric
#' precipitation;
#' 3) thalassotrophic water TH: a saline sodium-chloride type of water as
#' found in the oceans;
#' 4) molunotrophic water RH: polluted water as presently found in the Rhine.
#'
#' You can also show the mixing contours between the reference points LI, AT
#' and TH as curves or as lines. Most water analyses plot within the area
#' bounded by the (curved) lines LI-AT-TH-LI.
#'
#' @section Input:
#'
#' Input: a dataset with the ionic ration and electric conductivity at 25°C
#'
#' EC at 25°C can be in µS/cm or mS/m and will be shown on a logarithmic scale
#'
#' IR can be without units (0-1) or in %
#'
#' Compute IR as follows:
#' IR = [Ca2+] / ([Ca2+] + [Cl-]) with the Ca and Cl concentrations in meq/l
#' mydata <- mydata %>%
#'   mutate(Ca_meq = (Ca_mg*2)/40.078,
#'          Cl_meq = Cl_mg/35.453) %>%
#'   mutate(ir = Ca_meq/(Ca_meq + Cl_meq)) # ir without units (0-1)
#'
#'   or use the helper function \code{\link{calc_ir}} to calculate IR based on
#'   the Ca and Cl concentrations.
#'
#'
#' @section Typical way of using:
#'
#' Add your data points (and any other information you would like to plot)
#' to the Van Wirdum diagram as you would do for any ggplot.
#'
#' @param ir_unit The units for IR, can be NULL (default, axis 0-1)
#' or "pc" (%, axis 0-100). Choose this parameter according to the unit used
#' in your dataset.
#' @param ec25_unit The units for EC 25°C, can be "micro" (default, µS/cm)
#' or "milli" (mS/m). Choose this parameter according to the unit used
#' in your dataset.
#' @param contour Draw the mixing contours, "segment" (default), "curve" or
#' NULL (do not draw)
#' @param language Which language should be used for the legend, "EN" (English,
#' default) or "NL" (Dutch)
#' @param rhine Should the reference point for Rhine be shown? FALSE (default)
#' or TRUE
#'
#' @return
#' A ggplot object with the Van Wirdum IR-log EC diagram, the reference points
#' for the water types LI-AT-TH and the mixing contours.
#'
#' @examples
#' \dontrun{
#' mydata <-
#'   tibble(
#'     my_ir = runif(10), # without units (0-1)
#'     my_ec25 = rnorm(10, 100, 35),
#'     my_site = append(replicate(4, "site A"), replicate(6, "site B"))
#'   )
#'
#' p <- gg_vanwirdum(contour = "curve",
#'                  rhine = TRUE) +
#'      geom_point(data = mydata,
#'                 aes(x = my_ec25, y = my_ir, colour = my_site), size = 3) +
#'      theme(axis.text  = element_text(colour = "blue"))
#' }

# REMOVE THIS IN DEFINITIVE VERSION ------------------
# mydata <-
#   tibble(
#     my_ir = runif(10),
#     my_ec25 = rnorm(10, 100, 35),
#      my_site = append(replicate(4, "site A"), replicate(6, "site B"))
#     )
#
# library(ggplot2)
# library(scales)
# ir_unit = NULL
# ec25_unit = "micro"
# contour = "curve"
# language = "NL"
# rhine = TRUE
# ----------------------------------------------------

# NEEDED IN DESCRIPTION
# Suggests:
#   ggplot2
#   scales
# LazyData: true (is already ok)

# STILL TO DO: add dataset in R/sysdata.rda (or data/ ?)
#  vw_lat_framework <- read_csv("C:/Users/cecile_herr/Desktop/coordinates_LAT_framework.csv")
# https://r-pkgs.org/data.html
# usethis::use_data(vw_lat_framework, internal = TRUE)
# watina:::vw_lat_framework

# NICE TO HAVE: helper function to convert a dataset from get_chem to a dataset
# readily compatible with this function
watina <- connect_watina()
mydata <-
  get_locs(watina, area_codes = "ZWA") %>%
  get_chem(watina, "1/1/2019") %>%
    collect %>%
    as.data.frame
x <- mydata

calc_ir <- function(x){

    # collect the data if needed
    if ("tbl_sql" %in% class(x)) {
        x <- x %>%
            collect
    }

    x <- x %>%
        filter(chem_variable %in% c("Ca", "Cl", "CondL"))

    # check units and calculate ir
    if ("mg/l" %in% unique(x$unit)) {

        x <- x %>%
            select(-unit, -below_loq, -loq) %>%
            pivot_wider(names_from = chem_variable, values_from = value) %>%
            mutate(Ca_meq = (Ca*2)/40.078,
                   Cl_meq = Cl/35.453,
                   ir = Ca_meq/(Ca_meq + Cl_meq)) # ir without units (0-1), CondL in µS/cm => default in gg_vanwirdum
        } else {

            x <- x %>%
                select(-unit, -below_loq, -loq) %>%
                pivot_wider(names_from = chem_variable, values_from = value) %>%
                mutate(ir = Ca/(Ca + Cl))
            # ir without units (0-1), CondL in µS/cm => default in gg_vanwirdum
            }

}



gg_vanwirdum <- function(ir_unit = NULL,
                         ec25_unit = "micro",
                         contour = "segment",
                         language = "EN",
                         rhine = FALSE) {

  # Check for availability packages ggplot2 and scales
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package \"ggplot2\" needed for this function to work.
         Please install it.",
         call. = FALSE)
  }
    if (!requireNamespace("scales", quietly = TRUE)) {
        stop("Package \"scales\" needed for this function to work.
         Please install it.",
             call. = FALSE)
    }

  # Define reference locations
  ref_points <-
    data.frame(
      location_ref = c("Li (Angeren)", "At (Witteveen)", "Th (Noordwijk)",
                       "Rh (Lobith)"),
      Ca_ref = c(115, 0.4, 420, 82),
      Cl_ref = c(11, 3, 19100, 178),
      ec25_ref = c(651, 50.1, 52000, 996), # in µS/cm
      ir_ref = c(.95, .20, .04, .45), # without units (in 0-1)
      name_ref = c("Li", "At", "Th", "Rh")
    )

  # Use correct units for legend and limits in the plot for EC25 (param ec25_unit)
  if (ec25_unit == "micro") {
    ec25_u <- "(µS/cm)"
    ec25_limits <- c(5,100000)
  } else if (ec25_unit == "milli") {
    ec25_u <- "(mS/m)"
    ec25_limits <- c(0.5,10000)
    ref_points <- ref_points %>%
      mutate(ec25_ref = ec25_ref/10)
  }
  # Use correct units for legend and limits in the plot for IR (param ir_unit)
  if (is.null(ir_unit)) {
    ir_u <- ""
    ir_limits <- c(0,1)
  } else if (ir_unit == "pc") {
    ir_u <- "(%)"
    ir_limits <- c(0,100)
    ref_points <- ref_points %>%
      mutate(ir_ref = ir_ref*100)
  }

  # Legend in appropriate language (param language)
  dict_legend <-
    data.frame(
      lang = c("EN","NL"),
      ir = c("Ionic ratio","Ionenratio"),
      ec25 = c("Electric conductivity 25°C","Elektrische conductiviteit 25°C")
    )

  # Adapt dataset to show or hide the reference point for Rhine (param rhine)
  ref_points_rh <- if (rhine == FALSE) {
    ref_points %>% filter(name_ref != "Rh")
  } else {ref_points}

  # Define custom breaks for EC25 in the ggplot
  # major breaks:
  vw_major_breaks <- function(n = 10){
    function(x) {
      axisTicks(log10(range(x, na.rm = TRUE)), log = TRUE, n = n)
    }
  }
  # minor breaks:
  vw_minor_breaks <- rep(1:9, 21)*(10^rep(-10:10, each = 9))

  # create Van Wirdum diagram in ggplot2

  # add reference points
  p_vanwirdum <- ggplot() +
    geom_point(data = ref_points_rh,
               aes(x = ec25_ref, y = ir_ref),
               size = 2, colour = "black", shape = 15)

  # add mixing contours
  if (!is.null(contour)) {
    if (contour == "segment") {

      # draw geom_segment between the reference points
      ref_join <- ref_points %>%
        filter(name_ref != "Rh") %>%
        mutate(id =  1:nrow(ref_points  %>%
                              filter(name_ref != "Rh") )) %>%
        complete(nesting(ec25_ref,ir_ref), id) %>%
        select(id, xend = ec25_ref, yend = ir_ref) %>%
        left_join(ref_points  %>%
                    filter(name_ref != "Rh") %>%
                    mutate(id = row_number()), ., by = 'id') %>%
        filter(!(ec25_ref == xend & ir_ref == yend))

      p_vanwirdum <- p_vanwirdum +
        geom_segment(data = ref_join %>% filter(name_ref != "Rh"),
                     aes(x = ec25_ref, y = ir_ref, xend = xend, yend = yend),
                     colour = "black")

    } else if (contour == "curve") {

      if (ec25_unit == "micro") {
        vw_lat_framework <- vw_lat_framework %>%
          mutate(ec25 = ec25*10)
      }
      if (is.null(ir_unit)) {
        vw_lat_framework <- vw_lat_framework %>%
          mutate(ir = ir/100)
      }
      # draw curve along the points of the LAT framework
      p_vanwirdum <- p_vanwirdum +
        geom_path(data = vw_lat_framework,
                  aes(x = ec25, y = ir),
                  colour = "black", linetype = "dashed")
    }
  }

  # add labels for the reference points
  p_vanwirdum <- p_vanwirdum +
    geom_text(data = ref_points_rh,
              aes(x = ec25_ref, y = ir_ref, label  = name_ref),
              vjust = -1, colour = "black", size = 5) +
    scale_y_continuous(name = paste(dict_legend %>%
                                      filter(lang == language) %>%
                                      select(ir) %>% pull(),
                                    ir_u),
                       limits = ir_limits) +
    scale_x_log10(name = paste(dict_legend %>%
                                 filter(lang == language) %>%
                                 select(ec25) %>% pull(),
                               ec25_u),
                  breaks = vw_major_breaks(), minor_breaks = vw_minor_breaks,
                  labels = format_format(scientific = FALSE),
                  limits = ec25_limits) +
    theme_bw() +
    theme(axis.title = element_text(size = 14, face = "bold"),
          axis.text  = element_text(size = 13),
          panel.grid.minor = element_line(size = 1),
          panel.grid.major = element_line(size = 1))

}
history()
