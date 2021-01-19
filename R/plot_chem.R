#' Plots hydrochemistry
#'
#' Create a Van Wirdum diagram for water samples (ionic ratio - log electric
#' conductivity) in ggplot
#'
#' Source: Van Wirdum, Geert (1991). Vegetation and hydrology of floating
#' rich-fens. Datawyse, Maatsricht. 316 p. ISBN 90-5291-045-6. (Appendix D)
#'
#' Creates a ggplot object of the ionic ratio (IR) as Y axis against the
#' electric conductivity (EC 25°C) as X axis
#'
#' EC at 25°C in µS/cm or mS/m on a logarithmic scale
#'
#' IR = [Ca2+] / ([Ca2+] + [Cl-]) with the Ca and Cl concentrations in meq/l
#' Compute IR as follows:
#' mydata <- mydata %>%
#'   mutate(Ca_meq = (Ca_mg*2)/40.078,
#'          Cl_meq = Cl_mg/35.453) %>%
#'   mutate(ir = Ca_meq/(Ca_meq + Cl_meq)) # ir without units (0-1)
#'
#' Reference points (Van Wirdum 1991): benchmark samples for:
#' 1) lithotrophic water LI: a calcium-bicarbonate type of water, usually
#' owing its characteristic composition to a contact with soil;
#' 2) atmotrophic water AT: a type of water with low concentrations of most
#' constituents, usually owing its characteristic composition to atmospheric
#' precipitation;
#' 3) thalassotrophic water TH: a saline sodium-chloride type of water as
#' found in the oceans;
#' 4) molunotrophic water RH: polluted water as presently found in the Rhine.
#'
#' Mixing contours: most water analyses plot within the area bounded by the
#' curved lines LI-AT-TH-LI
#'
#' Add your data points (and any other information you would like to plot)
#' to the Van Wirdum diagram as you would do for any ggplot.
#'
#' @param ir_unit The units for IR, can be NULL (default, axis 0-1)
#' or "pc" (%, axis 0-100)
#' @param ec25_unit The units for EC 25°C, can be "micro" (default, µS/cm)
#' or "milli" (mS/m)
#' @param contour Draw the mixing contours, "segment" (default), "curve" or
#' NULL (do not draw)
#' @param language Which language should be used for the legend, "EN" (English,
#' default) or "NL" (Dutch)
#' @param rhine Should the reference point for Rhine be shown? FALSE (default)
#' or TRUE
#'
#' @return
#' A ggplot object with the Van Wirdum IR-log EC diagram
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
# ir_unit = NULL
# ec25_unit = "micro"
# contour = "curve"
# language = "NL"
# rhine = TRUE
# ----------------------------------------------------

# NEEDED IN DESCRIPTION
# Suggests:
#   ggplot2
# LazyData: true (is already ok)

# STILL TO DO: add dataset in R/sysdata.rda (or data/ ?)
#  vw_lat_framework <- read_csv("C:/Users/cecile_herr/Desktop/coordinates_LAT_framework.csv")
# https://r-pkgs.org/data.html
# usethis::use_data(vw_lat_framework, internal = TRUE)
# watina:::vw_lat_framework

# NICE TO HAVE: helper function to convert a dataset from get_chem to a dataset
# readily compatible with this function
# mydata <-
#   get_locs(watina, area_codes = "ZWA") %>%
#   get_chem(watina, "1/1/2010")
#
# mydata_vw <- mydata %>%
#   filter(chem_variable %in% c("Ca", "Cl", "CondL")) %>%
#   select(-unit, -below_loq, -loq) %>%
#   pivot_wider(names_from = chem_variable, values_from = value) %>%
#   mutate(Ca_meq = (Ca*2)/40.078,
#          Cl_meq = Cl/35.453,
#          ir = Ca_meq/(Ca_meq + Cl_meq))
# ir without units (0-1), CondL in µS/cm

gg_vanwirdum <- function(ir_unit = NULL,
                         ec25_unit = "micro",
                         contour = "segment",
                         language = "EN",
                         rhine = FALSE) {

  # Check for availability package ggplot2
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package \"ggplot2\" needed for this function to work.
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
