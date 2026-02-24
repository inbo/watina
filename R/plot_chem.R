#' Return the 'vanwirdum_data' data source as a tibble
#'
#' Returns the included data source 'vanwirdum_data' as a
#' \code{\link[tibble:tbl_df-class]{tibble}}.
#'
#' Source: Van Wirdum, Geert (1991). Vegetation and hydrology of floating
#' rich-fens. Datawyse, Maastricht. 316 p. ISBN 90-5291-045-6. (Appendix D)
#' \href{https://publicwiki.deltares.nl/display/VWD/Home}{dataset available
#' here}
#'
#' The 'vanwirdum_data' dataset gives the curved contour which encloses the
#' plotting area of all possible, simple mixtures of the following reference
#' water samples:
#'
#' - LI-ANG (a relatively calcium-rich groundwater sample),
#'
#' - AT-WTV (a precipitation sample caught in a relatively unpolluted inland
#' area of The Netherlands)
#'
#' - TH-N70 (a representative analysis from the North Sea
#' monitoring program, 70 km from the coast).
#'
#' The curved contour around these points gives the boundary within which
#' most natural waters can occur.
#'
#' \code{vanwirdum_data} returns this dataset as a
#' \code{\link[tibble:tbl_df-class]{tibble}} with 42 rows and 2 variables.
#' A tibble is a dataframe that makes working in the tidyverse a little
#' \href{https://r4ds.had.co.nz/tibbles.html}{easier}.
#'
#' Variables:
#' \describe{
#'   \item{ec25}{electric conductivity at 25°C in mS/m}
#'   \item{ir}{ionic ratio in \%}}
#'
#' @encoding UTF-8
#'
#' @return
#' The \code{vanwirdum_data} dataframe as a
#' \code{\link[tibble:tbl_df-class]{tibble}}
#'
#' @importFrom dplyr
#' tribble
#'
#' @keywords internal
#'

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


#' Plots hydrochemistry: Van Wirdum diagram
#'
#' Creates the background of a Van Wirdum diagram for water samples
#' in ggplot (ionic ratio - log electric conductivity).
#'
#' Source: Van Wirdum, Geert (1991). Vegetation and hydrology of floating
#' rich-fens. Datawyse, Maastricht. 316 p. ISBN 90-5291-045-6. (Appendix D)
#'
#' Creates a ggplot object of the ionic ratio (IR) as Y axis against the
#' electric conductivity at 25°C (EC25) as X axis and adds reference data.
#'
#' @section Reference data:
#'
#' Reference points (Van Wirdum 1991): benchmark water samples for:
#'
#' \enumerate{
#'  \item{lithotrophic water LI: a calcium-bicarbonate type of water, usually
#' owing its characteristic composition to a contact with soil;}
#'  \item{atmotrophic water AT: a type of water with low concentrations of most
#' constituents, usually owing its characteristic composition to atmospheric
#' precipitation;}
#'  \item{thalassotrophic water TH: a saline sodium-chloride type of water as
#' found in the oceans;}
#'  \item{molunotrophic water RH: polluted water as presently found in the Rhine.}
#'  }
#'
#' You can also show the mixing contours between the reference points LI, AT
#' and TH as curves or as lines. The curved contour encloses the
#' plotting area of all possible, simple mixtures of the reference water
#' samples LI-ANG (a relatively calcium-rich groundwater sample ), AT-WTV
#' (a precipitation sample caught in a relatively unpolluted inland area of
#' The Netherlands) and TH-N70 (a representative analysis from the North Sea
#' monitoring program, 70 km from the coast).
#' Most water analyses plot within the area bounded by the (curved) lines
#' LI-AT-TH-LI.
#'
#' @section Typical way of using:
#'
#' Add your own water samples as data points (and any other information you
#' would like to plot) to the Van Wirdum diagram as you would do for any ggplot.
#'
#' @encoding UTF-8
#'
#' @section Input format:
#'
#' Input: a dataset with the electric conductivity at 25°C and the ionic ratio:
#'
#'  \itemize{
#'  \item{EC at 25°C can be in µS/cm or mS/m and will be shown on a logarithmic scale}
#'  \item{IR can be without units (0-1) or in \%}
#'  }
#'
#' Compute the ionic ratio as follows:
#'
#' ionic ratio = [Ca2+] / ([Ca2+] + [Cl-]) with the Ca and Cl concentrations in meq/l
#'
#' \preformatted{
#' mydata <- mydata \%>\%
#'   mutate(Ca_meq = (Ca_mg*2)/40.078,
#'          Cl_meq = Cl_mg/35.453) \%>\%
#'   mutate(ir = Ca_meq/(Ca_meq + Cl_meq)) # ir without units (0-1)
#' }
#'
#'   or use the helper function \code{\link{calc_ir}} to calculate the ionic
#'   ratio based on the Ca and Cl concentrations obtained through
#'   the \code{\link{get_chem}} function.
#'
#' @param ir_unit The units for the ionic ratio, can be NULL (default, axis 0-1)
#' or "pc" (\%, axis 0-100). Choose this parameter according to the unit used
#' in your dataset.
#' @param ec25_unit The units for the electric conductivity et 25°C:
#' can be "micro" (default, µS/cm) or "milli" (mS/m).
#' Choose this parameter according to the unit used in your dataset.
#' @param contour Draw the mixing contours of the reference water samples,
#' "segment" (default), "curve" or NULL (do not draw).
#' @param lang Which language should be used for the legend, "en" (English,
#' default) or "nl" (Dutch)?
#' @param rhine Should the reference point for Rhine be shown? FALSE (default)
#' or TRUE.
#'
#' @return
#' A ggplot object with the Van Wirdum IR-log EC25 diagram, the reference points
#' for the water types LI-AT-TH and the mixing contours between the reference
#' water samples LI-AT-TH.
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#'
#' # a dataset with water samples
#' mydata <-
#'   tibble(
#'     my_ir = runif(10), # without units (0-1)
#'     my_ec25 = rnorm(10, 100, 35),
#'     my_site = append(replicate(4, "site A"), replicate(6, "site B"))
#'   )
#'
#' # default version of the basis plot without data
#' ggplot_vanwirdum_background(ec25_unit = "micro",
#'              contour = "segment",
#'              lang = "en",
#'              rhine = FALSE)
#'
#' # add your own data with EC as x and IR as y and format as you wish
#' ggplot_vanwirdum_background(contour = "curve",
#'              lang = "nl",
#'              rhine = TRUE) +
#'   geom_point(data = mydata,
#'              aes(x = my_ec25, y = my_ir, colour = my_site), size = 3) +
#'   theme(axis.text  = element_text(colour = "blue"))
#' }
#'
#' @export
#' @importFrom rlang
#' .data
#' @importFrom dplyr
#' %>%
#' filter
#' mutate
#'

ggplot_vanwirdum_background <- function(ir_unit = NULL,
                         ec25_unit = "micro",
                         contour = "segment",
                         lang = "en",
                         rhine = FALSE) {

  # Check for availability packages ggplot2 and scales
    require_pkgs("ggplot2")
    require_pkgs("scales")

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
    ec25_u <- "(\u00b5S/cm)"
    ec25_limits <- c(5,100000)
  } else if (ec25_unit == "milli") {
    ec25_u <- "(mS/m)"
    ec25_limits <- c(0.5,10000)
    ref_points <- ref_points %>%
      mutate(ec25_ref = .data$ec25_ref/10)
  }
  # Use correct units for legend and limits in the plot for IR (param ir_unit)
  if (is.null(ir_unit)) {
    ir_u <- ""
    ir_limits <- c(0,1)
  } else if (ir_unit == "pc") {
    ir_u <- "(%)"
    ir_limits <- c(0,100)
    ref_points <- ref_points %>%
      mutate(ir_ref = .data$ir_ref*100)
  }

  # Legend in appropriate language (param language)
  dict_legend <-
    data.frame(
      language = c("en","nl"),
      ir = c("Ionic ratio","Ionenratio"),
      ec25 = c("Electric conductivity 25\u00b0C","Elektrische conductiviteit 25\u00b0C")
    )

  # Adapt dataset to show or hide the reference point for Rhine (param rhine)
  ref_points_rh <- if (rhine == FALSE) {
    ref_points %>% filter(.data$name_ref != "Rh")
  } else {ref_points}

  # Define custom breaks for EC25 in the ggplot
  # major breaks:
  vw_major_breaks <- function(n = 10){
    function(x) {
        grDevices::axisTicks(log10(range(x, na.rm = TRUE)), log = TRUE, n = n)
    }
  }
  # minor breaks:
  vw_minor_breaks <- rep(1:9, 21)*(10^rep(-10:10, each = 9))

  # create Van Wirdum diagram in ggplot2

  # add reference points
  p_vanwirdum <- ggplot2::ggplot() +
      ggplot2::geom_point(data = ref_points_rh,
                          ggplot2::aes(x = .data$ec25_ref,
                                       y = .data$ir_ref),
                          size = 2, colour = "black", shape = 15)

  # add mixing contours
  if (!is.null(contour)) {
    if (contour == "segment") {

      # draw segment between the 3 reference points LI-AT-TH
      p_vanwirdum <- p_vanwirdum +
          ggplot2::geom_path(data = ref_points %>%
                                 filter(.data$name_ref != "Rh") %>%
                                 rbind(.[1,]), # close the triangle
                                ggplot2::aes(x = .data$ec25_ref,
                                             y = .data$ir_ref),
                                colour = "grey20")

    } else if (contour == "curve") {

        vw_lat_framework <- vanwirdum_data

      if (ec25_unit == "micro") {
        vw_lat_framework <- vw_lat_framework %>%
          mutate(ec25 = .data$ec25*10)
      }
      if (is.null(ir_unit)) {
        vw_lat_framework <- vw_lat_framework %>%
          mutate(ir = .data$ir/100)
      }
      # draw curve along the points of the LAT framework
      p_vanwirdum <- p_vanwirdum +
          ggplot2::geom_path(data = vw_lat_framework,
                             ggplot2::aes(x = .data$ec25,
                                          y = .data$ir),
                             colour = "grey20", linetype = "dashed")
    }
  }

  # add labels for the reference points
  p_vanwirdum <- p_vanwirdum +
      ggplot2::geom_text(data = ref_points_rh,
                         ggplot2::aes(x = .data$ec25_ref,
                                      y = .data$ir_ref,
                                      label  = .data$name_ref),
              vjust = -0.5, hjust = -0.3,
              colour = "grey20", size = 5) +
      ggplot2::scale_y_continuous(name = paste(dict_legend %>%
                                                   filter(.data$language == lang) %>%
                                                   select("ir") %>% pull(),
                                               ir_u),
                                  limits = ir_limits) +
      ggplot2::scale_x_log10(name = paste(dict_legend %>%
                                              filter(.data$language == lang) %>%
                                              select("ec25") %>% pull(),
                                          ec25_u),
                             breaks = vw_major_breaks(),
                             minor_breaks = vw_minor_breaks,
                             labels = scales::label_number(),
                             limits = ec25_limits) +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.title = ggplot2::element_text(size = 14, face = "bold"),
                     axis.text  = ggplot2::element_text(size = 13),
                     panel.grid.minor = ggplot2::element_line(linewidth = 1),
                     panel.grid.major = ggplot2::element_line(linewidth = 1))

  return(p_vanwirdum)

}
