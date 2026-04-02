# Plots hydrochemistry: Van Wirdum diagram

Creates the background of a Van Wirdum diagram for water samples in
ggplot (ionic ratio - log electrical conductivity).

## Usage

``` r
ggplot_vanwirdum_background(
  ir_format = c("decimal", "pct"),
  ec25_unit = c("micro_cm", "milli_m"),
  contour = c("segment", "curve", "none"),
  lang = c("en", "nl"),
  rhine = FALSE
)
```

## Arguments

- ir_format:

  The format for the ionic ratio, can be "decimal" (default, axis 0-1)
  or "pct" (%, axis 0-100). Choose this parameter according to the
  format used in your dataset.

- ec25_unit:

  The units for the electrical conductivity at 25°C: can be "micro_cm"
  (default, µS/cm) or "milli_m" (mS/m). Choose this parameter according
  to the unit used in your dataset.

- contour:

  Draw the mixing contours of the reference water samples, "segment"
  (default), "curve" or "none" (do not draw).

- lang:

  Which language should be used for the legend, "en" (English, default)
  or "nl" (Dutch)?

- rhine:

  Should the reference point for Rhine be shown? FALSE (default) or
  TRUE.

## Value

A ggplot object with the Van Wirdum IR-log EC25 diagram, the reference
points for the water types LI-AT-TH and the mixing contours between the
reference water samples LI-AT-TH.

## Details

Creates a ggplot object of the ionic ratio (IR) as Y axis against the
electrical conductivity at 25°C (EC25) as X axis and adds reference
data.

**Typical way of using:**

Add your own water samples as data points (and any other information you
would like to plot) to the Van Wirdum diagram, for instance:

    ggplot_vanwirdum_background() +
             geom_point(data = my_data,
                        aes(x = my_ec25, y = my_ir))

**Input format for the data to plot:**

Input: a dataset with the electrical conductivity at 25°C and the ionic
ratio as columns:

- EC at 25°C can be in µS/cm or mS/m and will be shown on a logarithmic
  scale

- IR can be without units (0-1) or in %

Compute the ionic ratio as follows:

ionic ratio = \[Ca2+\] / (\[Ca2+\] + \[Cl-\]) with the Ca and Cl
concentrations in meq/l

    mydata <- mydata %>%
      mutate(Ca_meq = (Ca_mg*2)/40.078,
             Cl_meq = Cl_mg/35.453) %>%
      mutate(ir = Ca_meq/(Ca_meq + Cl_meq)) # ir without units (0-1)

or use the helper function
[`calculate_ir`](https://inbo.github.io/watina/reference/calculate_ir.md)
to calculate the ionic ratio based on the Ca and Cl concentrations
obtained through the
[`get_chem`](https://inbo.github.io/watina/reference/get_chem.md)
function.

## Reference data on the Van Wirdum diagram

The reference points are benchmark water samples (defined in Van Wirdum
1991) for:

1.  **lithotrophic water LI**: a calcium-bicarbonate type of water,
    usually owing its characteristic composition to a contact with
    subsoil;

2.  **atmotrophic water AT**: a type of water with low concentrations of
    most constituents, usually owing its characteristic composition to
    atmospheric precipitation;

3.  **thalassotrophic water TH**: a saline sodium-chloride type of water
    as found in the oceans;

4.  **molunotrophic water RH**: polluted water as presently found in the
    Rhine.

You can also show the **mixing contours** between the reference points
LI, AT and TH as curves or as lines. The curved contour encloses the
plotting area of all possible, simple mixtures of the reference water
samples LI-ANG (a relatively calcium-rich groundwater sample ), AT-WTV
(a precipitation sample caught in a relatively unpolluted inland area of
The Netherlands) and TH-N70 (a representative analysis from the North
Sea monitoring program, 70 km from the coast). Most water analyses plot
within the area bounded by the (curved) lines LI-AT-TH-LI.

## References

Van Wirdum, Geert (1991). Vegetation and hydrology of floating
rich-fens. Datawyse, Maastricht. 316 p. ISBN 90-5291-045-6. (Appendix D)

## Examples

``` r
if (FALSE) { # \dontrun{
library(ggplot2)

# a dataset with water samples
my_data <-
  tibble(
    my_ir = runif(10), # without units (0-1)
    my_ec25 = rnorm(10, 100, 35),
    my_site = append(replicate(4, "site A"), replicate(6, "site B"))
  )

# default version of the basis plot without data
ggplot_vanwirdum_background(
  ec25_unit = "micro_cm",
  contour = "segment",
  lang = "en",
  rhine = FALSE
)

# add your own data with EC as x and IR as y and format as you wish
ggplot_vanwirdum_background(
  contour = "curve",
  lang = "nl",
  rhine = TRUE
) +
  geom_point(
    data = my_data,
    aes(x = my_ec25, y = my_ir, colour = my_site), size = 3
  ) +
  theme(axis.text = element_text(colour = "blue"))
} # }
```
