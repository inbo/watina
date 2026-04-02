# Qualify XG3 data per location x hydroyear

Helper function to determine the availability of the specified XG3 types
per location x hydroyear.

## Usage

``` r
qualify_xg3(data, xg3_type)
```

## Arguments

- data:

  An object returned by
  [`get_xg3`](https://inbo.github.io/watina/reference/get_xg3.md).

- xg3_type:

  Character vector of length 1, 2 or 3. Defines the types of XG3 which
  are taken from `data`. Specifies the 'X' in 'XG3': either `"L"`, `"H"`
  and/or `"V"`. Defaults to `"L"`.

## Value

A tibble, with columns `xg3_variable` (character; see Details) and
`available` (logical) to denote at each location whether the hydroyear
has the requested XG3 values available.

## Details

The column `xg3_variable` in the resulting tibble lists the requested
XG3 types, for each location and hydroyear. If more than one XG3 type is
requested, or if `data` contains both vertical CRSes, an extra value
"`combined`" is listed in the `xg3_variable` column. It evaluates the
combined presence of all selected XG3 variables at each location and
hydroyear.
