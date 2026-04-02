# Filter XG3 data per location x hydroyear

Helper function to select the specified XG3 columns per location x
hydroyear.

## Usage

``` r
filter_xg3(data, xg3_type)
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

A `tbl_lazy` object or a tibble, which is like `data` but with
non-requested XG3 variables discarded.
