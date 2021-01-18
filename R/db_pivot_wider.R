# NOTE: all credits for this db_pivot_wider function go to Edgar Ruiz.
# He made a PR where this function was proposed to incorporate in dbplyr:
# https://github.com/tidyverse/dbplyr/pull/344
# The code below is a copy and the terms of its original code source apply.
# Minor modifications were made in the approach to import rlang functions and to
# avoid global variables.

#' @importFrom rlang
#' .data
#' sym
#' syms
#' set_names
#' flatten
#' enquo
#' enquos
#' quo_get_expr
#' expr
#' @keywords internal
db_pivot_wider <- function(data,
                           id_cols = NULL,
                           names_from = .data$name,
                           names_prefix = "",
                           names_sep = NULL,
                           names_repair = NULL,
                           values_from = .data$value,
                           values_fill = NULL,
                           values_fn = NULL,
                           spec = NULL) {
    if (!requireNamespace("tidyselect", quietly = TRUE)) {
        stop("Package \"tidyselect\" is needed when using this function. ",
             "Please install it.",
             call. = FALSE)
    }
    if (!requireNamespace("purrr", quietly = TRUE)) {
        stop("Package \"purrr\" is needed when using this function. ",
             "Please install it.",
             call. = FALSE)
    }
    check_null_pivot_args(
        id_cols = !!id_cols, names_sep = !!names_sep,
        names_repair = !!names_repair, values_fill = !!values_fill,
        values_fn = !!values_fn, spec = !!spec
    )
    cn <- colnames(data)
    names_from <- tidyselect::vars_select(cn, !!enquo(names_from))
    values_from <- tidyselect::vars_select(cn, !!enquo(values_from))
    pl <- c(values_from, names_from)
    kp <- cn[!(cn %in% pl)]
    headers <- pull(summarise(group_by(data, !!sym(names_from))))
    mt <- purrr::map(
        headers,
        ~ {
            header <- .x
            purrr::map(
                values_from,
                ~ expr(max(ifelse(!!sym(names_from) == !!header, !!sym(.x), NA), na.rm = TRUE))
            )
        }
    )
    fmt <- flatten(mt)
    if (length(values_from) > 1) {
        vp <- paste0(values_from, "_")
    } else {
        vp <- ""
    }
    hn <- purrr::map(headers, ~ paste0(vp, names_prefix, .x))
    rhn <- purrr::reduce(hn, c)
    nmt <- set_names(fmt, rhn)
    grps <- group_by(data, !!!syms(kp))
    summarise(grps, !!!nmt)
}

check_null_pivot_args <- function(..., msg = "The `{arg}` argument is not supported for remote back-ends") {
    vars <- enquos(...)
    purrr::imap(
        vars,
        ~ assert_that(
            is.null(quo_get_expr(.x)),
            msg = sub("\\{arg\\}", .y, msg)
        )
    )
}
