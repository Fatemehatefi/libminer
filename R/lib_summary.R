#' R Library Summary
#'
#' Provides a brief summary of the package libraries on your machine
#'
#' @param sizes Should sizes of libraries be calculated. Default `FALSE`.
#' @param by
#'
#' @return A data.frame containing the count of packages in each of the user's
#'   libraries
#' @export
#'
#' @examples
#' lib_summary()
lib_summary <- function(by = .data$LibPath, sizes = FALSE) {
  if (!is.logical(sizes)) {
    stop("'sizes' must be logical")
  }

  pkg_df <- lib() |>
    calculate_sizes(do_calc = sizes)

  pkg_df |>
    dplyr::group_by({{ by }}) |>
    dplyr::summarise(
      n = dplyr::n(),
      dplyr::across(dplyr::any_of("size"), .fns = sum, .names = "size")
    )
}
