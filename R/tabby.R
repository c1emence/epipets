#' Tabby (Tabulate) from EpiPets
#'
#' Function for tabulating counts of one variable grouped by another in n(\%) form.
#'
#' @param dataframe A data frame.
#' @param var A chosen categorical variable, for example, race.
#' @param group A chosen categorical variable by which to group counts, for example, Treatment Group.
#' @return A dataframe with levels of "var" as rows, and levels of "group" as columns. Cells contain n (\%), where cell percentages are calculated as part of column totals.
#'
#' @examples
#' tabby(mtcars, cyl, gear)
#'
#' @export

# 09/02/2026 by Clemence Fichet

tabby <- function(dataframe, var, group) {
  dataframe |>
    dplyr::count({{var}}, {{group}}) |>
    dplyr::group_by({{group}}) |>
    dplyr::mutate(
      pct = n / sum(n) * 100,
      cell = paste0(n, " (", round(pct, 1), "%)")
    ) |>
    dplyr::ungroup() |>
    dplyr::select({{var}}, {{group}}, cell) |>
    tidyr::pivot_wider(
      names_from = {{group}},
      values_from = cell
    )
}
