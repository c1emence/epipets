#' Cooper from EpiPets
#'
#' A quick function for clean counts and percentages of categorical variables.
#'
#'@param dataframe A data frame.
#'@param var A categorical variable.
#'@return A nicely formatted count of a single variable's levels in n (\%) form.
#'
#'@examples
#'cooper(mtcars, cyl)
#'
#'@export


# Named Cooper for my childhood dog Cooper that passed on 08/29/2026 at the age of 10, when I was 21.
# 09/02/2026 by Clemence A. Fichet

cooper <- function(dataframe, var) {
  dataframe |>
    dplyr::count({{ var }}) |>
    dplyr::mutate(
      pct = n / sum(n) * 100,
      cell = paste0(n, " (", round(pct, 1), "%)")
    ) |>
    dplyr::select({{ var }}, cell)
}
