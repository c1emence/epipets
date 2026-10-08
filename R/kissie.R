#'Kissie From EpiPets
#'
#'Takes tabby() and loggy() and makes them kiss! Helpful for a manucript's table 2 with matching logistic regression output to tabulated n(\%) cell values. Can be used on its own but is internally built to be seamlessly compatible with flextable() functions.
#'
#'
#'@param dataframe A dataframe.
#'@param outcome A Binary Outcome Variable
#'@param exposure A Categorical Exposure Variable
#'@return A table with n x 2 cells with odds ratios and p-values matched to their corresponding rows. Ensure reference levels are properly designated before using.
#'
#'@examples
#'
#'data(mtcars)
#'mtcars$am <- factor(mtcars$am)
#'mtcars$cyl <- factor(mtcars$cyl)
#'kissie(mtcars, am, cyl)
#'
#'@export

# 10/07/2026 by Clemence A. Fichet


kissie <- function(dataframe, outcome, exposure) {

  # Capture exposure name and identify reference level
  exposure_name <- rlang::as_name(rlang::ensym(exposure))
  ref <- levels(dataframe[[exposure_name]])[1]

  # Descriptive counts
  counts <- dataframe |>
    tabby({{ exposure }}, {{ outcome }}) |>
    dplyr::rename(Category = {{ exposure }})

  # Identify outcome columns produced by tabby()
  outcome_cols <- setdiff(names(counts), "Category")

  # Logistic regression
  regression <- dataframe |>
    loggy({{ outcome }}, {{ exposure }}) |>
    dplyr::filter(Variable != "(Intercept)") |>
    dplyr::mutate(
      Category = stringr::str_remove(Variable, stringr::fixed(exposure_name))
    ) |>
    dplyr::select(
      Category,
      `OR (95% CI)`,
      `p-value`
    )

  # Combine and format
  counts |>
    dplyr::left_join(regression, by = "Category") |>
    dplyr::mutate(
      dplyr::across(
        tidyselect::all_of(outcome_cols),
        ~ stringr::str_remove(.x, "%")
      ),

      `OR (95% CI)` = dplyr::case_when(
        Category == ref ~ "Ref.",
        is.na(`OR (95% CI)`) ~ "*",
        TRUE ~ `OR (95% CI)`
      ),

      `p-value` = dplyr::case_when(
        Category == ref ~ "Ref.",
        is.na(`p-value`) ~ "*",
        TRUE ~ `p-value`
      )
    ) |>
    dplyr::rename(
      `P-Value` = `p-value`
    )
}
