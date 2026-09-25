#' Lookie (Lookup and Summarize)
#'
#' A useful tool to search for values or groups of values across one column or multiple columns. I built this for initial analysis with multiple columns of diagnosis codes, but it has various applications.
#'
#' @param dataframe A data frame.
#' @param var An unquoted column-name (variable) prefix used to identify columns to search.
#' @param group A named list of vectors containing values to search for.
#' @param match A matching argument that defaults to exact term matching. Specify match = "prefix" to use prefix-based matching of values. When using exact matching, a search for A15 will return false for a value A150 or A156. When using prefix matching, searching for A15 will return true for a value A15, A159, A1555, and so on.
#'
#' @return A tibble containing each group and its count and percentage formatted as n (\%), followed by a total row. Please pay attention to the output inherent to the structure of this function. If you utilize multiple values across multiple columns, percent values in the output will not sum to 100\%.
#'
#'@examples
#'# Aquire a Dataset with lots of repeating categorical values across multiple columns.
#'store <- data.frame(
#'id = 1:100,
#'produce_mon = sample(c("apple", "banana", "kiwi", "brocolli", "potato", "lettuce"),
#'100, replace = TRUE),
#'produce_tue = sample(c("apple", "banana", "kiwi", "brocolli", "potato", "lettuce"),
#'100, replace = TRUE),
#'produce_wed = sample(c("apple", "banana", "kiwi", "brocolli", "potato", "lettuce"),
#'100, replace = TRUE)
#')
#'# Assign desired groups to count by.
#'fruits <- c("apple", "banana", "kiwi")
#'veggies <- c("brocolli", "potato", "lettuce")
#'
#'# Bring your groups into one list for lookie() to use.
#'shopping_list <- list(
#'  fruits = fruits,
#'  veggies = veggies
#')
#'
#'lookie(store, produce, shopping_list)
#'
#' @export

lookie <- function(dataframe, var, group,
                   match = c("exact", "prefix")) {

  match <- match.arg(match)

  pattern <- rlang::as_name(rlang::ensym(var))
  # so we don't have to put quotes around var when calling it

  total_n <- nrow(dataframe)

  results <- purrr::imap_dfr(
    group,
    function(codes, group_name) {

      n <- dataframe |>
        dplyr::summarise(
          n = sum(
            dplyr::if_any(
              dplyr::starts_with(pattern),
              function(x) {
                if (match == "exact") {
                  x %in% codes
                } else if (match == "prefix") {
                  stringr::str_detect(
                    x,
                    paste0("^(", paste(codes, collapse = "|"), ")")
            )
            # take the dataframe, then:
            #   spit out the following:
            #     summarize:
            #       across all the columns that start with "pattern",
            #           check for values that match current set of "codes",
            #               if ANY selected column does, mark it true
            #     sum all the True rows
            #
            # Result:
            # sum of how many rows have at least one value in "group" across
            # any columns in the "dataframe" that match the "pattern"
                }
              }
            )
          )
        ) |>
        dplyr::pull(n)

      tibble::tibble(
        group = group_name |>
          stringr::str_replace_all("_", " ") |>
          stringr::str_to_title(),
        n = n,
        pct = n / total_n * 100,
        cell = paste0(n, " (", round(pct, 1), "%)")
      )
    }
  )

  dplyr::bind_rows(
    results,
    tibble::tibble(
      group = "Total",
      n = total_n,
      pct = 100,
      cell = paste0(total_n, " (100.0%)")
    )
  ) |>
    dplyr::select(group, cell)
}
