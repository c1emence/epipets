#Playspace


# Make a Dataset
set.seed(0905)

n <- 100

store <- data.frame(
  id = 1:n,
  produce_monday = sample(c("apple", "banana", "kiwi", "brocolli", "potato", "lettuce"), n, replace = TRUE),
  produce_tuesday = sample(c("apple", "banana", "kiwi", "brocolli", "potato", "lettuce"), n, replace = TRUE),
  produce_wednesday = sample(c("apple", "banana", "kiwi", "brocolli", "potato", "lettuce"), n, replace = TRUE)
)

head(store)

# Define a group

fruits <- c("apple", "banana", "kiwi")
veggies <- c("brocolli", "potato", "lettuce")

shopping_list <- list(
  fruits = fruits,
  veggies = veggies
  )

# Use normal functions

sum(store $ produce_monday %in% fruits)


store |>
  dplyr::mutate(
    hit = dplyr::if_any(
      dplyr::starts_with("produce_"),
      ~ .x %in% fruits
    )
  )
store |>
  dplyr::summarise(
    n = sum(
      dplyr::if_any(
        dplyr::starts_with("produce_"),
        ~ .x %in% fruits
      )
    )
  )


# Build the Function

lookie <- function(dataframe, var, group) {

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
                  function(x) x %in% codes
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

lookie(store, produce, shopping_list)

